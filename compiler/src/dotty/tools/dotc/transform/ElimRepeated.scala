package dotty.tools.dotc
package transform

import core._
import StdNames.nme
import Types._
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import Flags._
import Contexts._
import Symbols._
import Constants._
import Decorators._
import Denotations._, SymDenotations._
import dotty.tools.dotc.ast.tpd
import TypeErasure.erasure
import DenotTransformers._

object ElimRepeated {
  val name: String = "elimRepeated"
}

/** A transformer that eliminates repeated parameters (T*) from all types, replacing
 *  them with Seq or Array types and adapting repeated arguments to conform to
 *  the transformed type if needed.
 */
class ElimRepeated extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = ElimRepeated.name

  override def changesMembers: Boolean = true // the phase adds vararg forwarders

  def transformInfo(tp: Type, sym: Symbol)(using Context): Type =
    elimRepeated(tp)

  override def transform(ref: SingleDenotation)(using Context): SingleDenotation =
    super.transform(ref) match
      case ref1: SymDenotation if (ref1 ne ref) && overridesJava(ref1.symbol) =>
        // This method won't override the corresponding Java method at the end of this phase,
        // only the forwarder added by `addVarArgsForwarder` will.
        ref1.copySymDenotation(initFlags = ref1.flags &~ Override)
      case ref1 =>
        ref1

  override def mayChange(sym: Symbol)(using Context): Boolean = sym.is(Method)

  private def overridesJava(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(_.is(JavaDefined))

  private def hasVarargsAnnotation(sym: Symbol)(using Context) = sym.hasAnnotation(defn.VarargsAnnot)

  private def parentHasAnnotation(sym: Symbol)(using Context) = sym.allOverriddenSymbols.exists(hasVarargsAnnotation)

  /** Eliminate repeated parameters from method types. */
  private def elimRepeated(tp: Type)(using Context): Type = tp.stripTypeVar match
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      val resultType1 = elimRepeated(resultType)
      val paramTypes1 = paramTypes match
        case init :+ last if last.isRepeatedParam =>
          val isJava = tp.isJavaMethod
          // A generic unbounded Java varargs `T...` is erased to `Object[]` in
          // bytecode, instead of leaving type parameter elimination to Erasure,
          // we directly translate such a type to `Array[Object]` instead of
          // `Array[T]` here. This allows the tree transformer of this phase
          // to emit the correct adaptation for repeated arguments (cf `adaptToArray`).
          val last1 =
            if isJava && {
              val elemTp = last.elemType
              elemTp.isInstanceOf[TypeParamRef] && !elemTp.derivesFrom(defn.ObjectClass)
            }
            then defn.ArrayOf(defn.ObjectType)
            else last.translateFromRepeated(toArray = isJava)
          init :+ last1
        case _ =>
          paramTypes
      tp.derivedLambdaType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, elimRepeated(tp.resultType))
    case tp =>
      tp

  def transformTypeOfTree(tree: Tree)(using Context): Tree =
    tree.withType(elimRepeated(tree.tpe))

  override def transformTypeApply(tree: TypeApply)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformIdent(tree: Ident)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformSelect(tree: Select)(using Context): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(using Context): Tree =
    val args = tree.args.mapConserve {
      case arg: Typed if isWildcardStarArg(arg) =>
        val isJavaDefined = tree.fun.symbol.is(JavaDefined)
        val tpe = arg.expr.tpe
        if isJavaDefined then
          val pt = tree.fun.tpe.widen.firstParamTypes.last
          adaptToArray(arg.expr, pt.elemType.bounds.hi)
        else if tpe.derivesFrom(defn.ArrayClass) then
          arrayToSeq(arg.expr)
        else
          arg.expr
      case arg => arg
    }
    transformTypeOfTree(cpy.Apply(tree)(tree.fun, args))

  /** Convert sequence argument to Java array */
  private def seqToArray(tree: Tree)(using Context): Tree = tree match
    case SeqLiteral(elems, elemtpt) =>
      JavaSeqLiteral(elems, elemtpt)
    case _ =>
      val elemType = tree.tpe.elemType
      var elemClass = erasure(elemType).classSymbol
      if defn.NotRuntimeClasses.contains(elemClass) then
        elemClass = defn.ObjectClass
      end if
      ref(defn.DottyArraysModule)
        .select(nme.seqToArray)
        .appliedToType(elemType)
        .appliedTo(tree, clsOf(elemClass.typeRef))

  /** Adapt a Seq or Array tree to be a subtype of `Array[_ <: $elemPt]`.
   *
   *  @pre `elemPt` must either be a super type of the argument element type or `Object`.
   *        The special handling of `Object` is required to deal with the translation
   *        of generic Java varargs in `elimRepeated`.
   */
  private def adaptToArray(tree: Tree, elemPt: Type)(implicit ctx: Context): Tree =
    val elemTp = tree.tpe.elemType
    val treeIsArray = tree.tpe.derivesFrom(defn.ArrayClass)
    if elemTp <:< elemPt then
      if treeIsArray then
        tree // no adaptation needed
      else
        tree match
          case SeqLiteral(elems, elemtpt) =>
            JavaSeqLiteral(elems, elemtpt).withSpan(tree.span)
          case _ =>
            // Convert a Seq[T] to an Array[$elemPt]
            ref(defn.DottyArraysModule)
              .select(nme.seqToArray)
              .appliedToType(elemPt)
              .appliedTo(tree, clsOf(elemPt))
    else if treeIsArray then
      // Convert an Array[T] to an Array[Object]
      ref(defn.ScalaRuntime_toObjectArray)
        .appliedTo(tree)
    else
      // Convert a Seq[T] to an Array[Object]
      ref(defn.ScalaRuntime_toArray)
        .appliedToType(elemTp)
        .appliedTo(tree)

  /** Convert an Array into a scala.Seq */
  private def arrayToSeq(tree: Tree)(using Context): Tree =
    tpd.wrapArray(tree, tree.tpe.elemType)

  /** If method overrides a Java varargs method or is annotated with @varargs, add a varargs bridge.
   *  Also transform trees inside method annotation.
   */
  override def transformDefDef(tree: DefDef)(using Context): Tree =
    val sym = tree.symbol
    val hasAnnotation = hasVarargsAnnotation(sym)

    // atPhase(thisPhase) is used where necessary to see the repeated
    // parameters before their elimination
    val hasRepeatedParam = atPhase(thisPhase)(hasRepeatedParams(sym))
    if hasRepeatedParam then
      val isOverride = atPhase(thisPhase)(overridesJava(sym))
      if isOverride || hasAnnotation || parentHasAnnotation(sym) then
        // java varargs are more restrictive than scala's
        // see https://github.com/scala/bug/issues/11714
        val validJava = atPhase(thisPhase)(isValidJavaVarArgs(sym.info))
        if !validJava then
          report.error("""To generate java-compatible varargs:
                    |  - there must be a single repeated parameter
                    |  - it must be the last argument in the last parameter list
                    |""".stripMargin,
            sym.sourcePos)
          tree
        else
          // non-overrides cannot be synthetic otherwise javac refuses to call them
          addVarArgsForwarder(tree, isBridge = isOverride)
      else
        tree
    else
      if hasAnnotation then
        report.error("A method without repeated parameters cannot be annotated with @varargs", sym.sourcePos)
      tree

  /** Is there a repeated parameter in some parameter list? */
  private def hasRepeatedParams(sym: Symbol)(using Context): Boolean =
    sym.info.paramInfoss.flatten.exists(_.isRepeatedParam)

  /** Is this the type of a method that has a repeated parameter type as
   *  its last parameter in the last parameter list?
   */
  private def isValidJavaVarArgs(tp: Type)(using Context): Boolean = tp match
    case mt: MethodType =>
      val initp :+ lastp = mt.paramInfoss
      initp.forall(_.forall(!_.isRepeatedParam)) &&
      lastp.nonEmpty &&
      lastp.init.forall(!_.isRepeatedParam) &&
      lastp.last.isRepeatedParam
    case pt: PolyType =>
      isValidJavaVarArgs(pt.resultType)
    case _ =>
      throw new Exception("Match error in @varargs checks. This should not happen, please open an issue " + tp)


  /** Add a Java varargs forwarder
   *  @param ddef     the original method definition
   *  @param isBridge true if we are generating a "bridge" (synthetic override forwarder)
   *
   *  @return a thicket consisting of `ddef` and an additional method
   *          that forwards java varargs to `ddef`. It retains all the
   *          flags of `ddef` except `Private`.
   *
   *  A forwarder is necessary because the following hold:
   *    - the varargs in `ddef` will change from `RepeatedParam[T]` to `Seq[T]` after this phase
   *    - _but_ the callers of `ddef` expect its varargs to be changed to `Array[? <: T]`
   *  The solution is to add a method that converts its argument from `Array[? <: T]` to `Seq[T]` and
   *  forwards it to `ddef`.
   */
  private def addVarArgsForwarder(ddef: DefDef, isBridge: Boolean)(using Context): Tree =
    val original = ddef.symbol

    // The java-compatible forwarder symbol
    val sym = atPhase(thisPhase) {
      // Capture the flags before they get modified by #transform.
      // For simplicity we always set the varargs flag,
      // although it's not strictly necessary for overrides.
      val flags = original.flags | JavaVarargs
      original.copy(
        flags = if isBridge then flags | Artifact else flags,
        info = toJavaVarArgs(ddef.symbol.info)
      ).asTerm
    }

    // Find a method that would conflict with the forwarder if the latter existed.
    // This needs to be done at thisPhase so that parent @varargs don't conflict.
    val conflict = atPhase(thisPhase) {
      currentClass.info.member(sym.name).alternatives.find { s =>
        s.matches(sym) &&
        !(isBridge && s.asSymDenotation.is(JavaDefined))
      }
    }

    conflict match
      case Some(conflict) =>
        report.error(s"@varargs produces a forwarder method that conflicts with ${conflict.showDcl}", original.sourcePos)
        ddef
      case None =>
        val bridgeDef = polyDefDef(sym.enteredAfter(thisPhase), trefs => vrefss => {
          val init :+ (last :+ vararg) = vrefss
          // Can't call `.argTypes` here because the underlying array type is of the
          // form `Array[? <: SomeType]`, so we need `.argInfos` to get the `TypeBounds`.
          val elemtp = vararg.tpe.widen.argInfos.head
          ref(original.termRef)
            .appliedToTypes(trefs)
            .appliedToArgss(init)
            .appliedToArgs(last :+ tpd.wrapArray(vararg, elemtp))
          })
        Thicket(ddef, bridgeDef)

  /** Convert type from Scala to Java varargs method */
  private def toJavaVarArgs(tp: Type)(using Context): Type = tp match
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      tp.resultType match
        case m: MethodType => // multiple param lists
          tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(m))
        case _ =>
          val init :+ last = tp.paramInfos
          val vararg = varargArrayType(last)
          tp.derivedLambdaType(tp.paramNames, init :+ vararg, tp.resultType)

  /** Translate a repeated type T* to an `Array[? <: Upper]`
   *  such that it is compatible with java varargs.
   *
   *  When necessary we set `Upper = T & AnyRef`
   *  to prevent the erasure of `Array[? <: Upper]` to Object,
   *  which would break the varargs from Java.
   */
  private def varargArrayType(tp: Type)(using Context): Type =
    val array = tp.translateFromRepeated(toArray = true) // Array[? <: T]
    val element = array.elemType.hiBound // T

    if element <:< defn.AnyRefType || element.typeSymbol.isPrimitiveValueClass then array
    else defn.ArrayOf(TypeBounds.upper(AndType(element, defn.AnyRefType))) // Array[? <: T & AnyRef]
}
