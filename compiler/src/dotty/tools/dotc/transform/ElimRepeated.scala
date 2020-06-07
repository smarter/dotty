package dotty.tools.dotc
package transform

import core._
import StdNames.nme
import Types._
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import Flags._
import Contexts.Context
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

/** A transformer that removes repeated parameters (T*) from all types, replacing
 *  them with Seq types.
 */
class ElimRepeated extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = ElimRepeated.name

  override def changesMembers: Boolean = true // the phase adds vararg bridges

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type =
    elimRepeated(tp)

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation =
    super.transform(ref) match {
      case ref1: SymDenotation if (ref1 ne ref) && overridesJava(ref1.symbol) =>
        // This method won't override the corresponding Java method at the end of this phase,
        // only the bridge added by `addVarArgsBridge` will.
        ref1.copySymDenotation(initFlags = ref1.flags &~ Override)
      case ref1 =>
        ref1
    }

  override def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym.is(Method)

  private def overridesJava(sym: Symbol)(implicit ctx: Context) = sym.allOverriddenSymbols.exists(_.is(JavaDefined))

  private def elimRepeated(tp: Type)(implicit ctx: Context): Type = tp.stripTypeVar match {
    case tp @ MethodTpe(paramNames, paramTypes, resultType) =>
      val resultType1 = elimRepeated(resultType)
      val paramTypes1 =
        if (paramTypes.nonEmpty && paramTypes.last.isRepeatedParam) {
          val translated = paramTypes.last.translateFromRepeated(toArray = tp.isJavaMethod)
          val ret =
            translated match {
              case defn.ArrayOf(tp) if tp.typeSymbol eq defn.FromJavaObjectSymbol =>
                defn.ArrayOf(TypeBounds.upper(defn.ObjectType))
              case tp =>
                tp
            }
          paramTypes.init :+ ret
        }
        else paramTypes
      tp.derivedLambdaType(paramNames, paramTypes1, resultType1)
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, elimRepeated(tp.resultType))
    case tp =>
      tp
  }

  def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(elimRepeated(tree.tpe))

  override def transformIdent(tree: Ident)(implicit ctx: Context): Tree =
    transformTypeOfTree(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context): Tree =
    transformTypeOfTree(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context): Tree =
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

  /** Adapt a Seq or Array tree to be a subtype of `Array[_ <:< $elemPt]`
   *
   *  @pre `elemPt` must either be a super type of the argument element type or `Object`.
   *       The special handling of `Object` is required to deal with the translation
   *       of `FromJavaObject` in `elimRepeated`.
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
  private def arrayToSeq(tree: Tree)(implicit ctx: Context): Tree =
    tpd.wrapArray(tree, tree.tpe.elemType)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context): Tree =
    transformTypeOfTree(tree)

  /** If method overrides a Java varargs method, add a varargs bridge.
   *  Also transform trees inside method annotation
   */
  override def transformDefDef(tree: DefDef)(implicit ctx: Context): Tree =
    ctx.atPhase(thisPhase) {
      if (tree.symbol.info.isVarArgsMethod && overridesJava(tree.symbol))
        addVarArgsBridge(tree)
      else
        tree
    }

  /** Add a Java varargs bridge
   *  @param  ddef  the original method definition which is assumed to override
   *                a Java varargs method JM up to this phase.
   *  @return  a thicket consisting of `ddef` and a varargs bridge method
   *           which overrides the Java varargs method JM from this phase on
   *           and forwards to `ddef`.
   *
   *  A bridge is necessary because the following hold
   *    - the varargs in `ddef` will change from `RepeatedParam[T]` to `Seq[T]` after this phase
   *    - _but_ the callers of `ddef` expect its varargs to be changed to `Array[? <: T]`, since it overrides
   *      a Java varargs
   *  The solution is to add a "bridge" method that converts its argument from `Array[? <: T]` to `Seq[T]` and
   *  forwards it to `ddef`.
   */
  private def addVarArgsBridge(ddef: DefDef)(implicit ctx: Context): Tree = {
    val original = ddef.symbol.asTerm
    val bridge = original.copy(
      flags = ddef.symbol.flags &~ Private | Artifact,
      info = toJavaVarArgs(ddef.symbol.info)).enteredAfter(thisPhase).asTerm
    val bridgeDef = polyDefDef(bridge, trefs => vrefss => {
      val (vrefs :+ varArgRef) :: vrefss1 = vrefss
      // Can't call `.argTypes` here because the underlying array type is of the
      // form `Array[? <: SomeType]`, so we need `.argInfos` to get the `TypeBounds`.
      val elemtp = varArgRef.tpe.widen.argInfos.head
      ref(original.termRef)
        .appliedToTypes(trefs)
        .appliedToArgs(vrefs :+ tpd.wrapArray(varArgRef, elemtp))
        .appliedToArgss(vrefss1)
    })

    Thicket(ddef, bridgeDef)
  }

  /** Convert type from Scala to Java varargs method */
  private def toJavaVarArgs(tp: Type)(implicit ctx: Context): Type = tp match {
    case tp: PolyType =>
      tp.derivedLambdaType(tp.paramNames, tp.paramInfos, toJavaVarArgs(tp.resultType))
    case tp: MethodType =>
      val inits :+ last = tp.paramInfos
      val last1 = last.translateFromRepeated(toArray = true)
      tp.derivedLambdaType(tp.paramNames, inits :+ last1, tp.resultType)
  }
}
