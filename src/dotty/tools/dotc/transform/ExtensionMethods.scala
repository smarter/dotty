/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms._
import TreeExtractors._, ValueClasses._
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import mutable.ListBuffer
import core._
import Phases.Phase
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import TypeErasure.{ erasure, ErasedValueType }
import TypeUtils._
import util.Positions._
import Decorators._
import SymUtils._

/**
 * Perform Step 1 and 2 in the value classes SIP: Creates extension methods for all
 * methods in a value class, except parameter or super accessors, or constructors.
 */
class ExtensionMethods extends MiniPhaseTransform with DenotTransformer with FullParameterization { thisTransformer =>

  import tpd._

  /** the following two members override abstract members in Transform */
  override def phaseName: String = "extmethods"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[ElimRepeated])

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case ref: ClassDenotation if ref is ModuleClass =>
      ref.linkedClass match {
        case origClass: ClassSymbol if isDerivedValueClass(origClass) =>
          val cinfo = ref.classInfo
          val decls1 = cinfo.decls.cloneScope
          ctx.atPhase(thisTransformer.next) { implicit ctx =>
            // In Scala 2, extension methods are added before pickling so we should
            // not generate them again.
            if (!(origClass is Scala2x)) {
              for (decl <- origClass.classInfo.decls) {
                if (isMethodWithExtension(decl))
                  decls1.enter(createExtensionMethod(decl, ref.symbol))
              }
            }

            // For a value class V, let U be the underlying type after erasure. We add
            // to the companion object of V two cast methods:
            //   def underlying2evt$(x0: U): ErasedValueType(V, U)
            //   def evt2underlying$(x0: V): ErasedValueType(U, V)
            // The casts are used in Erasure to make it typecheck, they are then removed
            // in ElimErasedValueType.
            // This is different from the implementation of value classes in Scala 2
            // (see SIP-15) which used `asInstanceOf` which does not typecheck.
            val sym = ref.symbol
            val underlying = erasure(underlyingOfValueClass(origClass))
            val evt = ErasedValueType(origClass, underlying)
            val underlying2evtSym = ctx.newSymbol(sym, nme.UNDERLYING2EVT, Method,
              MethodType(List(nme.x_0), List(underlying), evt))
            val evt2underlyingSym = ctx.newSymbol(sym, nme.EVT2UNDERLYING, Method,
              MethodType(List(nme.x_0), List(evt), underlying))
            decls1.enter(underlying2evtSym)
            decls1.enter(evt2underlyingSym)
          }
          if (decls1.isEmpty) ref
          else ref.copySymDenotation(info = cinfo.derivedClassInfo(decls = decls1))
        case _ =>
          ref
      }
    case ref: SymDenotation
    if isMethodWithExtension(ref) && ref.hasAnnotation(defn.TailrecAnnotationClass) =>
      val ref1 = ref.copySymDenotation()
      ref1.removeAnnotation(defn.TailrecAnnotationClass)
      ref1
    case _ =>
      ref
  }

  protected def rewiredTarget(target: Symbol, derived: Symbol)(implicit ctx: Context): Symbol =
    if (isMethodWithExtension(target) &&
        target.owner.linkedClass == derived.owner) extensionMethod(target)
    else NoSymbol

  /** Generate stream of possible names for the extension version of given instance method `imeth`.
   *  If the method is not overloaded, this stream consists of just "imeth$extension".
   *  If the method is overloaded, the stream has as first element "imeth$extenionX", where X is the
   *  index of imeth in the sequence of overloaded alternatives with the same name. This choice will
   *  always be picked as the name of the generated extension method.
   *  After this first choice, all other possible indices in the range of 0 until the number
   *  of overloaded alternatives are returned. The secondary choices are used to find a matching method
   *  in `extensionMethod` if the first name has the wrong type. We thereby gain a level of insensitivity
   *  of how overloaded types are ordered between phases and picklings.
   */
  private def extensionNames(imeth: Symbol)(implicit ctx: Context): Stream[Name] = {
    val decl = imeth.owner.info.decl(imeth.name)

    /** No longer needed for Dotty, as we are more disciplined with scopes now.
    // Bridge generation is done at phase `erasure`, but new scopes are only generated
    // for the phase after that. So bridges are visible in earlier phases.
    //
    // `info.member(imeth.name)` filters these out, but we need to use `decl`
    // to restrict ourselves to members defined in the current class, so we
    // must do the filtering here.
    val declTypeNoBridge = decl.filter(sym => !sym.isBridge).tpe
    */
    decl match {
      case decl: MultiDenotation =>
        val alts = decl.alternatives
        val index = alts indexOf imeth.denot
        assert(index >= 0, alts+" does not contain "+imeth)
        def altName(index: Int) = (imeth.name+"$extension"+index).toTermName
        altName(index) #:: ((0 until alts.length).toStream filter (index != _) map altName)
      case decl =>
        assert(decl.exists, imeth.name+" not found in "+imeth.owner+"'s decls: "+imeth.owner.info.decls)
        Stream((imeth.name+"$extension").toTermName)
    }
  }

  /** Return the extension method that corresponds to given instance method `meth`. */
  def extensionMethod(imeth: Symbol)(implicit ctx: Context): TermSymbol =
    ctx.atPhase(thisTransformer.next) { implicit ctx =>
      // FIXME use toStatic instead?
      val companionInfo = imeth.owner.companionModule.info
      val candidates = extensionNames(imeth) map (companionInfo.decl(_).symbol) filter (_.exists)
      val matching = candidates filter (c => memberSignature(c.info) == imeth.signature)
      assert(matching.nonEmpty,
        sm"""|no extension method found for:
             |
             |  $imeth:${imeth.info.show} with signature ${imeth.signature}
             |
             | Candidates:
             |
             | ${candidates.map(c => c.name + ":" + c.info.show).mkString("\n")}
             |
             | Candidates (signatures normalized):
             |
             | ${candidates.map(c => c.name + ":" + c.info.signature + ":" + memberSignature(c.info)).mkString("\n")}
             |
             | Eligible Names: ${extensionNames(imeth).mkString(",")}""")
      matching.head.asTerm
    }

  private def createExtensionMethod(imeth: Symbol, staticClass: Symbol)(implicit ctx: Context): TermSymbol = {
    assert(ctx.phase == thisTransformer.next)
    val extensionName = extensionNames(imeth).head.toTermName
    val extensionMeth = ctx.newSymbol(staticClass, extensionName,
      imeth.flags | Final &~ (Override | Protected | AbsOverride),
      fullyParameterizedType(imeth.info, imeth.owner.asClass),
      privateWithin = imeth.privateWithin, coord = imeth.coord)
    extensionMeth.addAnnotations(from = imeth)(ctx.withPhase(thisTransformer))
      // need to change phase to add tailrec annotation which gets removed from original method in the same phase.
    extensionMeth
  }

  private val extensionDefs = mutable.Map[Symbol, mutable.ListBuffer[Tree]]()
  // TODO: this is state and should be per-run
  // todo: check that when transformation finished map is empty

  private def checkNonCyclic(pos: Position, seen: Set[Symbol], clazz: ClassSymbol)(implicit ctx: Context): Unit =
    if (seen contains clazz)
      ctx.error("value class may not unbox to itself", pos)
    else {
      val unboxed = underlyingOfValueClass(clazz).typeSymbol
      if (isDerivedValueClass(unboxed)) checkNonCyclic(pos, seen + clazz, unboxed.asClass)
    }

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (isDerivedValueClass(ctx.owner)) {
      /* This is currently redundant since value classes may not
         wrap over other value classes anyway.
        checkNonCyclic(ctx.owner.pos, Set(), ctx.owner) */
      tree
    } else if (ctx.owner.isStaticOwner) {
      extensionDefs remove tree.symbol.owner match {
        case Some(defns) if defns.nonEmpty =>
          cpy.Template(tree)(body = tree.body ++
            defns.map(transformFollowing(_)))
        case _ =>
          tree
      }
    } else tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    if (isMethodWithExtension(tree.symbol)) {
      val origMeth = tree.symbol
      val origClass = ctx.owner.asClass
      val staticClass = origClass.linkedClass
      assert(staticClass.exists, s"$origClass lacks companion, ${origClass.owner.definedPeriodsString} ${origClass.owner.info.decls} ${origClass.owner.info.decls}")
      val extensionMeth = extensionMethod(origMeth)
      ctx.log(s"Value class $origClass spawns extension method.\n  Old: ${origMeth.showDcl}\n  New: ${extensionMeth.showDcl}")
      val store: ListBuffer[Tree] = extensionDefs.get(staticClass) match {
        case Some(x) => x
        case None =>
          val newC = new ListBuffer[Tree]()
          extensionDefs(staticClass) = newC
          newC
      }
      store += atGroupEnd(fullyParameterizedDef(extensionMeth, tree)(_))
      cpy.DefDef(tree)(rhs = atGroupEnd(forwarder(extensionMeth, tree)(_)))
    } else tree
  }

  /** Implement Step 2 of SIP-15: rerouting calls.
   *  Note that this is different from Scala 2 which did the rerouting much later in
   *  the PostErasure phase.
   *  TODO: avoid code duplication with fullyParameterizedDef#rewireTree
   */
  private def rewire(select: Select, targs: List[Tree])(implicit ctx: Context): Tree = {
    val Select(qual, _) = select
    val origMeth = select.symbol
    if (isMethodWithExtension(origMeth)) {
      val extensionMeth = extensionMethod(origMeth)
      val origClass = origMeth.enclosingClass.asClass
      val base = qual.tpe.baseTypeWithArgs(origClass)
      assert(base.exists)
      ref(extensionMeth)
        .appliedToTypeTrees(targs ++ base.argInfos.map(TypeTree(_)))
        .appliedTo(qual)
    } else EmptyTree
  }

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      case TypeApply(sel @ Select(_,_), args) =>
        rewire(sel, args) orElse tree
      case _ =>
        tree
    }

  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree.tpe.widen match {
      case tp: PolyType =>
        tree // The rewiring will be handled by transformTypeApply
      case tp =>
        rewire(tree, Nil) orElse tree
    }


  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      // There is no extension method for == and != because they're defined in Any,
      // but we still want to replace them by calls to == and != on the unboxed values
      // to take advantage of the optimizations done by PeepholeOptimization, so we do
      // the following transform here:
      // v1 == v2 => v1.u == v2.u
      // v1 != v2 => v1.u != v2.u
      case BinaryOp(v1, op, v2) if (op eq defn.Any_==) || (op eq defn.Any_!=) =>
        val tpw1 = v1.tpe.widen
        val tpw2 = v2.tpe.widen
        val sym1 = tpw1.typeSymbol
        if (isDerivedValueClass(sym1) && (tpw1 =:= tpw2)) {
          val unboxSym = valueClassUnbox(sym1.asClass)
          val u1 = v1.select(unboxSym)
          val u2 = v2.select(unboxSym)
          // == and != are overloaded in primitive value classes
          applyOverloaded(u1, op.name.asTermName, List(u2), Nil, defn.BooleanType)
        } else tree
      case _ =>
        tree
    }
}
