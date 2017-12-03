package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import Denotations._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import Phases._
import ast.untpd
import ast.Trees._
import NameKinds.ImplMethName
import collection.mutable

/** Rewrite calls
 *
 *    super[M].f(args)
 *
 *  where M is a Scala 2.12 trait implemented by the current class to
 *
 *    M.f(this, args)
 *
 *  where f is a static member of M.
 */
class LinkScala2Impls extends MiniPhase with DenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "linkScala2Impls"
  override def changesMembers = true

  override def runsAfterGroupsOf: Set[Class[_ <: Phase]] = Set(classOf[Mixin])
    // Adds as a side effect static members to traits which can confuse Mixin,
    // that's why it is runsAfterGroupOf

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case mixin: ClassDenotation if mixin.is(Scala_2_12_Augmented) =>
      val ops = new MixinOps(mixin.symbol.asClass, thisPhase)
      import ops._

      val cinfo = mixin.classInfo

      def info_2_12(tp: Type) = tp match {
        case mt: MethodType =>
          MethodType(nme.SELF :: mt.paramNames, mixin.typeRef :: mt.paramInfos, mt.resType)
      }
      def newImpl(meth: TermSymbol): Symbol = {
        val mold =
          if (meth.isConstructor)
            meth.copySymDenotation(
              name = nme.TRAIT_CONSTRUCTOR,
              info = MethodType(Nil, defn.UnitType))
          else meth.ensureNotPrivate
        meth.copy(
          // owner = mixin.symbol,
          name = if (meth.isConstructor) mold.name.asTermName else ImplMethName(mold.name.asTermName),
          flags = Method | JavaStatic,
          info = info_2_12(mold.info)
        )
      }

      val decls = cinfo.decls
      val decls1 = decls.cloneScope
      for (sym <- decls) {
        if (needsForwarder(sym) || sym.isConstructor || sym.isGetter && sym.is(Lazy) ||
            sym.is(Method, butNot = Deferred))
          decls1.enter(newImpl(sym.asTerm))
      }

      mixin.copySymDenotation(info =
        cinfo.derivedClassInfo(decls = decls1))
    case _ =>
      ref
  }


  override def transformApply(app: Apply)(implicit ctx: Context) = {
    def currentClass = ctx.owner.enclosingClass.asClass
    app match {
      case Apply(sel @ Select(Super(_, _), _), args)
      if sel.symbol.owner.is(Scala2xTrait) && currentClass.mixins.contains(sel.symbol.owner) =>
        val impl = implMethod(sel.symbol)
        if (impl.exists) Apply(ref(impl), This(currentClass) :: args).withPos(app.pos)
        else app // could have been an abstract method in a trait linked to from a super constructor
      case _ =>
        app
    }
  }

  /** The 2.12 implementation method of a super call or implementation class target */
  private def implMethod(meth: Symbol)(implicit ctx: Context): Symbol = {
    val implName = ImplMethName(meth.name.asTermName)
    val cls = meth.owner
    if (cls.is(Scala_2_12_Trait))
      if (meth.isConstructor)
        cls.info.decl(nme.TRAIT_CONSTRUCTOR).symbol
      else
        cls.info.decl(implName)
          .suchThat(c => FullParameterization.memberSignature(c.info) == meth.signature)
          .symbol
    else throw new AssertionError(i"no impl method for $meth")
  }

  private val Scala2xTrait = allOf(Scala2x, Trait)
}
