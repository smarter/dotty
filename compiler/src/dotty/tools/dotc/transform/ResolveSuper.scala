package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import SymDenotations._
import Types._
import Decorators._
import DenotTransformers._
import StdNames._
import NameOps._
import NameKinds._
import ast.Trees._
import util.Positions._
import Names._
import collection.mutable
import ResolveSuper._
import NameKinds.ImplMethName

/** This phase adds super accessors and method overrides where
 *  linearization differs from Java's rule for default methods in interfaces.
 *  In particular:
 *
 *        For every trait M directly implemented by the class (see SymUtils.mixin), in
 *        reverse linearization order, add the following definitions to C:
 *
 *          3.1 (done in `superAccessors`) For every superAccessor
 *              `<mods> def super$f[Ts](ps1)...(psN): U` in M:
 *
 *                <mods> def super$f[Ts](ps1)...(psN): U = super[S].f[Ts](ps1)...(psN)
 *
 *              where `S` is the superclass of `M` in the linearization of `C`.
 *
 *          3.2 (done in `methodOverrides`) For every method
 *              `<mods> def f[Ts](ps1)...(psN): U` in M` that needs to be disambiguated:
 *
 *                <mods> def f[Ts](ps1)...(psN): U = super[M].f[Ts](ps1)...(psN)
 *
 *        A method in M needs to be disambiguated if it is concrete, not overridden in C,
 *        and if it overrides another concrete method.
 *
 *  This is the first part of what was the mixin phase. It is complemented by
 *  Mixin, which runs after erasure.
 */
class ResolveSuper extends MiniPhase with IdentityDenotTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName: String = "resolveSuper"

  override def runsAfter = Set(classOf[ElimByName], // verified empirically, need to figure out what the reason is.
                               classOf[AugmentScala2Traits])

  override def changesMembers = true // the phase adds super accessors and method forwarders

  override def transformTemplate(impl: Template)(implicit ctx: Context) = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisPhase)
    import ops._

    def superAccessors(mixin: ClassSymbol): List[Tree] =
      for (superAcc <- mixin.info.decls.filter(s => s.isSuperAccessor && !s.name.is(ImplMethName)))
        yield {
          util.Stats.record("super accessors")
          polyDefDef(implementation(superAcc.asTerm), forwarder(rebindSuper(cls, superAcc)))
        }

    def methodOverrides(mixin: ClassSymbol): List[Tree] =
      for (meth <- mixin.info.decls.toList if needsForwarder(meth))
        yield {
          util.Stats.record("method forwarders")
          polyDefDef(implementation(meth.asTerm), forwarder(meth))
        }

    val overrides = mixins.flatMap(mixin => superAccessors(mixin) ::: methodOverrides(mixin))

    cpy.Template(impl)(body = overrides ::: impl.body)
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context) = {
    val meth = ddef.symbol.asTerm
    if (meth.isSuperAccessor && !meth.is(Deferred)) {
      assert(ddef.rhs.isEmpty)
      val cls = meth.owner.asClass
      val ops = new MixinOps(cls, thisPhase)
      import ops._
      polyDefDef(meth, forwarder(rebindSuper(cls, meth)))
    }
    else ddef
  }

  private val PrivateOrAccessorOrDeferred = Private | Accessor | Deferred
}

object ResolveSuper {
  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param acc        The symbol statically referred to by the superaccessor in the trait
   */
  def rebindSuper(base: Symbol, acc: Symbol)(implicit ctx: Context): Symbol = {
    var bcs = base.info.baseClasses.dropWhile(acc.owner != _).tail
    var sym: Symbol = NoSymbol
    val SuperAccessorName(memberName) = acc.name.unexpandedName
    ctx.debuglog(i"starting rebindsuper from $base of ${acc.showLocated}: ${acc.info} in $bcs, name = $memberName")
    while (bcs.nonEmpty && sym == NoSymbol) {
      val other = bcs.head.info.nonPrivateDecl(memberName)
      if (ctx.settings.Ydebug.value)
        ctx.log(i"rebindsuper ${bcs.head} $other deferred = ${other.symbol.is(Deferred)}")
      sym = other.matchingDenotation(base.thisType, base.thisType.memberInfo(acc)).symbol
      bcs = bcs.tail
    }
    assert(sym.exists)
    sym
  }
}
