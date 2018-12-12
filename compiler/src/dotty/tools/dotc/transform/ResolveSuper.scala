package dotty.tools.dotc
package transform

import core._
import MegaPhase._
import Contexts.Context
import Flags._
import SymUtils._
import Symbols._
import Decorators._
import DenotTransformers._
import NameOps._
import NameKinds._
import ResolveSuper._

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

  override def phaseName: String = ResolveSuper.name

  override def runsAfter: Set[String] = Set(ElimByName.name, // verified empirically, need to figure out what the reason is.
                               AugmentScala2Traits.name,
                               PruneErasedDefs.name) // Erased decls make `isCurrent` work incorrectly

  override def changesMembers: Boolean = true // the phase adds super accessors and method forwarders

  override def transformTemplate(impl: Template)(implicit ctx: Context): Template = {
    val cls = impl.symbol.owner.asClass
    val ops = new MixinOps(cls, thisPhase)
    import ops._

    def superAccessors(mixin: ClassSymbol): List[Tree] =
      for (superAcc <- mixin.info.decls.filter(_.isSuperAccessor))
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

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context): Tree = {
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
}

object ResolveSuper {
  val name: String = "resolveSuper"

  /** Returns the symbol that is accessed by a super-accessor in a mixin composition.
   *
   *  @param base       The class in which everything is mixed together
   *  @param acc        The symbol statically referred to by the superaccessor in the trait
   *  @param mixin      The trait where the superaccessor is defined
   */
  def rebindSuper(base: Symbol, acc: Symbol)(implicit ctx: Context): Symbol = {
    var bcs = base.info.baseClasses.dropWhile(acc.owner != _).tail
    var sym: Symbol = NoSymbol
    val SuperAccessorName(memberName) = acc.name.unexpandedName
    ctx.debuglog(i"starting rebindsuper from $base of ${acc.showLocated}: ${acc.info} in $bcs, name = $memberName")
    while (bcs.nonEmpty && sym == NoSymbol) {
      val cur = bcs.head
      val other = cur.info.nonPrivateDecl(memberName)
      if (ctx.settings.Ydebug.value)
        ctx.log(i"rebindsuper ${bcs.head} $other deferred = ${other.symbol.is(Deferred)}")
      val otherMember = other.matchingDenotation(base.thisType, base.thisType.memberInfo(acc))
      if (otherMember.exists) {
        sym = otherMember.symbol
        // Having a matching denotation is not enough: it should also be a subtype
        // of the superaccessor's type, see i5433.scala for an example where this matters
        val otherTp = otherMember.asSeenFrom(base.typeRef).info
        val accTp = acc.asSeenFrom(base.typeRef).info
        if (!(otherTp <:< accTp)) {
          val superCall = i"super.$memberName"
          val accSuperCall = i"super[${acc.owner.name}].$memberName"
          val resolvedSuperCall = i"super[${cur.name}].$memberName"
          val overriddenOwnerName = {
            val overriddenIterator = other.symbol.allOverriddenSymbols
            if (overriddenIterator.hasNext)
              overriddenIterator.next.owner.name
            else
              "SomeParent"
          }
          val overriddenSuperCall = i"super[$overriddenOwnerName].$memberName"
          ctx.error(
            hl"""$base cannot be instantiated due to a conflict between its parents when
                |resolving a super-call:
                |
                |1. One of its parent (${acc.owner}) contains a call $superCall in its body.
                |2. Because ${cur.name} comes before ${acc.owner.name} in the linearization
                |   order of ${base.name}, and because ${cur.name} overrides $memberName,
                |   this is resolved as a call to $resolvedSuperCall in ${base.name}.
                |3. However:
                |   ${otherTp.widenExpr} (the type of $resolvedSuperCall in ${base.name})
                |   is not a subtype of
                |   ${accTp.widenExpr} (the type of $accSuperCall in ${base.name})
                |
                |Here are two possible ways to resolve this:
                |
                |1. Change the linearization order of ${base.name} such that
                |   ${acc.owner.name} comes before ${cur.name}.
                |2. Alternatively, replace $superCall in the body of ${acc.owner} by a
                |   super-call to a specific parent, e.g. $overriddenSuperCall
                |""".stripMargin, base.sourcePos)
        }
      }

      bcs = bcs.tail
    }
    assert(sym.exists)
    sym
  }
}
