package dotty.tools.dotc
package transform

import core._
import Symbols._, Types._, Contexts._, DenotTransformers._, Flags._
import util.Spans._
import SymUtils._
import StdNames._, NameOps._
import Decorators._

class MixinOps(cls: ClassSymbol, thisPhase: DenotTransformer)(implicit ctx: Context) {
  import ast.tpd._

  val superCls: Symbol = cls.superClass
  val mixins: List[ClassSymbol] = cls.mixins

  lazy val JUnit4Annotations: List[Symbol] = List("Test", "Ignore", "Before", "After", "BeforeClass", "AfterClass").
    map(n => ctx.getClassIfDefined("org.junit." + n)).
    filter(_.exists)

  def mkForwarder(member: TermSymbol, extraFlags: FlagSet = EmptyFlags): TermSymbol = {
    val res = member.copy(
      owner = cls,
      name = member.name.stripScala2LocalSuffix,
      flags = member.flags &~ Deferred | extraFlags,
      info = cls.thisType.memberInfo(member)).enteredAfter(thisPhase).asTerm
    res.addAnnotations(member.annotations.filter(_.symbol != defn.TailrecAnnot))
    res
  }

  def superRef(target: Symbol, span: Span = cls.span): Tree = {
    val sup = if (target.isConstructor && !target.owner.is(Trait))
      Super(This(cls), tpnme.EMPTY, true)
    else
      Super(This(cls), target.owner.name.asTypeName, false, target.owner)
    //println(i"super ref $target on $sup")
    ast.untpd.Select(sup.withSpan(span), target.name)
      .withType(NamedType(sup.tpe, target))
    //sup.select(target)
  }

  /** Is `sym` a member of implementing class `cls`?
   *  The test is performed at phase `thisPhase`.
   */
  def isCurrent(sym: Symbol): Boolean =
    ctx.atPhase(thisPhase) { implicit ctx =>
      cls.info.nonPrivateMember(sym.name).hasAltWith(_.symbol == sym)
      // this is a hot spot, where we spend several seconds while compiling stdlib
      // unfortunately it will discard and recompute all the member chaches,
      // both making itself slow and slowing down anything that runs after it
      // because resolveMixins uses hacks with explicit adding to scopes through .enter
      // this cannot be fixed by a smarter caching strategy. With current implementation
      // we HAVE to discard caches here for correctness
    }

  /** Does mixin `method` need a forwarder in class `cls`
   *  Method needs a forwarder in those cases:
   *   - there's a class defining a method with same signature
   *   - there are multiple traits defining method with same signature
   */
  def needsMixinForwarder(meth: Symbol): Boolean = {
    lazy val competingMethods = competingMethodsIterator(meth).toList

    def needsDisambiguation = competingMethods.exists(x=> !(x is Deferred)) // multiple implementations are available
    def hasNonInterfaceDefinition = competingMethods.exists(!_.owner.is(Trait)) // there is a definition originating from class
    meth.is(Method, butNot = PrivateOrAccessorOrDeferred) &&
    (meth.owner.is(Scala2x) || needsDisambiguation || hasNonInterfaceDefinition || needsJUnit4Fix(meth) ) &&
    isCurrent(meth)
  }

  private def needsJUnit4Fix(meth: Symbol): Boolean = {
    meth.annotations.nonEmpty && JUnit4Annotations.exists(annot => meth.hasAnnotation(annot))
  }

  final val PrivateOrAccessor: FlagSet = Private | Accessor
  final val PrivateOrAccessorOrDeferred: FlagSet = Private | Accessor | Deferred

  def forwarder(target: Symbol): List[Type] => List[List[Tree]] => Tree =
    targs => vrefss =>
      superRef(target).appliedToTypes(targs).appliedToArgss(vrefss)

  private def competingMethodsIterator(meth: Symbol): Iterator[Symbol] = {
    cls.baseClasses.iterator
      .filter(_ ne meth.owner)
      .map(base => meth.overriddenSymbol(base, cls))
      .filter(_.exists)
  }
}
