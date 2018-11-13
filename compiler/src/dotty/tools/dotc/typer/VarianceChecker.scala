package dotty.tools.dotc
package typer

import dotty.tools.dotc.ast.{ Trees, tpd }
import core._
import Types._, Contexts._, Flags._, Symbols._, Trees._
import Decorators._
import Variance._
import NameKinds._
import util.Positions._
import config.Printers.variances
import reporting.trace

/** Provides `check` method to check that all top-level definitions
 *  in tree are variance correct. Does not recurse inside methods.
 *  The method should be invoked once for each Template.
 */
object VarianceChecker {
  case class VarianceError(tvar: Symbol, required: Variance)
  def check(tree: tpd.Tree)(implicit ctx: Context): Unit =
    new VarianceChecker()(ctx).Traverser.traverse(tree)

  /** Check that variances of type lambda correspond to their occurrences in its body.
   *  Note: this is achieved by a mechanism separate from checking class type parameters.
   *  Question: Can the two mechanisms be combined in one?
   */
  def checkLambda(tree: tpd.LambdaTypeTree)(implicit ctx: Context): Unit = tree.tpe match {
    case tl: HKTypeLambda =>
      val checkOK = new TypeAccumulator[Boolean] {
        def error(tref: TypeParamRef) = {
          val param = tl.typeParams(tref.paramNum)
          val paramName = param.paramName
          val pos = tree.tparams
            .find(_.name.toTermName == paramName)
            .map(_.pos)
            .getOrElse(tree.pos)
          ctx.error(em"${param.paramVariance} type parameter $paramName occurs in ${variance} position in ${tl.resType}", pos)
        }
        def apply(x: Boolean, t: Type) = x && {
          t match {
            case tref: TypeParamRef if tref.binder `eq` tl =>
              val v = tl.typeParams(tref.paramNum).paramVariance
              variance.conforms(v) || { error(tref); false }
            case _ =>
              foldOver(x, t)
          }
        }
      }
      checkOK.apply(true, tl.resType)
    case _ =>
  }
}

class VarianceChecker()(implicit ctx: Context) {
  import VarianceChecker._
  import tpd._

  private object Validator extends TypeAccumulator[Option[VarianceError]] {
    private[this] var base: Symbol = _

    /** Is no variance checking needed within definition of `base`? */
    def ignoreVarianceIn(base: Symbol): Boolean = (
         base.isTerm
      || base.is(Package)
      || base.is(PrivateLocal)
    )

    /** The variance of a symbol occurrence of `tvar` seen at the level of the definition of `base`.
     *  The search proceeds from `base` to the owner of `tvar`.
     *  Initially the state is covariant, but it might change along the search.
     */
    def relativeVariance(tvar: Symbol, base: Symbol, v: Variance = Covariance): Variance = /*trace(i"relative variance of $tvar wrt $base, so far: $v")*/ {
      if (base == tvar.owner) v
      else if ((base is Param) && base.owner.isTerm)
        relativeVariance(tvar, paramOuter(base.owner), v.flip)
      else if (ignoreVarianceIn(base.owner)) Bivariance
      else if (base.isAliasType) relativeVariance(tvar, base.owner, Invariance)
      else relativeVariance(tvar, base.owner, v)
    }

    /** The next level to take into account when determining the
     *  relative variance with a method parameter as base. The method
     *  is always skipped. If the method is a constructor, we also skip
     *  its class owner, because constructors are not checked for variance
     *  relative to the type parameters of their own class. On the other
     *  hand constructors do count for checking the variance of type parameters
     *  of enclosing classes. I believe the Scala 2 rules are too lenient in
     *  that respect.
     */
    private def paramOuter(meth: Symbol) =
      if (meth.isConstructor) meth.owner.owner else meth.owner

    /** Check variance of abstract type `tvar` when referred from `base`. */
    private def checkVarianceOfSymbol(tvar: Symbol): Option[VarianceError] = {
      val relative = relativeVariance(tvar, base)
      if (relative == Bivariance) None
      else {
        val required = relative.compose(this.variance)
        def tvar_s = s"$tvar (${tvar.flags.variance} ${tvar.showLocated})"
        def base_s = s"$base in ${base.owner}" + (if (base.owner.isClass) "" else " in " + base.owner.enclosingClass)
        ctx.log(s"verifying $tvar_s is $required at $base_s")
        ctx.log(s"relative variance: $relative")
        ctx.log(s"current variance: $variance")
        ctx.log(s"owner chain: ${base.ownersIterator.toList}")
        if (tvar.variance.conforms(required)) None
        else Some(VarianceError(tvar, required))
      }
    }

    /** For PolyTypes, type parameters are skipped because they are defined
     *  explicitly (their TypeDefs will be passed here.) For MethodTypes, the
     *  same is true of the parameters (ValDefs).
     */
    def apply(status: Option[VarianceError], tp: Type): Option[VarianceError] = trace(s"variance checking $tp of $base at $variance", variances) {
      try
        if (status.isDefined) status
        else tp match {
          case tp: TypeRef =>
            val sym = tp.symbol
            if (sym.variance != Invariance && base.isContainedIn(sym.owner)) checkVarianceOfSymbol(sym)
            else if (sym.isAliasType) this(status, sym.info.bounds.hi)
            else foldOver(status, tp)
          case tp: MethodOrPoly =>
            this(status, tp.resultType) // params will be checked in their TypeDef or ValDef nodes.
          case AnnotatedType(_, annot) if annot.symbol == defn.UncheckedVarianceAnnot =>
            status
          case tp: MatchType =>
            apply(status, tp.bound)
          case tp: ClassInfo =>
            foldOver(status, tp.classParents)
          case _ =>
            foldOver(status, tp)
        }
      catch {
        case ex: Throwable => handleRecursive("variance check of", tp.show, ex)
      }
    }

    def validateDefinition(base: Symbol): Option[VarianceError] = {
      val saved = this.base
      this.base = base
      try apply(None, base.info)
      finally this.base = saved
    }
  }

  private object Traverser extends TreeTraverser {
    def checkVariance(sym: Symbol, pos: Position) = Validator.validateDefinition(sym) match {
      case Some(VarianceError(tvar, required)) =>
        def msg = i"${tvar.flags.variance} $tvar occurs in $required position in type ${sym.info} of $sym"
        if (ctx.scala2Mode &&
            (sym.owner.isConstructor || sym.ownersIterator.exists(_.is(ProtectedLocal)))) {
          ctx.migrationWarning(s"According to new variance rules, this is no longer accepted; need to annotate with @uncheckedVariance:\n$msg", pos)
            // patch(Position(pos.end), " @scala.annotation.unchecked.uncheckedVariance")
            // Patch is disabled until two TODOs are solved:
            // TODO use an import or shorten if possible
            // TODO need to use a `:' if annotation is on term
        }
        else ctx.error(msg, pos)
      case None =>
    }

    override def traverse(tree: Tree)(implicit ctx: Context) = {
      def sym = tree.symbol
      // No variance check for private/protected[this] methods/values.
      def skip =
        !sym.exists ||
        sym.is(PrivateLocal) ||
        sym.name.is(InlineAccessorName) || // TODO: should we exclude all synthetic members?
        sym.is(TypeParam) && sym.owner.isClass // already taken care of in primary constructor of class
      tree match {
        case defn: MemberDef if skip =>
          ctx.debuglog(s"Skipping variance check of ${sym.showDcl}")
        case tree: TypeDef =>
          checkVariance(sym, tree.pos)
          tree.rhs match {
            case rhs: Template => traverseChildren(rhs)
            case _ =>
          }
        case tree: ValDef =>
          checkVariance(sym, tree.pos)
        case DefDef(_, tparams, vparamss, _, _) =>
          checkVariance(sym, tree.pos)
          tparams foreach traverse
          vparamss foreach (_ foreach traverse)
        case _ =>
      }
    }
  }
}
