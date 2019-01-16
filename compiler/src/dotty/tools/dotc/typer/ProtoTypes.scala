package dotty.tools
package dotc
package typer

import core._
import ast._
import Contexts._, Types._, Denotations._, Names._, StdNames._, NameOps._, Symbols._
import NameKinds.DepParamName
import Trees._
import Constants._
import util.{Stats, SimpleIdentityMap}
import Decorators._
import Uniques._
import config.Printers.typr

import scala.annotation.internal.sharable

object ProtoTypes {

  import tpd._

  /** A trait defining an `isCompatible` method. */
  trait Compatibility {

    /** Is there an implicit conversion from `tp` to `pt`? */
    def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean

    /** A type `tp` is compatible with a type `pt` if one of the following holds:
     *    1. `tp` is a subtype of `pt`
     *    2. `pt` is by name parameter type, and `tp` is compatible with its underlying type
     *    3. there is an implicit conversion from `tp` to `pt`.
     *    4. `tp` is a numeric subtype of `pt` (this case applies even if implicit conversions are disabled)
     */
    def isCompatible(tp: Type, pt: Type)(implicit ctx: Context): Boolean =
      (tp.widenExpr relaxed_<:< pt.widenExpr) || viewExists(tp, pt)

    /** Test compatibility after normalization in a fresh typerstate. */
    def normalizedCompatible(tp: Type, pt: Type)(implicit ctx: Context): Boolean =
      ctx.test { implicit ctx =>
        val normTp = normalize(tp, pt)
        isCompatible(normTp, pt) || pt.isRef(defn.UnitClass) && normTp.isParameterless
      }

    private def disregardProto(pt: Type)(implicit ctx: Context): Boolean = pt.dealias match {
      case _: OrType => true
      case pt => pt.isRef(defn.UnitClass)
    }

    /** Check that the result type of the current method
     *  fits the given expected result type.
     */
    def constrainResult(mt: Type, pt: Type)(implicit ctx: Context): Boolean = {
      val savedConstraint = ctx.typerState.constraint
      val res = pt.widenExpr match {
        case pt: FunProto =>
          mt match {
            case mt: MethodType => constrainResult(resultTypeApprox(mt), pt.resultType)
            case _ => true
          }
        case _: ValueTypeOrProto if !disregardProto(pt) =>
          isCompatible(normalize(mt, pt), pt)
        case pt: WildcardType if pt.optBounds.exists =>
          isCompatible(normalize(mt, pt), pt)
        case _ =>
          true
      }
      if (!res) ctx.typerState.resetConstraintTo(savedConstraint)
      res
    }

    /** Constrain result with special case if `meth` is an inlineable method in an inlineable context.
     *  In that case, we should always succeed and not constrain type parameters in the expected type,
     *  because the actual return type can be a subtype of the currently known return type.
     *  However, we should constrain parameters of the declared return type. This distinction is
     *  achieved by replacing expected type parameters with wildcards.
     */
    def constrainResult(meth: Symbol, mt: Type, pt: Type)(implicit ctx: Context): Boolean =
      if (Inliner.isInlineable(meth)) {
        constrainResult(mt, wildApprox(pt))
        true
      }
      else constrainResult(mt, pt)
  }

  object NoViewsAllowed extends Compatibility {
    override def viewExists(tp: Type, pt: Type)(implicit ctx: Context): Boolean = false
  }

  /** A trait for prototypes that match all types */
  trait MatchAlways extends ProtoType {
    def isMatchedBy(tp1: Type)(implicit ctx: Context): Boolean = true
    def map(tm: TypeMap)(implicit ctx: Context): ProtoType = this
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T = x
    override def toString: String = getClass.toString
  }

  /** A class marking ignored prototypes that can be revealed by `deepenProto` */
  case class IgnoredProto(ignored: Type) extends UncachedGroundType with MatchAlways {
    override def revealIgnored = ignored.revealIgnored
    override def deepenProto(implicit ctx: Context): Type = ignored
  }

  /** A prototype for expressions [] that are part of a selection operation:
   *
   *       [ ].name: proto
   */
  abstract case class SelectionProto(name: Name, memberProto: Type, compat: Compatibility, privateOK: Boolean)
  extends CachedProxyType with ProtoType with ValueTypeOrProto {

    /** Is the set of members of this type unknown? This is the case if:
     *  1. The type has Nothing or Wildcard as a prefix or underlying type
     *  2. The type has an uninstantiated TypeVar as a prefix or underlying type,
     *  or as an upper bound of a prefix or underlying type.
     */
    private def hasUnknownMembers(tp: Type)(implicit ctx: Context): Boolean = tp match {
      case tp: TypeVar => !tp.isInstantiated
      case tp: WildcardType => true
      case NoType => true
      case tp: TypeRef =>
        val sym = tp.symbol
        sym == defn.NothingClass ||
        !sym.isStatic && {
          hasUnknownMembers(tp.prefix) || {
            val bound = tp.info.hiBound
            bound.isProvisional && hasUnknownMembers(bound)
          }
        }
      case tp: AppliedType => hasUnknownMembers(tp.tycon) || hasUnknownMembers(tp.superType)
      case tp: TypeProxy => hasUnknownMembers(tp.superType)
      case _ => false
    }

    override def isMatchedBy(tp1: Type)(implicit ctx: Context): Boolean = {
      name == nme.WILDCARD || hasUnknownMembers(tp1) ||
      {
        val mbr = if (privateOK) tp1.member(name) else tp1.nonPrivateMember(name)
        def qualifies(m: SingleDenotation) =
          memberProto.isRef(defn.UnitClass) ||
          tp1.isValueType && compat.normalizedCompatible(NamedType(tp1, name, m), memberProto)
            // Note: can't use `m.info` here because if `m` is a method, `m.info`
            //       loses knowledge about `m`'s default arguments.
        mbr match { // hasAltWith inlined for performance
          case mbr: SingleDenotation => mbr.exists && qualifies(mbr)
          case _ => mbr hasAltWith qualifies
        }
      }
    }

    def underlying(implicit ctx: Context): Type = WildcardType

    def derivedSelectionProto(name: Name, memberProto: Type, compat: Compatibility)(implicit ctx: Context): SelectionProto =
      if ((name eq this.name) && (memberProto eq this.memberProto) && (compat eq this.compat)) this
      else SelectionProto(name, memberProto, compat, privateOK)

    override def equals(that: Any): Boolean = that match {
      case that: SelectionProto =>
        (name eq that.name) && (memberProto == that.memberProto) && (compat eq that.compat) && (privateOK == that.privateOK)
      case _ =>
        false
    }

    def map(tm: TypeMap)(implicit ctx: Context): SelectionProto = derivedSelectionProto(name, tm(memberProto), compat)
    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T = ta(x, memberProto)

    override def deepenProto(implicit ctx: Context): SelectionProto = derivedSelectionProto(name, memberProto.deepenProto, compat)

    override def computeHash(bs: Hashable.Binders): Int = {
      val delta = (if (compat eq NoViewsAllowed) 1 else 0) | (if (privateOK) 2 else 0)
      addDelta(doHash(bs, name, memberProto), delta)
    }
  }

  class CachedSelectionProto(name: Name, memberProto: Type, compat: Compatibility, privateOK: Boolean)
  extends SelectionProto(name, memberProto, compat, privateOK)

  object SelectionProto {
    def apply(name: Name, memberProto: Type, compat: Compatibility, privateOK: Boolean)(implicit ctx: Context): SelectionProto = {
      val selproto = new CachedSelectionProto(name, memberProto, compat, privateOK)
      if (compat eq NoViewsAllowed) unique(selproto) else selproto
    }
  }

  /** Create a selection proto-type, but only one level deep;
   *  treat constructors specially
   */
  def selectionProto(name: Name, tp: Type, typer: Typer)(implicit ctx: Context): TermType =
    if (name.isConstructorName) WildcardType
    else tp match {
      case tp: UnapplyFunProto => new UnapplySelectionProto(name)
      case tp => SelectionProto(name, IgnoredProto(tp), typer, privateOK = true)
    }

  /** A prototype for expressions [] that are in some unspecified selection operation
   *
   *    [].?: ?
   *
   *  Used to indicate that expression is in a context where the only valid
   *  operation is further selection. In this case, the expression need not be a value.
   *  @see checkValue
   */
  @sharable object AnySelectionProto extends SelectionProto(nme.WILDCARD, WildcardType, NoViewsAllowed, true)

  /** A prototype for selections in pattern constructors */
  class UnapplySelectionProto(name: Name) extends SelectionProto(name, WildcardType, NoViewsAllowed, true)

  trait ApplyingProto extends ProtoType   // common trait of ViewProto and FunProto
  trait FunOrPolyProto extends ProtoType  // common trait of PolyProto and FunProto

  class FunProtoState {

    /** The list of typed arguments, if all arguments are typed */
    var typedArgs: List[Tree] = Nil

    /** A map in which typed arguments can be stored to be later integrated in `typedArgs`. */
    var typedArg: SimpleIdentityMap[untpd.Tree, Tree] = SimpleIdentityMap.Empty

    /** A map recording the typer states and constraints in which arguments stored in myTypedArg were typed */
    var evalState: SimpleIdentityMap[untpd.Tree, (TyperState, Constraint)] = SimpleIdentityMap.Empty

    /** If true, the application of this prototype was canceled. */
    var toDrop: Boolean = false
  }

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType
   */
  case class FunProto(args: List[untpd.Tree], resType: Type)(typer: Typer, state: FunProtoState = new FunProtoState)(implicit ctx: Context)
  extends UncachedGroundType with ApplyingProto with FunOrPolyProto {
    override def resultType(implicit ctx: Context): Type = resType

    def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean =
      typer.isApplicable(tp, Nil, unforcedTypedArgs, resultType)

    def derivedFunProto(args: List[untpd.Tree] = this.args, resultType: Type, typer: Typer = this.typer): FunProto =
      if ((args eq this.args) && (resultType eq this.resultType) && (typer eq this.typer)) this
      else new FunProto(args, resultType)(typer)

    override def notApplied: Type = WildcardType

    /** Forget the types of any arguments that have been typed producing a constraint
     *    - that is in a typer state that is not yet committed into the one of the current context `ctx`,
     *    - or that has been retracted from its typestate because oif a failed operation.
     *  This is necessary to avoid "orphan" TypeParamRefs that are referred to from
     *  type variables in the typed arguments, but that are not registered in the
     *  current constraint. Test cases are pos/t1756.scala and pos/i3538.scala.
     *  @return True if all arguments have types (in particular, no types were forgotten).
     */
    def allArgTypesAreCurrent()(implicit ctx: Context): Boolean = {
      state.evalState foreachBinding { (arg, tstateConstr) =>
        if ((tstateConstr._1.uncommittedAncestor.constraint `ne` ctx.typerState.constraint) ||
            tstateConstr._2.isRetracted) {
          typr.println(i"need to invalidate $arg / ${state.typedArg(arg)}, ${tstateConstr._2}, current = ${ctx.typerState.constraint}")
          state.typedArg = state.typedArg.remove(arg)
          state.evalState = state.evalState.remove(arg)
        }
      }
      state.typedArg.size == args.length
    }

    private def cacheTypedArg(arg: untpd.Tree, typerFn: untpd.Tree => Tree, force: Boolean)(implicit ctx: Context): Tree = {
      var targ = state.typedArg(arg)
      if (targ == null) {
        if (!force && untpd.functionWithUnknownParamType(arg).isDefined)
          // If force = false, assume ? rather than reporting an error.
          // That way we don't cause a "missing parameter" error in `typerFn(arg)`
          targ = arg.withType(WildcardType)
        else {
          targ = typerFn(arg)
          if (!ctx.reporter.hasUnreportedErrors) {
            // FIXME: This can swallow warnings by updating the typerstate from a nested
            // context that gets discarded later. But we do have to update the
            // typerstate if there are no errors. If we also omitted the next two lines
            // when warning were emitted, `pos/t1756.scala` would fail when run with -feature.
            // It would produce an orphan type parameter for CI when pickling.
            state.typedArg = state.typedArg.updated(arg, targ)
            state.evalState = state.evalState.updated(arg, (ctx.typerState, ctx.typerState.constraint))
          }
        }
      }
      targ
    }

    /** The typed arguments. This takes any arguments already typed using
     *  `typedArg` into account.
     *  @param  force   if true try to typecheck arguments even if they are functions
     *                  with unknown parameter types - this will then cause a
     *                  "missing parameter type" error
     */
    private def typedArgs(force: Boolean): List[Tree] = {
      if (state.typedArgs.size != args.length)
        state.typedArgs = args.mapconserve(cacheTypedArg(_, typer.typed(_), force))
      state.typedArgs
    }

    def typedArgs: List[Tree] = typedArgs(force = true)
    def unforcedTypedArgs: List[Tree] = typedArgs(force = false)

    /** Type single argument and remember the unadapted result in `myTypedArg`.
     *  used to avoid repeated typings of trees when backtracking.
     */
    def typedArg(arg: untpd.Tree, formal: Type)(implicit ctx: Context): Tree = {
      val locked = ctx.typerState.ownedVars
      val targ = cacheTypedArg(arg, typer.typedUnadapted(_, formal, locked), force = true)
      typer.adapt(targ, formal, locked)
    }

    /** The type of the argument `arg`, or `NoType` if `arg` has not been typed before
     *  or if `arg`'s typing produced a type error.
     */
    def typeOfArg(arg: untpd.Tree)(implicit ctx: Context): Type = {
      val t = state.typedArg(arg)
      if (t == null) NoType else t.tpe
    }

    private var myTupled: Type = NoType
    /** The same proto-type but with all arguments combined in a single tuple */
    def tupled: FunProto = myTupled match {
      case pt: FunProto =>
        pt
      case _ =>
        val pt = new FunProto(untpd.Tuple(args) :: Nil, resultType)(typer)
        myTupled = pt
        pt
    }

    /** Somebody called the `tupled` method of this prototype */
    def isTupled: Boolean = myTupled.isInstanceOf[FunProto]

    /** Cancel the application of this prototype. This can happen for a nullary
     *  application `f()` if `f` refers to a symbol that exists both in parameterless
     *  form `def f` and nullary method form `def f()`. A common example for such
     *  a method is `toString`. If in that case the type in the denotation is
     *  parameterless, we compensate by dropping the application.
     */
    def markAsDropped(): Unit = {
      assert(args.isEmpty)
      state.toDrop = true
    }

    def isDropped: Boolean = state.toDrop

    override def isErroneous(implicit ctx: Context): Boolean =
      state.typedArgs.tpes.exists(_.widen.isErroneous)

    override def toString: String = s"FunProto(${args mkString ","} => $resultType)"

    def map(tm: TypeMap)(implicit ctx: Context): FunProto =
      derivedFunProto(args, tm(resultType), typer)

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta.foldOver(x, typedArgs.tpes), resultType)

    override def deepenProto(implicit ctx: Context): FunProto = derivedFunProto(args, resultType.deepenProto, typer)

    override def withContext(newCtx: Context): ProtoType =
      if (newCtx `eq` ctx) this
      else {
        val pt2 = new FunProto(args, resType)(typer, state)(newCtx)
        if (myTupled.exists)
          pt2.myTupled = myTupled.asInstanceOf[FunProto].withContext(newCtx)
        pt2
      }
  }

  /** A prototype for expressions that appear in function position
   *
   *  [](args): resultType, where args are known to be typed
   */
  class FunProtoTyped(args: List[tpd.Tree], resultType: Type)(typer: Typer)(implicit ctx: Context) extends FunProto(args, resultType)(typer)(ctx) {
    override def typedArgs: List[tpd.Tree] = args
    override def withContext(ctx: Context): FunProtoTyped = this
  }

  /** A prototype for implicitly inferred views:
   *
   *    []: argType => resultType
   */
  abstract case class ViewProto(argType: Type, resType: Type)
  extends CachedGroundType with ApplyingProto {

    override def resultType(implicit ctx: Context): Type = resType

    def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean =
      ctx.typer.isApplicable(tp, argType :: Nil, resultType) || {
        resType match {
          case SelectionProto(name: TermName, mbrType, _, _) =>
            ctx.typer.hasExtensionMethod(tp, name, argType, mbrType)
              //.reporting(res => i"has ext $tp $name $argType $mbrType: $res")
          case _ =>
            false
        }
      }

    def derivedViewProto(argType: Type, resultType: Type)(implicit ctx: Context): ViewProto =
      if ((argType eq this.argType) && (resultType eq this.resultType)) this
      else ViewProto(argType, resultType)

    def map(tm: TypeMap)(implicit ctx: Context): ViewProto = derivedViewProto(tm(argType), tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta(x, argType), resultType)

    override def deepenProto(implicit ctx: Context): ViewProto = derivedViewProto(argType, resultType.deepenProto)
  }

  class CachedViewProto(argType: Type, resultType: Type) extends ViewProto(argType, resultType) {
    override def computeHash(bs: Hashable.Binders): Int = doHash(bs, argType, resultType)
  }

  object ViewProto {
    def apply(argType: Type, resultType: Type)(implicit ctx: Context): ViewProto =
      unique(new CachedViewProto(argType, resultType))
  }

  class UnapplyFunProto(argType: Type, typer: Typer)(implicit ctx: Context) extends FunProto(
    untpd.TypedSplice(dummyTreeOfType(argType))(ctx) :: Nil, WildcardType)(typer)

  /** A prototype for expressions [] that are type-parameterized:
   *
   *    [] [targs] resultType
   */
  case class PolyProto(targs: List[Tree], resType: Type) extends UncachedGroundType with FunOrPolyProto {

    override def resultType(implicit ctx: Context): Type = resType

    override def isMatchedBy(tp: Type)(implicit ctx: Context): Boolean = {
      def isInstantiatable(tp: Type) = tp.widen match {
        case tp: PolyType => tp.paramNames.length == targs.length
        case _ => false
      }
      isInstantiatable(tp) || tp.member(nme.apply).hasAltWith(d => isInstantiatable(d.info))
    }

    def derivedPolyProto(targs: List[Tree], resultType: Type): PolyProto =
      if ((targs eq this.targs) && (resType eq this.resType)) this
      else PolyProto(targs, resType)

    override def notApplied: Type = WildcardType

    def map(tm: TypeMap)(implicit ctx: Context): PolyProto =
      derivedPolyProto(targs, tm(resultType))

    def fold[T](x: T, ta: TypeAccumulator[T])(implicit ctx: Context): T =
      ta(ta.foldOver(x, targs.tpes), resultType)

    override def deepenProto(implicit ctx: Context): PolyProto = derivedPolyProto(targs, resultType.deepenProto)
  }

  /** A prototype for expressions [] that are known to be functions:
   *
   *    [] _
   */
  @sharable object AnyFunctionProto extends UncachedGroundType with MatchAlways

  /** A prototype for type constructors that are followed by a type application */
  @sharable object AnyTypeConstructorProto extends UncachedGroundType with MatchAlways

  /** Add all parameters of given type lambda `tl` to the constraint's domain.
   *  If the constraint contains already some of these parameters in its domain,
   *  make a copy of the type lambda and add the copy's type parameters instead.
   *  Return either the original type lambda, or the copy, if one was made.
   *  Also, if `owningTree` is non-empty ot `alwaysAddTypeVars` is true, add a type variable
   *  for each parameter.
   *  @return  The added type lambda, and the list of created type variables.
   */
  def constrained(tl: TypeLambda, owningTree: untpd.Tree, alwaysAddTypeVars: Boolean = false)(implicit ctx: Context): (TypeLambda, List[TypeTree]) = {
    val state = ctx.typerState
    val addTypeVars = alwaysAddTypeVars || !owningTree.isEmpty
    if (tl.isInstanceOf[PolyType])
      assert(!(ctx.typerState.isCommittable && !addTypeVars),
        s"inconsistent: no typevars were added to committable constraint ${state.constraint}")
      // hk type lambdas can be added to constraints without typevars during match reduction

    def newTypeVars(tl: TypeLambda): List[TypeTree] =
      for (paramRef <- tl.paramRefs)
      yield {
        val tt = new TypeVarBinder().withPos(owningTree.pos)
        val tvar = new TypeVar(paramRef, state)
        state.ownedVars += tvar
        tt.withType(tvar)
      }

    /** Ensure that `tl` is not already in constraint, make a copy of necessary */
    def ensureFresh(tl: TypeLambda): TypeLambda =
      if (state.constraint contains tl) {
      	var paramInfos = tl.paramInfos
      	if (tl.isInstanceOf[HKLambda]) {
      	  // HKLambdas are hash-consed, need to create an artificial difference by adding
      	  // a LazyRef to a bound.
          val TypeBounds(lo, hi) :: pinfos1 = tl.paramInfos
          paramInfos = TypeBounds(lo, LazyRef(_ => hi)) :: pinfos1
        }
        ensureFresh(tl.newLikeThis(tl.paramNames, paramInfos, tl.resultType))
      }
      else tl
    val added = ensureFresh(tl)
    val tvars = if (addTypeVars) newTypeVars(added) else Nil
    ctx.typeComparer.addToConstraint(added, tvars.tpes.asInstanceOf[List[TypeVar]])
    (added, tvars)
  }

  /**  Same as `constrained(tl, EmptyTree)`, but returns just the created type lambda */
  def constrained(tl: TypeLambda)(implicit ctx: Context): TypeLambda = constrained(tl, EmptyTree)._1

  def newTypeVar(bounds: TypeBounds)(implicit ctx: Context): TypeVar = {
    val poly = PolyType(DepParamName.fresh().toTypeName :: Nil)(
        pt => bounds :: Nil,
        pt => defn.AnyType)
    constrained(poly, untpd.EmptyTree, alwaysAddTypeVars = true)
      ._2.head.tpe.asInstanceOf[TypeVar]
  }

  /** Create a new TypeVar that represents a dependent method parameter singleton */
  def newDepTypeVar(tp: Type)(implicit ctx: Context): TypeVar = {
    newTypeVar(TypeBounds.upper(AndType(tp.widenExpr, defn.SingletonClass.typeRef)))
  }
  /** The result type of `mt`, where all references to parameters of `mt` are
   *  replaced by either wildcards (if typevarsMissContext) or TypeParamRefs.
   */
  def resultTypeApprox(mt: MethodType)(implicit ctx: Context): Type =
    if (mt.isResultDependent) {
      def replacement(tp: Type) =
        if (ctx.mode.is(Mode.TypevarsMissContext) ||
            !tp.widenExpr.isValueTypeOrWildcard) WildcardType
        else newDepTypeVar(tp)
      mt.resultType.substParams(mt, mt.paramInfos.map(replacement))
    }
    else mt.resultType

  /** The normalized form of a type
   *   - unwraps polymorphic types, tracking their parameters in the current constraint
   *   - skips implicit parameters of methods and functions;
   *     if result type depends on implicit parameter, replace with fresh type dependent parameter.
   *   - converts non-dependent method types to the corresponding function types
   *     unless the expected type is an ApplyingProto or IgnoredProto.
   *   - dereferences parameterless method types
   *   - dereferences nullary method types provided the corresponding function type
   *     is not a subtype of the expected type.
   * Note: We need to take account of the possibility of inserting a () argument list in normalization. Otherwise, a type with a
   *     def toString(): String
   * member would not count as a valid solution for ?{toString: String}. This would then lead to an implicit
   * insertion, with a nice explosion of inference search because of course every implicit result has some sort
   * of toString method. The problem is solved by dereferencing nullary method types if the corresponding
   * function type is not compatible with the prototype.
   */
  def normalize(tp: Type, pt: Type)(implicit ctx: Context): Type = Stats.track("normalize") {
    tp.widenSingleton match {
      case poly: PolyType =>
        normalize(constrained(poly).resultType, pt)
      case mt: MethodType =>
        if (mt.isImplicitMethod) normalize(resultTypeApprox(mt), pt)
        else if (mt.isResultDependent) tp
        else {
          val rt = normalize(mt.resultType, pt)
          pt match {
            case pt: IgnoredProto  =>
              tp
            case pt: ApplyingProto =>
              if (rt eq mt.resultType) tp
              else mt.derivedLambdaType(mt.paramNames, mt.paramInfos, rt)
            case _ =>
              val ft = defn.FunctionOf(mt.paramInfos, rt)
              if (mt.paramInfos.nonEmpty || ft <:< pt) ft else rt
          }
        }
      case et: ExprType =>
        normalize(et.resultType, pt)
      case wtp =>
        val iftp = defn.asImplicitFunctionType(wtp)
        if (iftp.exists) normalize(iftp.dropDependentRefinement.argInfos.last, pt) else tp
    }
  }

  /** Approximate occurrences of parameter types and uninstantiated typevars
   *  by wildcard types.
   */
  private def wildApprox(tp: Type, theMap: WildApproxMap, seen: Set[TypeParamRef])(implicit ctx: Context): Type = tp match {
    case tp: NamedType => // default case, inlined for speed
      val isPatternBoundTypeRef = tp.isInstanceOf[TypeRef] && tp.symbol.is(Flags.Case) && !tp.symbol.isClass
      if (isPatternBoundTypeRef) WildcardType(tp.underlying.bounds)
      else if (tp.symbol.isStatic || (tp.prefix `eq` NoPrefix)) tp
      else tp.derivedSelect(wildApprox(tp.prefix, theMap, seen))
    case tp @ AppliedType(tycon, args) =>
      wildApprox(tycon, theMap, seen) match {
        case _: WildcardType => WildcardType // this ensures we get a * type
        case tycon1 => tp.derivedAppliedType(tycon1, args.mapConserve(wildApprox(_, theMap, seen)))
      }
    case tp: RefinedType => // default case, inlined for speed
      tp.derivedRefinedType(
          wildApprox(tp.parent, theMap, seen),
          tp.refinedName,
          wildApprox(tp.refinedInfo, theMap, seen))
    case tp: AliasingBounds => // default case, inlined for speed
      tp.derivedAlias(wildApprox(tp.alias, theMap, seen))
    case tp @ TypeParamRef(poly, pnum) =>
      def wildApproxBounds(bounds: TypeBounds) =
        if (seen.contains(tp)) WildcardType
        else WildcardType(wildApprox(bounds, theMap, seen + tp).bounds)
      def unconstrainedApprox = wildApproxBounds(poly.paramInfos(pnum))
      def approxPoly =
        if (ctx.mode.is(Mode.TypevarsMissContext)) unconstrainedApprox
        else
          ctx.typerState.constraint.entry(tp) match {
            case bounds: TypeBounds => wildApproxBounds(bounds)
            case NoType             => unconstrainedApprox
            case inst               => wildApprox(inst, theMap, seen)
          }
      approxPoly
    case TermParamRef(mt, pnum) =>
      WildcardType(TypeBounds.upper(wildApprox(mt.paramInfos(pnum), theMap, seen)))
    case tp: TypeVar =>
      wildApprox(tp.underlying, theMap, seen)
    case tp: AndType =>
      def approxAnd = {
        val tp1a = wildApprox(tp.tp1, theMap, seen)
        val tp2a = wildApprox(tp.tp2, theMap, seen)
        def wildBounds(tp: Type) =
          if (tp.isInstanceOf[WildcardType]) tp.bounds else TypeBounds.upper(tp)
        if (tp1a.isInstanceOf[WildcardType] || tp2a.isInstanceOf[WildcardType])
          WildcardType(wildBounds(tp1a) & wildBounds(tp2a))
        else
          tp.derivedAndType(tp1a, tp2a)
      }
      approxAnd
    case tp: OrType =>
      def approxOr = {
        val tp1a = wildApprox(tp.tp1, theMap, seen)
        val tp2a = wildApprox(tp.tp2, theMap, seen)
        if (tp1a.isInstanceOf[WildcardType] || tp2a.isInstanceOf[WildcardType])
          WildcardType(tp1a.bounds | tp2a.bounds)
        else
          tp.derivedOrType(tp1a, tp2a)
      }
      approxOr
    case tp: SelectionProto =>
      tp.derivedSelectionProto(tp.name, wildApprox(tp.memberProto, theMap, seen), NoViewsAllowed)
    case tp: ViewProto =>
      tp.derivedViewProto(
          wildApprox(tp.argType, theMap, seen),
          wildApprox(tp.resultType, theMap, seen))
    case  _: ThisType | _: BoundType => // default case, inlined for speed
      tp
    case _ =>
      (if (theMap != null && seen.eq(theMap.seen)) theMap else new WildApproxMap(seen))
        .mapOver(tp)
  }

  final def wildApprox(tp: Type)(implicit ctx: Context): Type = wildApprox(tp, null, Set.empty)

  @sharable object AssignProto extends UncachedGroundType with MatchAlways

  private[ProtoTypes] class WildApproxMap(val seen: Set[TypeParamRef])(implicit ctx: Context) extends TypeMap {
    def apply(tp: Type): Type = wildApprox(tp, this, seen)
  }

  /** Dummy tree to be used as an argument of a FunProto or ViewProto type */
  object dummyTreeOfType {
    def apply(tp: Type): Tree = untpd.Literal(Constant(null)) withTypeUnchecked tp
    def unapply(tree: untpd.Tree): Option[Type] = tree match {
      case Literal(Constant(null)) => Some(tree.typeOpt)
      case _ => None
    }
  }
}
