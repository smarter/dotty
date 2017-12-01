package dotty.tools
package dotc
package typer

import core._
import ast._
import Scopes._, Contexts._, Constants._, Types._, Symbols._, Names._, Flags._, Decorators._
import ErrorReporting._, Annotations._, Denotations._, SymDenotations._, StdNames._, TypeErasure._
import util.Positions._
import config.Printers.typr
import ast.Trees._
import NameOps._
import collection.mutable
import reporting.diagnostic.Message
import reporting.diagnostic.messages._
import Checking.checkNoPrivateLeaks

trait TypeAssigner {
  import tpd._

  /** The qualifying class of a this or super with prefix `qual` (which might be empty).
   *  @param packageOk   The qualifier may refer to a package.
   */
  def qualifyingClass(tree: untpd.Tree, qual: Name, packageOK: Boolean)(implicit ctx: Context): Symbol = {
    def qualifies(sym: Symbol) =
      sym.isClass && (
          qual.isEmpty ||
          sym.name == qual ||
          sym.is(Module) && sym.name.stripModuleClassSuffix == qual)
    ctx.outersIterator.map(_.owner).find(qualifies) match {
      case Some(c) if packageOK || !(c is Package) =>
        c
      case _ =>
        ctx.error(
          if (qual.isEmpty) tree.show + " can be used only in a class, object, or template"
          else qual.show + " is not an enclosing class", tree.pos)
        NoSymbol
    }
  }

  /** An abstraction of a class info, consisting of
   *   - the intersection of its parents,
   *   - refined by all non-private fields, methods, and type members,
   *   - abstracted over all type parameters (into a type lambda)
   *   - where all references to `this` of the class are closed over in a RecType.
   */
  def classBound(info: ClassInfo)(implicit ctx: Context): Type = {
    val cls = info.cls
    val parentType = info.parents.reduceLeft(ctx.typeComparer.andType(_, _))

    def addRefinement(parent: Type, decl: Symbol) = {
      val inherited =
        parentType.findMember(decl.name, cls.thisType, excluded = Private)
          .suchThat(decl.matches(_))
      val inheritedInfo = inherited.info
      if (inheritedInfo.exists && decl.info <:< inheritedInfo && !(inheritedInfo <:< decl.info)) {
        val r = RefinedType(parent, decl.name, decl.info)
        typr.println(i"add ref $parent $decl --> " + r)
        r
      }
      else
        parent
    }

    def close(tp: Type) = RecType.closeOver(rt => tp.substThis(cls, rt.recThis))

    def isRefinable(sym: Symbol) = !sym.is(Private) && !sym.isConstructor
    val refinableDecls = info.decls.filter(isRefinable)
    val raw = (parentType /: refinableDecls)(addRefinement)
    HKTypeLambda.fromParams(cls.typeParams, raw) match {
      case tl: HKTypeLambda => tl.derivedLambdaType(resType = close(tl.resType))
      case tp => close(tp)
    }
  }

  /** An upper approximation of the given type `tp` that does not refer to any symbol in `symsToAvoid`.
   *  We need to approximate with ranges:
   *
   *    term references to symbols in `symsToAvoid`,
   *    term references that have a widened type of which some part refers
   *    to a symbol in `symsToAvoid`,
   *    type references to symbols in `symsToAvoid`,
   *    this types of classes in `symsToAvoid`.
   *
   *  Type variables that would be interpolated to a type that
   *  needs to be widened are replaced by the widened interpolation instance.
   */
  def avoid(tp: Type, symsToAvoid: => List[Symbol])(implicit ctx: Context): Type = {
    val widenMap = new ApproximatingTypeMap {
      lazy val forbidden = symsToAvoid.toSet
      def toAvoid(sym: Symbol) = !sym.isStatic && forbidden.contains(sym)
      def partsToAvoid = new NamedPartsAccumulator(tp => toAvoid(tp.symbol))
      def apply(tp: Type): Type = tp match {
        case tp: TermRef
        if toAvoid(tp.symbol) || partsToAvoid(mutable.Set.empty, tp.info).nonEmpty =>
          tp.info.widenExpr match {
            case info: SingletonType => apply(info)
            case info => range(tp.info.bottomType, apply(info))
          }
        case tp: TypeRef if toAvoid(tp.symbol) =>
          tp.info match {
            case TypeAlias(alias) =>
              apply(alias)
            case TypeBounds(lo, hi) =>
              range(atVariance(-variance)(apply(lo)), apply(hi))
            case info: ClassInfo =>
              range(tp.bottomType, apply(classBound(info)))
            case _ =>
              range(tp.bottomType, tp.topType) // should happen only in error cases
          }
        case tp: ThisType if toAvoid(tp.cls) =>
          range(tp.bottomType, apply(classBound(tp.cls.classInfo)))
        case tp: TypeVar if ctx.typerState.constraint.contains(tp) =>
          val lo = ctx.typeComparer.instanceType(tp.origin, fromBelow = variance >= 0)
          val lo1 = apply(lo)
          if (lo1 ne lo) lo1 else tp
        case _ =>
          mapOver(tp)
      }

      /** Three deviations from standard derivedSelect:
       *   1. We first try a widening conversion to the type's info with
       *      the original prefix. Since the original prefix is known to
       *      be a subtype of the returned prefix, this can improve results.
       *   2. IThen, if the approximation result is a singleton reference C#x.type, we
       *      replace by the widened type, which is usually more natural.
       *   3. Finally, we need to handle the case where the prefix type does not have a member
       *      named `tp.name` anymmore. In that case, we need to fall back to Bot..Top.
       */
      override def derivedSelect(tp: NamedType, pre: Type) =
        if (pre eq tp.prefix)
          tp
        else tryWiden(tp, tp.prefix).orElse {
          if (tp.isTerm && variance > 0 && !pre.isInstanceOf[SingletonType])
          	apply(tp.info.widenExpr)
          else if (upper(pre).member(tp.name).exists)
            super.derivedSelect(tp, pre)
          else
            range(tp.bottomType, tp.topType)
        }
    }

    widenMap(tp)
  }

  def avoidingType(expr: Tree, bindings: List[Tree])(implicit ctx: Context): Type =
    avoid(expr.tpe, localSyms(bindings).filter(_.isTerm))

  def avoidPrivateLeaks(sym: Symbol, pos: Position)(implicit ctx: Context): Type =
    if (!sym.is(SyntheticOrPrivate) && sym.owner.isClass) checkNoPrivateLeaks(sym, pos)
    else sym.info

  def seqToRepeated(tree: Tree)(implicit ctx: Context): Tree =
    Typed(tree, TypeTree(tree.tpe.widen.translateParameterized(defn.SeqClass, defn.RepeatedParamClass)))

  /** A denotation exists really if it exists and does not point to a stale symbol. */
  final def reallyExists(denot: Denotation)(implicit ctx: Context): Boolean = try
    denot match {
      case denot: SymDenotation =>
        denot.exists && !denot.isAbsent
      case denot: SingleDenotation =>
        val sym = denot.symbol
        (sym eq NoSymbol) || reallyExists(sym.denot)
      case _ =>
        true
    }
  catch {
    case ex: StaleSymbol => false
  }

  /** If `tpe` is a named type, check that its denotation is accessible in the
   *  current context. Return the type with those alternatives as denotations
   *  which are accessible.
   *
   *  Also performs the following normalizations on the type `tpe`.
   *  (1) parameter accessors are always dereferenced.
   *  (2) if the owner of the denotation is a package object, it is assured
   *      that the package object shows up as the prefix.
   */
  def ensureAccessible(tpe: Type, superAccess: Boolean, pos: Position)(implicit ctx: Context): Type = {
    def test(tpe: Type, firstTry: Boolean): Type = tpe match {
      case tpe: NamedType =>
        val pre = tpe.prefix
        val name = tpe.name
        val d = tpe.denot.accessibleFrom(pre, superAccess)
        if (!d.exists) {
          // it could be that we found an inaccessible private member, but there is
          // an inherited non-private member with the same name and signature.
          val d2 = pre.nonPrivateMember(name)
          if (reallyExists(d2) && firstTry)
            test(NamedType(pre, name, d2), false)
          else if (pre.derivesFrom(defn.DynamicClass)) {
            TryDynamicCallType
          } else {
            val alts = tpe.denot.alternatives.map(_.symbol).filter(_.exists)
            val what = alts match {
              case Nil =>
                name.toString
              case sym :: Nil =>
                if (sym.owner == pre.typeSymbol) sym.show else sym.showLocated
              case _ =>
                em"none of the overloaded alternatives named $name"
            }
            val where = if (ctx.owner.exists) s" from ${ctx.owner.enclosingClass}" else ""
            val whyNot = new StringBuffer
            alts foreach (_.isAccessibleFrom(pre, superAccess, whyNot))
            if (tpe.isError) tpe
            else errorType(ex"$what cannot be accessed as a member of $pre$where.$whyNot", pos)
          }
        }
        else ctx.makePackageObjPrefixExplicit(tpe withDenot d)
      case _ =>
        tpe
    }
    test(tpe, true)
  }

  /** The type of a selection with `name` of a tree with type `site`.
   */
  def selectionType(site: Type, name: Name, pos: Position)(implicit ctx: Context): Type = {
    val mbr = site.member(name)
    if (reallyExists(mbr)) site.select(name, mbr)
    else if (site.derivesFrom(defn.DynamicClass) && !Dynamic.isDynamicMethod(name)) {
      TryDynamicCallType
    } else {
      if (site.isErroneous || name.toTermName == nme.ERROR) UnspecifiedErrorType
      else {
        def kind = if (name.isTypeName) "type" else "value"
        def addendum =
          if (site.derivesFrom(defn.DynamicClass)) "\npossible cause: maybe a wrong Dynamic method signature?"
          else ""
        errorType(
          if (name == nme.CONSTRUCTOR) ex"$site does not have a constructor"
          else NotAMember(site, name, kind),
          pos)
      }
    }
  }

  /** The selection type, which is additionally checked for accessibility.
   */
  def accessibleSelectionType(tree: untpd.RefTree, qual1: Tree)(implicit ctx: Context): Type = {
    var qualType = qual1.tpe.widenIfUnstable
    if (qualType.isHK) qualType = errorType(em"$qualType takes type parameters", qual1.pos)
    val ownType = selectionType(qualType, tree.name, tree.pos)
    ensureAccessible(ownType, qual1.isInstanceOf[Super], tree.pos)
  }

  /** Type assignment method. Each method takes as parameters
   *   - an untpd.Tree to which it assigns a type,
   *   - typed child trees it needs to access to cpmpute that type,
   *   - any further information it needs to access to compute that type.
   */

  def assignType(tree: untpd.Ident, tp: Type)(implicit ctx: Context) =
    tree.withType(tp)

  def assignType(tree: untpd.Select, qual: Tree)(implicit ctx: Context): Select = {
    def qualType = qual.tpe.widen
    def arrayElemType = {
      val JavaArrayType(elemtp) = qualType
      elemtp
    }
    val p = nme.primitive
    val tp = tree.name match {
      case p.arrayApply => MethodType(defn.IntType :: Nil, arrayElemType)
      case p.arrayUpdate => MethodType(defn.IntType :: arrayElemType :: Nil, defn.UnitType)
      case p.arrayLength => MethodType(Nil, defn.IntType)

      // Note that we do not need to handle calls to Array[T]#clone() specially:
      // The JLS section 10.7 says "The return type of the clone method of an array type
      // T[] is T[]", but the actual return type at the bytecode level is Object which
      // is casted to T[] by javac. Since the return type of Array[T]#clone() is Array[T],
      // this is exactly what Erasure will do.

      case _ => accessibleSelectionType(tree, qual)
    }
    tree.withType(tp)
  }

  def assignType(tree: untpd.New, tpt: Tree)(implicit ctx: Context) =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.Literal)(implicit ctx: Context) =
    tree.withType {
      val value = tree.const
      value.tag match {
        case UnitTag => defn.UnitType
        case NullTag => defn.NullType
        case _ => if (ctx.erasedTypes) value.tpe else ConstantType(value)
      }
    }

  def assignType(tree: untpd.This)(implicit ctx: Context) = {
    val cls = qualifyingClass(tree, tree.qual.name, packageOK = false)
    tree.withType(
        if (cls.isClass) cls.thisType
        else errorType("not a legal qualifying class for this", tree.pos))
  }

  def assignType(tree: untpd.Super, qual: Tree, inConstrCall: Boolean, mixinClass: Symbol = NoSymbol)(implicit ctx: Context) = {
    val mix = tree.mix
    qual.tpe match {
      case err: ErrorType => untpd.cpy.Super(tree)(qual, mix).withType(err)
      case qtype @ ThisType(_) =>
        val cls = qtype.cls
        def findMixinSuper(site: Type): Type = site.parents filter (_.typeSymbol.name == mix.name) match {
          case p :: Nil =>
            p.typeConstructor
          case Nil =>
            errorType(SuperQualMustBeParent(mix, cls), tree.pos)
          case p :: q :: _ =>
            errorType("ambiguous parent class qualifier", tree.pos)
        }
        val owntype =
          if (mixinClass.exists) mixinClass.appliedRef
          else if (!mix.isEmpty) findMixinSuper(cls.info)
          else if (inConstrCall || ctx.erasedTypes) cls.info.firstParent.typeConstructor
          else {
            val ps = cls.classInfo.parents
            if (ps.isEmpty) defn.AnyType else ps.reduceLeft((x: Type, y: Type) => x & y)
          }
        tree.withType(SuperType(cls.thisType, owntype))
    }
  }

  /** Substitute argument type `argType` for parameter `pref` in type `tp`,
   *  skolemizing the argument type if it is not stable and `pref` occurs in `tp`.
   */
  def safeSubstParam(tp: Type, pref: ParamRef, argType: Type)(implicit ctx: Context) = {
    val tp1 = tp.substParam(pref, argType)
    if ((tp1 eq tp) || argType.isStable) tp1
    else tp.substParam(pref, SkolemType(argType.widen))
  }

  /** Substitute types of all arguments `args` for corresponding `params` in `tp`.
   *  The number of parameters `params` may exceed the number of arguments.
   *  In this case, only the common prefix is substituted.
   */
  def safeSubstParams(tp: Type, params: List[ParamRef], argTypes: List[Type])(implicit ctx: Context): Type = argTypes match {
    case argType :: argTypes1 =>
      val tp1 = safeSubstParam(tp, params.head, argType)
      safeSubstParams(tp1, params.tail, argTypes1)
    case Nil =>
      tp
    }

  def assignType(tree: untpd.Apply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    val ownType = fn.tpe.widen match {
      case fntpe: MethodType =>
        if (sameLength(fntpe.paramInfos, args) || ctx.phase.prev.relaxedTyping || true)
          if (fntpe.isDependent) safeSubstParams(fntpe.resultType, fntpe.paramRefs, args.tpes)
          else fntpe.resultType
        else
          errorType(i"wrong number of arguments at ${ctx.phase.prev} for $fntpe: ${fn.tpe}, expected: ${fntpe.paramInfos.length}, found: ${args.length}", tree.pos)
      case t =>
        errorType(err.takesNoParamsStr(fn, ""), tree.pos)
    }
    tree.withType(ownType)
  }

  def assignType(tree: untpd.TypeApply, fn: Tree, args: List[Tree])(implicit ctx: Context) = {
    val ownType = fn.tpe.widen match {
      case pt: TypeLambda =>
        val paramNames = pt.paramNames
        if (hasNamedArg(args)) {
          val paramBoundsByName = paramNames.zip(pt.paramInfos).toMap

          // Type arguments which are specified by name (immutable after this first loop)
          val namedArgMap = new mutable.HashMap[Name, Type]
          for (NamedArg(name, arg) <- args)
            if (namedArgMap.contains(name))
              ctx.error(DuplicateNamedTypeParameter(name), arg.pos)
            else if (!paramNames.contains(name))
              ctx.error(UndefinedNamedTypeParameter(name, paramNames), arg.pos)
            else
              namedArgMap(name) = arg.tpe

          // Holds indexes of non-named typed arguments in paramNames
          val gapBuf = new mutable.ListBuffer[Int]
          def nextPoly(idx: Int) = {
            val newIndex = gapBuf.length
            gapBuf += idx
            // Re-index unassigned type arguments that remain after transformation
            pt.paramRefs(newIndex)
          }

          // Type parameters after naming assignment, conserving paramNames order
          val normArgs: List[Type] = paramNames.zipWithIndex.map { case (pname, idx) =>
            namedArgMap.getOrElse(pname, nextPoly(idx))
          }

          val transform = new TypeMap {
            def apply(t: Type) = t match {
              case TypeParamRef(`pt`, idx) => normArgs(idx)
              case _ => mapOver(t)
            }
          }
          val resultType1 = transform(pt.resultType)
          if (gapBuf.isEmpty) resultType1
          else {
            val gaps = gapBuf.toList
            pt.derivedLambdaType(
              gaps.map(paramNames),
              gaps.map(idx => transform(pt.paramInfos(idx)).bounds),
              resultType1)
          }
        }
        else {
          val argTypes = args.tpes
          if (sameLength(argTypes, paramNames)) pt.instantiate(argTypes)
          else wrongNumberOfTypeArgs(fn.tpe, pt.typeParams, args, tree.pos)
        }
      case _ =>
        //println(i"bad type: $fn: ${fn.symbol} / ${fn.symbol.isType} / ${fn.symbol.info}") // DEBUG
        errorType(err.takesNoParamsStr(fn, "type "), tree.pos)
    }

    tree.withType(ownType)
  }

  def assignType(tree: untpd.Typed, tpt: Tree)(implicit ctx: Context) =
    tree.withType(tpt.tpe)

  def assignType(tree: untpd.NamedArg, arg: Tree)(implicit ctx: Context) =
    tree.withType(arg.tpe)

  def assignType(tree: untpd.Assign)(implicit ctx: Context) =
    tree.withType(defn.UnitType)

  def assignType(tree: untpd.Block, stats: List[Tree], expr: Tree)(implicit ctx: Context) =
    tree.withType(avoidingType(expr, stats))

  def assignType(tree: untpd.Inlined, bindings: List[Tree], expansion: Tree)(implicit ctx: Context) =
    tree.withType(avoidingType(expansion, bindings))

  def assignType(tree: untpd.If, thenp: Tree, elsep: Tree)(implicit ctx: Context) =
    tree.withType(lubInSameUniverse(thenp :: elsep :: Nil, "branches of an if/else"))

  def assignType(tree: untpd.Closure, meth: Tree, target: Tree)(implicit ctx: Context) =
    tree.withType(
      if (target.isEmpty) meth.tpe.widen.toFunctionType(tree.env.length)
      else target.tpe)

  def assignType(tree: untpd.CaseDef, body: Tree)(implicit ctx: Context) =
    tree.withType(body.tpe)

  def assignType(tree: untpd.Match, cases: List[CaseDef])(implicit ctx: Context) = {
    if (tree.selector.typeOpt.isPhantom)
      ctx.error("cannot pattern match on values of a phantom type", tree.selector.pos)
    tree.withType(lubInSameUniverse(cases, "branches of a match"))
  }

  def assignType(tree: untpd.Return)(implicit ctx: Context) =
    tree.withType(defn.NothingType)

  def assignType(tree: untpd.Try, expr: Tree, cases: List[CaseDef])(implicit ctx: Context) =
    if (cases.isEmpty) tree.withType(expr.tpe)
    else tree.withType(lubInSameUniverse(expr :: cases, "branches of a try"))

  def assignType(tree: untpd.SeqLiteral, elems: List[Tree], elemtpt: Tree)(implicit ctx: Context) = {
    val ownType = tree match {
      case tree: untpd.JavaSeqLiteral => defn.ArrayOf(elemtpt.tpe)
      case _ => if (ctx.erasedTypes) defn.SeqType else defn.SeqType.appliedTo(elemtpt.tpe)
    }
    tree.withType(ownType)
  }

  def assignType(tree: untpd.SingletonTypeTree, ref: Tree)(implicit ctx: Context) =
    tree.withType(ref.tpe)

  def assignType(tree: untpd.AndTypeTree, left: Tree, right: Tree)(implicit ctx: Context) =
    tree.withType(inSameUniverse(AndType(_, _), left.tpe, right, "an `&`"))

  def assignType(tree: untpd.OrTypeTree, left: Tree, right: Tree)(implicit ctx: Context) =
    tree.withType(inSameUniverse(OrType(_, _), left.tpe, right, "an `|`"))

  /** Assign type of RefinedType.
   *  Refinements are typed as if they were members of refinement class `refineCls`.
   */
  def assignType(tree: untpd.RefinedTypeTree, parent: Tree, refinements: List[Tree], refineCls: ClassSymbol)(implicit ctx: Context) = {
    def addRefinement(parent: Type, refinement: Tree): Type = {
      val rsym = refinement.symbol
      val rinfo = if (rsym is Accessor) rsym.info.resultType else rsym.info
      if (rinfo.isError) rinfo
      else if (!rinfo.exists) parent // can happen after failure in self type definition
      else RefinedType(parent, rsym.name, rinfo)
    }
    val refined = (parent.tpe /: refinements)(addRefinement)
    tree.withType(RecType.closeOver(rt => refined.substThis(refineCls, rt.recThis)))
  }

  def assignType(tree: untpd.AppliedTypeTree, tycon: Tree, args: List[Tree])(implicit ctx: Context) = {
    assert(!hasNamedArg(args))
    val tparams = tycon.tpe.typeParams
    val ownType =
      if (sameLength(tparams, args)) tycon.tpe.appliedTo(args.tpes)
      else wrongNumberOfTypeArgs(tycon.tpe, tparams, args, tree.pos)
    tree.withType(ownType)
  }

  def assignType(tree: untpd.LambdaTypeTree, tparamDefs: List[TypeDef], body: Tree)(implicit ctx: Context) =
    tree.withType(HKTypeLambda.fromParams(tparamDefs.map(_.symbol.asType), body.tpe))

  def assignType(tree: untpd.ByNameTypeTree, result: Tree)(implicit ctx: Context) =
    tree.withType(ExprType(result.tpe))

  def assignType(tree: untpd.TypeBoundsTree, lo: Tree, hi: Tree)(implicit ctx: Context) =
    tree.withType(
        if (lo eq hi) TypeAlias(lo.tpe)
        else inSameUniverse(TypeBounds(_, _), lo.tpe, hi, "type bounds"))

  def assignType(tree: untpd.Bind, sym: Symbol)(implicit ctx: Context) =
    tree.withType(NamedType(NoPrefix, sym))

  def assignType(tree: untpd.Alternative, trees: List[Tree])(implicit ctx: Context) =
    tree.withType(ctx.typeComparer.lub(trees.tpes))

  def assignType(tree: untpd.UnApply, proto: Type)(implicit ctx: Context) =
    tree.withType(proto)

  def assignType(tree: untpd.ValDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(if (sym.exists) assertExists(sym.termRef) else NoType)

  def assignType(tree: untpd.DefDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.TypeDef, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.typeRef)

  def assertExists(tp: Type) = { assert(tp != NoType); tp }

  def assignType(tree: untpd.Import, sym: Symbol)(implicit ctx: Context) =
    tree.withType(sym.termRef)

  def assignType(tree: untpd.Annotated, arg: Tree, annot: Tree)(implicit ctx: Context) =
    tree.withType(AnnotatedType(arg.tpe.widen, Annotation(annot)))

  def assignType(tree: untpd.PackageDef, pid: Tree)(implicit ctx: Context) =
    tree.withType(pid.symbol.termRef)

  /** Ensure that `tree2`'s type is in the same universe as `tree1`. If that's the case, return
   *  `op` applied to both types.
   *  If not, issue an error and return `tree1`'s type.
   */
  private def inSameUniverse(op: (Type, Type) => Type, tp1: Type, tree2: Tree, relationship: => String)(implicit ctx: Context): Type =
    if (tp1.topType == tree2.tpe.topType)
      op(tp1, tree2.tpe)
    else {
      ctx.error(ex"$tp1 and ${tree2.tpe} are in different universes. They cannot be combined in $relationship", tree2.pos)
      tp1
    }

  private def lubInSameUniverse(trees: List[Tree], relationship: => String)(implicit ctx: Context): Type =
    trees match {
      case first :: rest => (first.tpe /: rest)(inSameUniverse(_ | _, _, _, relationship))
      case Nil => defn.NothingType
    }
}

object TypeAssigner extends TypeAssigner

