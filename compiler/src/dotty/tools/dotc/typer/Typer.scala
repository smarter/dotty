package dotty.tools
package dotc
package typer

import core._
import ast.{tpd, _}
import Trees._
import Constants._
import StdNames._
import Scopes._
import Denotations._
import ProtoTypes._
import Contexts._
import Comments._
import Symbols._
import Types._
import SymDenotations._
import Annotations._
import Names._
import NameOps._
import NameKinds._
import Flags._
import Decorators._
import ErrorReporting._
import Checking._
import Inferencing._
import EtaExpansion.etaExpand
import util.Positions._
import util.common._
import util.{Property, SourcePosition}

import collection.mutable
import annotation.tailrec
import Implicits._
import util.Stats.{record, track}
import config.Printers.{gadts, typr}
import rewrite.Rewrites.patch
import NavigateAST._
import transform.SymUtils._
import reporting.trace
import config.Config

import language.implicitConversions
import printing.SyntaxHighlighting._

object Typer {

  /** The precedence of bindings which determines which of several bindings will be
   *  accessed by an Ident.
   */
  object BindingPrec {
    val definition = 4
    val namedImport = 3
    val wildImport = 2
    val packageClause = 1
    val nothingBound = 0
    def isImportPrec(prec: Int) = prec == namedImport || prec == wildImport
  }

  /** Assert tree has a position, unless it is empty or a typed splice */
  def assertPositioned(tree: untpd.Tree)(implicit ctx: Context) =
    if (!tree.isEmpty && !tree.isInstanceOf[untpd.TypedSplice] && ctx.typerState.isGlobalCommittable)
      assert(tree.pos.exists, s"position not set for $tree # ${tree.uniqueId}")

  /** A context property that indicates the owner of any expressions to be typed in the context
   *  if that owner is different from the context's owner. Typically, a context with a class
   *  as owner would have a local dummy as ExprOwner value.
   */
  private val ExprOwner = new Property.Key[Symbol]

  /** An attachment on a Select node with an `apply` field indicating that the `apply`
   *  was inserted by the Typer.
   */
  private val InsertedApply = new Property.Key[Unit]

  /** An attachment on a tree `t` occurring as part of a `t()` where
   *  the `()` was dropped by the Typer.
   */
  private val DroppedEmptyArgs = new Property.Key[Unit]
}

class Typer extends Namer
               with TypeAssigner
               with Applications
               with Implicits
               with Inferencing
               with Dynamic
               with Checking
               with Docstrings {

  import Typer._
  import tpd.{cpy => _, _}
  import untpd.cpy
  import Dynamic.isDynamicMethod
  import reporting.diagnostic.Message
  import reporting.diagnostic.messages._

  /** A temporary data item valid for a single typed ident:
   *  The set of all root import symbols that have been
   *  encountered as a qualifier of an import so far.
   *  Note: It would be more proper to move importedFromRoot into typedIdent.
   *  We should check that this has no performance degradation, however.
   */
  private[this] var unimported: Set[Symbol] = Set()

  /** Temporary data item for single call to typed ident:
   *  This symbol would be found under Scala2 mode, but is not
   *  in dotty (because dotty conforms to spec section 2
   *  wrt to package member resolution but scalac doe not).
   */
  private[this] var foundUnderScala2: Type = NoType

  def newLikeThis: Typer = new Typer

  /** Attribute an identifier consisting of a simple name or wildcard
   *
   *  @param tree      The tree representing the identifier.
   *  Transformations: (1) Prefix class members with this.
   *                   (2) Change imported symbols to selections.
   *                   (3) Change pattern Idents id (but not wildcards) to id @ _
   */
  def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = track("typedIdent") {
    val refctx = ctx
    val name = tree.name
    val noImports = ctx.mode.is(Mode.InPackageClauseName)

    /** Method is necessary because error messages need to bind to
     *  to typedIdent's context which is lost in nested calls to findRef
     */
    def error(msg: => Message, pos: Position) = ctx.error(msg, pos)

    /** Does this identifier appear as a constructor of a pattern? */
    def isPatternConstr =
      if (ctx.mode.isExpr && (ctx.outer.mode is Mode.Pattern))
        ctx.outer.tree match {
          case Apply(`tree`, _) => true
          case _ => false
        }
      else false

    /** A symbol qualifies if it really exists and is not a package class.
     *  In addition, if we are in a constructor of a pattern, we ignore all definitions
     *  which are methods and not accessors (note: if we don't do that
     *  case x :: xs in class List would return the :: method).
     *
     *  Package classes are part of their parent's scope, because otherwise
     *  we could not reload them via `_.member`. On the other hand, accessing a
     *  package as a type from source is always an error.
     */
    def qualifies(denot: Denotation): Boolean =
      reallyExists(denot) &&
        !(pt.isInstanceOf[UnapplySelectionProto] &&
          (denot.symbol is (Method, butNot = Accessor))) &&
        !(denot.symbol is PackageClass)

    /** Find the denotation of enclosing `name` in given context `ctx`.
     *  @param previous    A denotation that was found in a more deeply nested scope,
     *                     or else `NoDenotation` if nothing was found yet.
     *  @param prevPrec    The binding precedence of the previous denotation,
     *                     or else `nothingBound` if nothing was found yet.
     *  @param prevCtx     The context of the previous denotation,
     *                     or else `NoContext` if nothing was found yet.
     */
    def findRef(previous: Type, prevPrec: Int, prevCtx: Context)(implicit ctx: Context): Type = {
      import BindingPrec._

      /** Check that any previously found result from an inner context
       *  does properly shadow the new one from an outer context.
       *  @param found     The newly found result
       *  @param newPrec   Its precedence
       *  @param scala2pkg Special mode where we check members of the same package, but defined
       *                   in different compilation units under Scala2. If set, and the
       *                   previous and new contexts do not have the same scope, we select
       *                   the previous (inner) definition. This models what scalac does.
       */
      def checkNewOrShadowed(found: Type, newPrec: Int, scala2pkg: Boolean = false)(implicit ctx: Context): Type =
        if (!previous.exists || ctx.typeComparer.isSameRef(previous, found)) found
        else if ((prevCtx.scope eq ctx.scope) &&
                 (newPrec == definition ||
                  newPrec == namedImport && prevPrec == wildImport)) {
          // special cases: definitions beat imports, and named imports beat
          // wildcard imports, provided both are in contexts with same scope
          found
        }
        else {
          if (!scala2pkg && !previous.isError && !found.isError) {
            error(AmbiguousImport(name, newPrec, prevPrec, prevCtx), tree.pos)
          }
          previous
        }

      def selection(imp: ImportInfo, name: Name) =
        if (imp.sym.isCompleting) {
          ctx.warning(i"cyclic ${imp.sym}, ignored", tree.pos)
          NoType
        } else if (unimported.nonEmpty && unimported.contains(imp.site.termSymbol))
          NoType
        else {
          val pre = imp.site
          val denot = pre.member(name).accessibleFrom(pre)(refctx)
            // Pass refctx so that any errors are reported in the context of the
            // reference instead of the
          if (reallyExists(denot)) pre.select(name, denot) else NoType
        }

      /** The type representing a named import with enclosing name when imported
       *  from given `site` and `selectors`.
       */
      def namedImportRef(imp: ImportInfo)(implicit ctx: Context): Type = {
        val Name = name.toTermName
        def recur(selectors: List[untpd.Tree]): Type = selectors match {
          case selector :: rest =>
            def checkUnambiguous(found: Type) = {
              val other = recur(selectors.tail)
              if (other.exists && found.exists && (found != other))
                error(em"reference to `$name` is ambiguous; it is imported twice in ${ctx.tree}",
                      tree.pos)
              found
            }

            def unambiguousSelection(name: Name) =
              checkUnambiguous(selection(imp, name))

            selector match {
              case Thicket(fromId :: Ident(Name) :: _) =>
                val Ident(from) = fromId
                unambiguousSelection(if (name.isTypeName) from.toTypeName else from)
              case Ident(Name) =>
                unambiguousSelection(name)
              case _ =>
                recur(rest)
            }
          case nil =>
            NoType
        }
        recur(imp.selectors)
      }

      /** The type representing a wildcard import with enclosing name when imported
       *  from given import info
       */
      def wildImportRef(imp: ImportInfo)(implicit ctx: Context): Type =
        if (imp.isWildcardImport && !imp.excluded.contains(name.toTermName) && name != nme.CONSTRUCTOR)
          selection(imp, name)
        else NoType

      /** Is (some alternative of) the given predenotation `denot`
       *  defined in current compilation unit?
       */
      def isDefinedInCurrentUnit(denot: Denotation)(implicit ctx: Context): Boolean = denot match {
        case MultiDenotation(d1, d2) => isDefinedInCurrentUnit(d1) || isDefinedInCurrentUnit(d2)
        case denot: SingleDenotation => denot.symbol.sourceFile == ctx.source.file
      }

      /** Is `denot` the denotation of a self symbol? */
      def isSelfDenot(denot: Denotation)(implicit ctx: Context) = denot match {
        case denot: SymDenotation => denot is SelfName
        case _ => false
      }

      /** Would import of kind `prec` be not shadowed by a nested higher-precedence definition? */
      def isPossibleImport(prec: Int)(implicit ctx: Context) =
        !noImports &&
        (prevPrec < prec || prevPrec == prec && (prevCtx.scope eq ctx.scope))

      @tailrec def loop(lastCtx: Context)(implicit ctx: Context): Type = {
        if (ctx.scope == null) previous
        else {
          var result: Type = NoType

          val curOwner = ctx.owner

          // Can this scope contain new definitions? This is usually the first
          // context where either the scope or the owner changes wrt the
          // context immediately nested in it. But for package contexts, it's
          // the opposite: the last context before the package changes. This distinction
          // is made so that top-level imports following a package clause are
          // logically nested in that package clause. Finally, for the root package
          // we switch back to the original test. This means that rop-level packages in
          // the root package take priority over root imports. For instance,
          // a top-level io package takes priority over scala.io.
          // It would be nice if we could drop all this complication, and
          // always use the second condition. Unfortunately compileStdLib breaks
          // with an error on CI which I cannot replicate locally (not even
          // with the exact list of files given).
          val isNewDefScope =
            if (curOwner.is(Package) && !curOwner.isRoot) curOwner ne ctx.outer.owner
            else (ctx.scope ne lastCtx.scope) || (curOwner ne lastCtx.owner)

          if (isNewDefScope) {
            val defDenot = ctx.denotNamed(name)
            if (qualifies(defDenot)) {
              val found =
                if (isSelfDenot(defDenot)) curOwner.enclosingClass.thisType
                else {
                  val effectiveOwner =
                    if (curOwner.isTerm && defDenot.symbol.isType)
                      // Don't mix NoPrefix and thisType prefixes, since type comparer
                      // would not detect types to be compatible. Note: If we replace the
                      // 2nd condition by `defDenot.symbol.maybeOwner.isType` we get lots
                      // of failures in the `tastyBootstrap` test. Trying to compile these
                      // files in isolation works though.
                      // TODO: Investigate why that happens.
                      defDenot.symbol.owner
                    else
                      curOwner
                  effectiveOwner.thisType.select(name, defDenot)
                }
              if (!(curOwner is Package) || isDefinedInCurrentUnit(defDenot))
                result = checkNewOrShadowed(found, definition) // no need to go further out, we found highest prec entry
              else {
                if (ctx.scala2Mode && !foundUnderScala2.exists)
                  foundUnderScala2 = checkNewOrShadowed(found, definition, scala2pkg = true)
                if (defDenot.symbol is Package)
                  result = checkNewOrShadowed(previous orElse found, packageClause)
                else if (prevPrec < packageClause)
                  result = findRef(found, packageClause, ctx)(ctx.outer)
              }
            }
          }

          if (result.exists) result
          else {  // find import
            val outer = ctx.outer
            val curImport = ctx.importInfo
            def updateUnimported() =
              if (curImport.unimported.exists) unimported += curImport.unimported
            if (ctx.owner.is(Package) && curImport != null && curImport.isRootImport && previous.exists)
              previous // no more conflicts possible in this case
            else if (isPossibleImport(namedImport) && (curImport ne outer.importInfo)) {
              val namedImp = namedImportRef(curImport)
              if (namedImp.exists)
                findRef(checkNewOrShadowed(namedImp, namedImport), namedImport, ctx)(outer)
              else if (isPossibleImport(wildImport) && !curImport.sym.isCompleting) {
                val wildImp = wildImportRef(curImport)
                if (wildImp.exists)
                  findRef(checkNewOrShadowed(wildImp, wildImport), wildImport, ctx)(outer)
                else {
                  updateUnimported()
                  loop(ctx)(outer)
                }
              }
              else {
                updateUnimported()
                loop(ctx)(outer)
              }
            }
            else loop(ctx)(outer)
          }
        }
      }

      // begin findRef
      loop(NoContext)(ctx)
    }

    // begin typedIdent
    def kind = if (name.isTermName) "" else "type "
    typr.println(s"typed ident $kind$name in ${ctx.owner}")
    if (ctx.mode is Mode.Pattern) {
      if (name == nme.WILDCARD)
        return tree.withType(pt)
      if (untpd.isVarPattern(tree) && name.isTermName)
        return typed(desugar.patternVar(tree), pt)
    }

    val rawType = {
      val saved1 = unimported
      val saved2 = foundUnderScala2
      unimported = Set.empty
      foundUnderScala2 = NoType
      try {
        var found = findRef(NoType, BindingPrec.nothingBound, NoContext)
        if (foundUnderScala2.exists && !(foundUnderScala2 =:= found)) {
          ctx.migrationWarning(
            ex"""Name resolution will change.
              | currently selected                     : $foundUnderScala2
              | in the future, without -language:Scala2: $found""", tree.pos)
          found = foundUnderScala2
        }
        found
      }
      finally {
      	unimported = saved1
      	foundUnderScala2 = saved2
      }
    }

    val ownType =
      if (rawType.exists)
        ensureAccessible(rawType, superAccess = false, tree.pos)
      else if (name == nme._scope) {
        // gross hack to support current xml literals.
        // awaiting a better implicits based solution for library-supported xml
        return ref(defn.XMLTopScopeModuleRef)
      }
      else if (name.toTermName == nme.ERROR)
        UnspecifiedErrorType
      else if (ctx.owner.isConstructor && ctx.mode.is(Mode.InSuperCall) &&
          ctx.owner.owner.unforcedDecls.lookup(tree.name).exists) {
        // When InSuperCall mode and in a constructor we are in the arguments
        // of a this(...) constructor call
        errorType(ex"$tree is not accessible from constructor arguments", tree.pos)
      } else
        errorType(new MissingIdent(tree, kind, name.show), tree.pos)

    val tree1 = ownType match {
      case ownType: NamedType if !prefixIsElidable(ownType) =>
        ref(ownType).withPos(tree.pos)
      case _ =>
        tree.withType(ownType)
    }

    checkStableIdentPattern(tree1, pt)
    checkValue(tree1, pt)
  }

  /** Check that a stable identifier pattern is indeed stable (SLS 8.1.5)
   */
  private def checkStableIdentPattern(tree: Tree, pt: Type)(implicit ctx: Context): tree.type = {
    if (ctx.mode.is(Mode.Pattern) &&
        !tree.isType &&
        !pt.isInstanceOf[ApplyingProto] &&
        !tree.tpe.isStable &&
        !isWildcardArg(tree))
      ctx.error(s"stable identifier required, but ${tree.show} found", tree.pos)

    tree
  }

  private def typedSelect(tree: untpd.Select, pt: Type, qual: Tree)(implicit ctx: Context): Select =
    checkValue(assignType(cpy.Select(tree)(qual, tree.name), qual), pt)

  def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = track("typedSelect") {

    def typeSelectOnTerm(implicit ctx: Context): Tree = {
      val qual1 = typedExpr(tree.qualifier, selectionProto(tree.name, pt, this))
      if (tree.name.isTypeName) checkStable(qual1.tpe, qual1.pos)
      val select = typedSelect(tree, pt, qual1)
      if (select.tpe ne TryDynamicCallType) ConstFold(checkStableIdentPattern(select, pt))
      else if (pt.isInstanceOf[PolyProto] || pt.isInstanceOf[FunProto] || pt == AssignProto) select
      else typedDynamicSelect(tree, Nil, pt)
    }

    def typeSelectOnType(qual: untpd.Tree)(implicit ctx: Context) =
      typedSelect(untpd.cpy.Select(tree)(qual, tree.name.toTypeName), pt)

    def tryJavaSelectOnType(implicit ctx: Context): Tree = tree.qualifier match {
      case Select(qual, name) => typeSelectOnType(untpd.Select(qual, name.toTypeName))
      case Ident(name)        => typeSelectOnType(untpd.Ident(name.toTypeName))
      case _                  => errorTree(tree, "cannot convert to type selection") // will never be printed due to fallback
    }

    def selectWithFallback(fallBack: Context => Tree) =
      tryAlternatively(typeSelectOnTerm(_))(fallBack)

    if (tree.qualifier.isType) {
      val qual1 = typedType(tree.qualifier, selectionProto(tree.name, pt, this))
      assignType(cpy.Select(tree)(qual1, tree.name), qual1)
    }
    else if (ctx.compilationUnit.isJava && tree.name.isTypeName)
      // SI-3120 Java uses the same syntax, A.B, to express selection from the
      // value A and from the type A. We have to try both.
      selectWithFallback(tryJavaSelectOnType(_)) // !!! possibly exponential bcs of qualifier retyping
    else
      typeSelectOnTerm(ctx)
  }

  def typedThis(tree: untpd.This)(implicit ctx: Context): Tree = track("typedThis") {
    assignType(tree)
  }

  def typedSuper(tree: untpd.Super, pt: Type)(implicit ctx: Context): Tree = track("typedSuper") {
    val qual1 = typed(tree.qual)
    val inConstrCall = pt match {
      case pt: SelectionProto if pt.name == nme.CONSTRUCTOR => true
      case _ => false
    }
    val enclosingInlineable = ctx.owner.ownersIterator.findSymbol(_.isInlineableMethod)
    if (enclosingInlineable.exists && !Inliner.isLocal(qual1.symbol, enclosingInlineable))
      ctx.error(SuperCallsNotAllowedInline(enclosingInlineable), tree.pos)
    pt match {
      case pt: SelectionProto if pt.name.isTypeName =>
        qual1 // don't do super references for types; they are meaningless anyway
      case _ =>
        assignType(cpy.Super(tree)(qual1, tree.mix), qual1, inConstrCall)
    }
  }

  def typedLiteral(tree: untpd.Literal)(implicit ctx: Context): Tree = track("typedLiteral") {
    assignType(tree)
  }

  def typedNew(tree: untpd.New, pt: Type)(implicit ctx: Context) = track("typedNew") {
    tree.tpt match {
      case templ: untpd.Template =>
        import untpd._
        var templ1 = templ
        def isEligible(tp: Type) = tp.exists && !tp.typeSymbol.is(Final)
        if (templ1.parents.isEmpty &&
            isFullyDefined(pt, ForceDegree.noBottom) &&
            isEligible(pt.underlyingClassRef(refinementOK = false)))
          templ1 = cpy.Template(templ)(parents = untpd.TypeTree(pt) :: Nil) // widenIfUnstable ?
        templ1.parents foreach {
          case parent: RefTree =>
            typedAheadImpl(parent, tree => inferTypeParams(typedType(tree), pt))
          case _ =>
        }
        val x = tpnme.ANON_CLASS
        val clsDef = TypeDef(x, templ1).withFlags(Final)
        typed(cpy.Block(tree)(clsDef :: Nil, New(Ident(x), Nil)), pt)
      case _ =>
        var tpt1 = typedType(tree.tpt)
        tpt1 = tpt1.withType(ensureAccessible(tpt1.tpe, superAccess = false, tpt1.pos))
        tpt1.tpe.dealias match {
          case TypeApplications.EtaExpansion(tycon) => tpt1 = tpt1.withType(tycon)
          case _ =>
        }
        checkClassType(tpt1.tpe, tpt1.pos, traitReq = false, stablePrefixReq = true)

        tpt1 match {
          case AppliedTypeTree(_, targs) =>
            for (targ @ TypeBoundsTree(_, _) <- targs)
              ctx.error(WildcardOnTypeArgumentNotAllowedOnNew(), targ.pos)
          case _ =>
        }

        assignType(cpy.New(tree)(tpt1), tpt1)
        // todo in a later phase: checkInstantiatable(cls, tpt1.pos)
    }
  }

  def typedTyped(tree: untpd.Typed, pt: Type)(implicit ctx: Context): Tree = track("typedTyped") {
    /*  Handles three cases:
     *  @param  ifPat    how to handle a pattern (_: T)
     *  @param  ifExpr   how to handle an expression (e: T)
     *  @param  wildName what name `w` to use in the rewriting of
     *                   (x: T) to (x @ (w: T)). This is either `_` or `_*`.
     */
    def cases(ifPat: => Tree, ifExpr: => Tree, wildName: TermName) = tree.expr match {
      case id: untpd.Ident if (ctx.mode is Mode.Pattern) && untpd.isVarPattern(id) =>
        if (id.name == nme.WILDCARD || id.name == nme.WILDCARD_STAR) ifPat
        else {
          import untpd._
          typed(Bind(id.name, Typed(Ident(wildName), tree.tpt)).withPos(tree.pos), pt)
        }
      case _ => ifExpr
    }
    def ascription(tpt: Tree, isWildcard: Boolean) = {
      val underlyingTreeTpe =
        if (isRepeatedParamType(tpt)) TypeTree(defn.SeqType.appliedTo(pt :: Nil))
        else tpt

      val expr1 =
        if (isRepeatedParamType(tpt)) tree.expr.withType(defn.SeqType.appliedTo(pt :: Nil))
        else if (isWildcard) tree.expr.withType(tpt.tpe)
        else typed(tree.expr, tpt.tpe.widenSkolem)
      assignType(cpy.Typed(tree)(expr1, tpt), underlyingTreeTpe)
    }
    if (untpd.isWildcardStarArg(tree))
      cases(
        ifPat = ascription(TypeTree(defn.RepeatedParamType.appliedTo(pt)), isWildcard = true),
        ifExpr = seqToRepeated(typedExpr(tree.expr, defn.SeqType.appliedTo(defn.AnyType))),
        wildName = nme.WILDCARD_STAR)
    else {
      def typedTpt = checkSimpleKinded(typedType(tree.tpt))
      def handlePattern: Tree = {
        val tpt1 = typedTpt
        if (!ctx.isAfterTyper) constrainPatternType(tpt1.tpe, pt)(ctx.addMode(Mode.GADTflexible))
        // special case for an abstract type that comes with a class tag
        tryWithClassTag(ascription(tpt1, isWildcard = true), pt)
      }
      cases(
        ifPat = handlePattern,
        ifExpr = ascription(typedTpt, isWildcard = false),
        wildName = nme.WILDCARD)
    }
  }

  /** For a typed tree `e: T`, if `T` is an abstract type for which an implicit class tag `ctag`
   *  exists, rewrite to `ctag(e)`.
   *  @pre We are in pattern-matching mode (Mode.Pattern)
   */
  def tryWithClassTag(tree: Typed, pt: Type)(implicit ctx: Context) = tree.tpt.tpe.dealias match {
    case tref: TypeRef if !tref.symbol.isClass && !ctx.isAfterTyper =>
      require(ctx.mode.is(Mode.Pattern))
      inferImplicit(defn.ClassTagType.appliedTo(tref),
                    EmptyTree, tree.tpt.pos)(ctx.retractMode(Mode.Pattern)) match {
        case SearchSuccess(clsTag, _, _) =>
          typed(untpd.Apply(untpd.TypedSplice(clsTag), untpd.TypedSplice(tree.expr)), pt)
        case _ =>
          tree
      }
    case _ => tree
  }

  def typedNamedArg(tree: untpd.NamedArg, pt: Type)(implicit ctx: Context) = track("typedNamedArg") {
    val arg1 = typed(tree.arg, pt)
    assignType(cpy.NamedArg(tree)(tree.name, arg1), arg1)
  }

  def typedAssign(tree: untpd.Assign, pt: Type)(implicit ctx: Context) = track("typedAssign") {
    tree.lhs match {
      case lhs @ Apply(fn, args) =>
        typed(untpd.Apply(untpd.Select(fn, nme.update), args :+ tree.rhs), pt)
      case untpd.TypedSplice(Apply(MaybePoly(Select(fn, app), targs), args)) if app == nme.apply =>
        val rawUpdate: untpd.Tree = untpd.Select(untpd.TypedSplice(fn), nme.update)
        val wrappedUpdate =
          if (targs.isEmpty) rawUpdate
          else untpd.TypeApply(rawUpdate, targs map (untpd.TypedSplice(_)))
        val appliedUpdate =
          untpd.Apply(wrappedUpdate, (args map (untpd.TypedSplice(_))) :+ tree.rhs)
        typed(appliedUpdate, pt)
      case lhs =>
        val locked = ctx.typerState.ownedVars
        val lhsCore = typedUnadapted(lhs, AssignProto, locked)
        def lhs1 = typed(untpd.TypedSplice(lhsCore), WildcardType, locked)

        def reassignmentToVal =
          errorTree(cpy.Assign(tree)(lhsCore, typed(tree.rhs, lhs1.tpe.widen)),
            ReassignmentToVal(lhsCore.symbol.name))

        def canAssign(sym: Symbol) =
          sym.is(Mutable, butNot = Accessor) ||
          ctx.owner.isPrimaryConstructor && !sym.is(Method) && sym.owner == ctx.owner.owner ||
            // allow assignments from the primary constructor to class fields
          ctx.owner.name.is(TraitSetterName) || ctx.owner.isStaticConstructor

        lhsCore.tpe match {
          case ref: TermRef =>
            val lhsVal = lhsCore.denot.suchThat(!_.is(Method))
            if (canAssign(lhsVal.symbol)) {
              // lhsBounds: (T .. Any) as seen from lhs prefix, where T is the type of lhsVal.symbol
              // This ensures we do the as-seen-from on T with variance -1. Test case neg/i2928.scala
              val lhsBounds =
                TypeBounds.lower(lhsVal.symbol.info).asSeenFrom(ref.prefix, lhsVal.symbol.owner)
              assignType(cpy.Assign(tree)(lhs1, typed(tree.rhs, lhsBounds.loBound)))
            }
            else {
              val pre = ref.prefix
              val setterName = ref.name.setterName
              val setter = pre.member(setterName)
              lhsCore match {
                case lhsCore: RefTree if setter.exists =>
                  val setterTypeRaw = pre.select(setterName, setter)
                  val setterType = ensureAccessible(setterTypeRaw, isSuperSelection(lhsCore), tree.pos)
                  val lhs2 = untpd.rename(lhsCore, setterName).withType(setterType)
                  typedUnadapted(untpd.Apply(untpd.TypedSplice(lhs2), tree.rhs :: Nil), WildcardType, locked)
                case _ =>
                  reassignmentToVal
              }
            }
          case TryDynamicCallType =>
            typedDynamicAssign(tree, pt)
          case tpe =>
            reassignmentToVal
        }
    }
  }

  def typedBlockStats(stats: List[untpd.Tree])(implicit ctx: Context): (Context, List[tpd.Tree]) =
    (indexAndAnnotate(stats), typedStats(stats, ctx.owner))

  def typedBlock(tree: untpd.Block, pt: Type)(implicit ctx: Context) = track("typedBlock") {
    val (exprCtx, stats1) = typedBlockStats(tree.stats)
    val expr1 = typedExpr(tree.expr, pt.notApplied)(exprCtx)
    ensureNoLocalRefs(
      cpy.Block(tree)(stats1, expr1).withType(expr1.tpe), pt, localSyms(stats1))
  }

  def escapingRefs(block: Tree, localSyms: => List[Symbol])(implicit ctx: Context): collection.Set[NamedType] = {
    lazy val locals = localSyms.toSet
    block.tpe namedPartsWith (tp => locals.contains(tp.symbol))
  }

  /** Ensure that an expression's type can be expressed without references to locally defined
   *  symbols. This is done by adding a type ascription of a widened type that does
   *  not refer to the locally defined symbols. The widened type is computed using
   *  `TyperAssigner#avoid`. However, if the expected type is fully defined and not
   *  a supertype of the widened type, we ascribe with the expected type instead.
   *
   *  There's a special case having to do with anonymous classes. Sometimes the
   *  expected type of a block is the anonymous class defined inside it. In that
   *  case there's technically a leak which is not removed by the ascription.
   */
  protected def ensureNoLocalRefs(tree: Tree, pt: Type, localSyms: => List[Symbol])(implicit ctx: Context): Tree = {
    def ascribeType(tree: Tree, pt: Type): Tree = tree match {
      case block @ Block(stats, expr) =>
        val expr1 = ascribeType(expr, pt)
        cpy.Block(block)(stats, expr1) withType expr1.tpe // no assignType here because avoid is redundant
      case _ =>
        Typed(tree, TypeTree(pt.simplified))
    }
    def noLeaks(t: Tree): Boolean = escapingRefs(t, localSyms).isEmpty
    if (noLeaks(tree)) tree
    else {
      fullyDefinedType(tree.tpe, "block", tree.pos)
      var avoidingType = avoid(tree.tpe, localSyms)
      val ptDefined = isFullyDefined(pt, ForceDegree.none)
      val isAnonClass = tree match {
        case Block(_, expr) =>
          expr.symbol.isConstructor && expr.symbol.owner.name == tpnme.ANON_CLASS
        case _ =>
          false
      }
      val matches = avoidingType <:< pt
      if (ptDefined && isAnonClass && !matches)
        avoidingType = pt

      val tree1 = ascribeType(tree, avoidingType)
      assert(/*ptDefined ||*/ noLeaks(tree1) || tree1.tpe.widen.isErroneous,
          // `ptDefined` needed because of special case of anonymous classes
          i"leak: ${escapingRefs(tree1, localSyms).toList}%, % in $tree1")
      tree1
    }
  }

  def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context): Tree = track("typedIf") {
    val cond1 = typed(tree.cond, defn.BooleanType)
    val thenp2 :: elsep2 :: Nil = harmonic(harmonize) {
      val thenp1 = typed(tree.thenp, pt.notApplied)
      val elsep1 = typed(tree.elsep orElse (untpd.unitLiteral withPos tree.pos), pt.notApplied)
      thenp1 :: elsep1 :: Nil
    }
    assignType(cpy.If(tree)(cond1, thenp2, elsep2), thenp2, elsep2)
  }

  /** Decompose function prototype into a list of parameter prototypes and a result prototype
   *  tree, using WildcardTypes where a type is not known.
   *  For the result type we do this even if the expected type is not fully
   *  defined, which is a bit of a hack. But it's needed to make the following work
   *  (see typers.scala and printers/PlainPrinter.scala for examples).
   *
   *     def double(x: Char): String = s"$x$x"
   *     "abc" flatMap double
   */
  private def decomposeProtoFunction(pt: Type, defaultArity: Int)(implicit ctx: Context): (List[Type], untpd.Tree) = {
    def typeTree(tp: Type) = tp match {
      case _: WildcardType => untpd.TypeTree()
      case _ => untpd.TypeTree(tp)
    }
    pt.stripTypeVar match {
      case _ if defn.isNonDepFunctionType(pt) =>
        // if expected parameter type(s) are wildcards, approximate from below.
        // if expected result type is a wildcard, approximate from above.
        // this can type the greatest set of admissible closures.
        val funType = pt.dealias
        (funType.argTypesLo.init, typeTree(funType.argTypesHi.last))
      case SAMType(sam @ MethodTpe(_, formals, restpe)) =>
        (formals,
         if (sam.isResultDependent)
           untpd.DependentTypeTree(syms => restpe.substParams(sam, syms.map(_.termRef)))
         else
           typeTree(restpe))
      case tp: TypeParamRef =>
        decomposeProtoFunction(ctx.typerState.constraint.entry(tp).bounds.hi, defaultArity)
      case _ =>
        (List.tabulate(defaultArity)(alwaysWildcardType), untpd.TypeTree())
    }
  }

  def typedFunction(tree: untpd.Function, pt: Type)(implicit ctx: Context) = track("typedFunction") {
    if (ctx.mode is Mode.Type) typedFunctionType(tree, pt)
    else typedFunctionValue(tree, pt)
  }

  def typedFunctionType(tree: untpd.Function, pt: Type)(implicit ctx: Context) = {
    val untpd.Function(args, body) = tree
    val (isImplicit, isErased) = tree match {
      case tree: untpd.FunctionWithMods =>
        val isImplicit = tree.mods.is(Implicit)
        var isErased = tree.mods.is(Erased)
        if (isErased && args.isEmpty) {
          ctx.error("An empty function cannot not be erased", tree.pos)
          isErased = false
        }
        (isImplicit, isErased)
      case _ => (false, false)
    }

    val funCls = defn.FunctionClass(args.length, isImplicit, isErased)

    /** Typechecks dependent function type with given parameters `params` */
    def typedDependent(params: List[ValDef])(implicit ctx: Context): Tree = {
      completeParams(params)
      val params1 = params.map(typedExpr(_).asInstanceOf[ValDef])
      val resultTpt = typed(body)
      val companion = MethodType.maker(isImplicit = isImplicit, isErased = isErased)
      val mt = companion.fromSymbols(params1.map(_.symbol), resultTpt.tpe)
      if (mt.isParamDependent)
        ctx.error(i"$mt is an illegal function type because it has inter-parameter dependencies", tree.pos)
      val resTpt = TypeTree(mt.nonDependentResultApprox).withPos(body.pos)
      val typeArgs = params1.map(_.tpt) :+ resTpt
      val tycon = TypeTree(funCls.typeRef)
      val core = assignType(cpy.AppliedTypeTree(tree)(tycon, typeArgs), tycon, typeArgs)
      val appMeth = ctx.newSymbol(ctx.owner, nme.apply, Synthetic | Deferred, mt)
      val appDef = assignType(
        untpd.DefDef(appMeth.name, Nil, List(params1), resultTpt, EmptyTree),
        appMeth)
      RefinedTypeTree(core, List(appDef), ctx.owner.asClass)
    }

    args match {
      case ValDef(_, _, _) :: _ =>
        typedDependent(args.asInstanceOf[List[ValDef]])(
          ctx.fresh.setOwner(ctx.newRefinedClassSymbol(tree.pos)).setNewScope)
      case _ =>
        typed(cpy.AppliedTypeTree(tree)(untpd.TypeTree(funCls.typeRef), args :+ body), pt)
    }
  }

  def typedFunctionValue(tree: untpd.Function, pt: Type)(implicit ctx: Context) = {
    val untpd.Function(params: List[untpd.ValDef] @unchecked, body) = tree

    val isImplicit = tree match {
      case tree: untpd.FunctionWithMods => tree.mods.is(Implicit)
      case _ => false
    }

    pt match {
      case pt: TypeVar if untpd.isFunctionWithUnknownParamType(tree) =>
        // try to instantiate `pt` if this is possible. If it does not
        // work the error will be reported later in `inferredParam`,
        // when we try to infer the parameter type.
        isFullyDefined(pt, ForceDegree.noBottom)
      case _ =>
    }

    val (protoFormals, resultTpt) = decomposeProtoFunction(pt, params.length)

    def refersTo(arg: untpd.Tree, param: untpd.ValDef): Boolean = arg match {
      case Ident(name) => name == param.name
      case _ => false
    }

    /** The function body to be returned in the closure. Can become a TypedSplice
      *  of a typed expression if this is necessary to infer a parameter type.
      */
    var fnBody = tree.body

    /** A map from parameter names to unique positions where the parameter
      *  appears in the argument list of an application.
      */
    var paramIndex = Map[Name, Int]()

    /** If parameter `param` appears exactly once as an argument in `args`,
      *  the singleton list consisting of its position in `args`, otherwise `Nil`.
      */
    def paramIndices(param: untpd.ValDef, args: List[untpd.Tree]): List[Int] = {
      def loop(args: List[untpd.Tree], start: Int): List[Int] = args match {
        case arg :: args1 =>
          val others = loop(args1, start + 1)
          if (refersTo(arg, param)) start :: others else others
        case _ => Nil
      }
      val allIndices = loop(args, 0)
      if (allIndices.length == 1) allIndices else Nil
    }

    /** If function is of the form
      *      (x1, ..., xN) => f(... x1, ..., XN, ...)
      *  where each `xi` occurs exactly once in the argument list of `f` (in
      *  any order), the type of `f`, otherwise NoType.
      *  Updates `fnBody` and `paramIndex` as a side effect.
      *  @post: If result exists, `paramIndex` is defined for the name of
      *         every parameter in `params`.
      */
    def calleeType: Type = fnBody match {
      case Apply(expr, args) =>
        paramIndex = {
          for (param <- params; idx <- paramIndices(param, args))
          yield param.name -> idx
        }.toMap
        if (paramIndex.size == params.length)
          expr match {
            case untpd.TypedSplice(expr1) =>
              expr1.tpe
            case _ =>
              val protoArgs = args map (_ withType WildcardType)
              val callProto = FunProto(protoArgs, WildcardType, this)
              val expr1 = typedExpr(expr, callProto)
              fnBody = cpy.Apply(fnBody)(untpd.TypedSplice(expr1), args)
              expr1.tpe
          }
        else NoType
      case _ =>
        NoType
    }

    /** Two attempts: First, if expected type is fully defined pick this one.
      *  Second, if function is of the form
      *      (x1, ..., xN) => f(... x1, ..., XN, ...)
      *  where each `xi` occurs exactly once in the argument list of `f` (in
      *  any order), and f has a method type MT, pick the corresponding parameter
      *  type in MT, if this one is fully defined.
      *  If both attempts fail, issue a "missing parameter type" error.
      */
    def inferredParamType(param: untpd.ValDef, formal: Type): Type = {
      if (isFullyDefined(formal, ForceDegree.noBottom)) return formal
      calleeType.widen match {
        case mtpe: MethodType =>
          val pos = paramIndex(param.name)
          if (pos < mtpe.paramInfos.length) {
            val ptype = mtpe.paramInfos(pos)
            if (isFullyDefined(ptype, ForceDegree.noBottom) && !ptype.isRepeatedParam)
              return ptype
          }
        case _ =>
      }
      errorType(AnonymousFunctionMissingParamType(param, params, tree, pt), param.pos)
    }

    def protoFormal(i: Int): Type =
      if (protoFormals.length == params.length) protoFormals(i)
      else errorType(WrongNumberOfParameters(protoFormals.length), tree.pos)

    /** Is `formal` a product type which is elementwise compatible with `params`? */
    def ptIsCorrectProduct(formal: Type) = {
      isFullyDefined(formal, ForceDegree.noBottom) &&
      defn.isProductSubType(formal) &&
      Applications.productSelectorTypes(formal).corresponds(params) {
        (argType, param) =>
          param.tpt.isEmpty || argType <:< typedAheadType(param.tpt).tpe
      }
    }

    val desugared =
      if (protoFormals.length == 1 && params.length != 1 && ptIsCorrectProduct(protoFormals.head)) {
        desugar.makeTupledFunction(params, fnBody)
      }
      else {
        val inferredParams: List[untpd.ValDef] =
          for ((param, i) <- params.zipWithIndex) yield
            if (!param.tpt.isEmpty) param
            else cpy.ValDef(param)(
              tpt = untpd.TypeTree(
                inferredParamType(param, protoFormal(i)).underlyingIfRepeated(isJava = false)))
        desugar.makeClosure(inferredParams, fnBody, resultTpt, isImplicit)
      }
    typed(desugared, pt)
  }

  def typedClosure(tree: untpd.Closure, pt: Type)(implicit ctx: Context): Tree = track("typedClosure") {
    val env1 = tree.env mapconserve (typed(_))
    val meth1 = typedUnadapted(tree.meth)
    val target =
      if (tree.tpt.isEmpty)
        meth1.tpe.widen match {
          case mt: MethodType =>
            pt match {
              case SAMType(sam)
              if !defn.isFunctionType(pt) && mt <:< sam =>
                if (!isFullyDefined(pt, ForceDegree.all))
                  ctx.error(ex"result type of closure is an underspecified SAM type $pt", tree.pos)
                TypeTree(pt)
              case _ =>
                if (mt.isParamDependent) {
                  throw new java.lang.Error(
                    i"""internal error: cannot turn method type $mt into closure
                     |because it has internal parameter dependencies,
                     |position = ${tree.pos}, raw type = ${mt.toString}""") // !!! DEBUG. Eventually, convert to an error?
                }
                else if ((tree.tpt `eq` untpd.ImplicitEmptyTree) && mt.paramNames.isEmpty)
                  // Note implicitness of function in target type sicne there are no method parameters that indicate it.
                  TypeTree(defn.FunctionOf(Nil, mt.resType, isImplicit = true, isErased = false))
                else
                  EmptyTree
            }
          case tp =>
            throw new java.lang.Error(i"internal error: closing over non-method $tp, pos = ${tree.pos}")
        }
      else typed(tree.tpt)
    //println(i"typing closure $tree : ${meth1.tpe.widen}")
    assignType(cpy.Closure(tree)(env1, meth1, target), meth1, target)
  }

  def typedMatch(tree: untpd.Match, pt: Type)(implicit ctx: Context) = track("typedMatch") {
    tree.selector match {
      case EmptyTree =>
        val (protoFormals, _) = decomposeProtoFunction(pt, 1)
        val unchecked = pt.isRef(defn.PartialFunctionClass)
        typed(desugar.makeCaseLambda(tree.cases, protoFormals.length, unchecked) withPos tree.pos, pt)
      case _ =>
        val sel1 = typedExpr(tree.selector)
        val selType = fullyDefinedType(sel1.tpe, "pattern selector", tree.pos).widen

        val cases1 = harmonic(harmonize)(typedCases(tree.cases, selType, pt.notApplied))
          .asInstanceOf[List[CaseDef]]
        assignType(cpy.Match(tree)(sel1, cases1), cases1)
    }
  }

  def typedCases(cases: List[untpd.CaseDef], selType: Type, pt: Type)(implicit ctx: Context) = {

    /** gadtSyms = "all type parameters of enclosing methods that appear
     *              non-variantly in the selector type" todo: should typevars
     *              which appear with variances +1 and -1 (in different
     *              places) be considered as well?
     */
    val gadtSyms: Set[Symbol] = trace(i"GADT syms of $selType", gadts) {
      val accu = new TypeAccumulator[Set[Symbol]] {
        def apply(tsyms: Set[Symbol], t: Type): Set[Symbol] = {
          val tsyms1 = t match {
            case tr: TypeRef if (tr.symbol is TypeParam) && tr.symbol.owner.isTerm && variance == 0 =>
              tsyms + tr.symbol
            case _ =>
              tsyms
          }
          foldOver(tsyms1, t)
        }
      }
      accu(Set.empty, selType)
    }

    cases mapconserve (typedCase(_, pt, selType, gadtSyms))
  }

  /** Type a case. */
  def typedCase(tree: untpd.CaseDef, pt: Type, selType: Type, gadtSyms: Set[Symbol])(implicit ctx: Context): CaseDef = track("typedCase") {
    val originalCtx = ctx

    val gadtCtx = ctx.fresh.setFreshGADTBounds
    for (sym <- gadtSyms)
      if (!gadtCtx.gadt.bounds.contains(sym))
        gadtCtx.gadt.setBounds(sym, TypeBounds.empty)

    var wildcardSyms: List[Symbol] = Nil

    /** - strip all instantiated TypeVars from pattern types.
     *    run/reducable.scala is a test case that shows stripping typevars is necessary.
     *  - enter all symbols introduced by a Bind in current scope
     */
    val indexPattern = new TreeMap {
      val stripTypeVars = new TypeMap {
        def apply(t: Type) = mapOver(t)
      }
      override def transform(trt: Tree)(implicit ctx: Context) =
        super.transform(trt.withType(stripTypeVars(trt.tpe))) match {
          case b: Bind =>
            val sym = b.symbol
            if (sym.name != tpnme.WILDCARD)
              if (ctx.scope.lookup(b.name) == NoSymbol) ctx.enter(sym)
              else ctx.error(new DuplicateBind(b, tree), b.pos)
            else
              wildcardSyms = sym :: wildcardSyms
            if (!ctx.isAfterTyper) {
              val bounds = ctx.gadt.bounds(sym)
              if (bounds != null) sym.info = bounds
            }
            b
          case t => t
        }
      }

    def caseRest(pat: Tree)(implicit ctx: Context) = {
      val pat1 = indexPattern.transform(pat)
      val guard1 = typedExpr(tree.guard, defn.BooleanType)
      var body1 = ensureNoLocalRefs(typedExpr(tree.body, pt), pt, wildcardSyms ++ ctx.scope.toList)
      if (pt.isValueType) // insert a cast if body does not conform to expected type if we disregard gadt bounds
        body1 = body1.ensureConforms(pt)(originalCtx)
      assignType(cpy.CaseDef(tree)(pat1, guard1, body1), body1)
    }

    val pat1 = typedPattern(tree.pat, selType)(gadtCtx)
    caseRest(pat1)(gadtCtx.fresh.setNewScope)
  }

  def typedReturn(tree: untpd.Return)(implicit ctx: Context): Return = track("typedReturn") {
    def returnProto(owner: Symbol, locals: Scope): Type =
      if (owner.isConstructor) defn.UnitType
      else owner.info match {
        case info: PolyType =>
          val tparams = locals.toList.takeWhile(_ is TypeParam)
          assert(info.paramNames.length == tparams.length,
                 i"return mismatch from $owner, tparams = $tparams, locals = ${locals.toList}%, %")
          info.instantiate(tparams.map(_.typeRef)).finalResultType
        case info =>
          info.finalResultType
      }
    def enclMethInfo(cx: Context): (Tree, Type) = {
      val owner = cx.owner
      if (owner.isType) {
        ctx.error(ReturnOutsideMethodDefinition(owner), tree.pos)
        (EmptyTree, WildcardType)
      }
      else if (owner != cx.outer.owner && owner.isRealMethod) {
        if (owner.isInlineableMethod)
          (EmptyTree, errorType(NoReturnFromInline(owner), tree.pos))
        else if (!owner.isCompleted)
          (EmptyTree, errorType(MissingReturnTypeWithReturnStatement(owner), tree.pos))
        else {
          val from = Ident(TermRef(NoPrefix, owner.asTerm))
          val proto = returnProto(owner, cx.scope)
          (from, proto)
        }
      }
      else enclMethInfo(cx.outer)
    }
    val (from, proto) =
      if (tree.from.isEmpty) enclMethInfo(ctx)
      else {
        val from = tree.from.asInstanceOf[tpd.Tree]
        val proto =
          if (ctx.erasedTypes) from.symbol.info.finalResultType
          else WildcardType // We cannot reliably detect the internal type view of polymorphic or dependent methods
                            // because we do not know the internal type params and method params.
                            // Hence no adaptation is possible, and we assume WildcardType as prototype.
        (from, proto)
      }
    val expr1 = typedExpr(tree.expr orElse untpd.unitLiteral.withPos(tree.pos), proto)
    assignType(cpy.Return(tree)(expr1, from))
  }

  def typedTry(tree: untpd.Try, pt: Type)(implicit ctx: Context): Try = track("typedTry") {
    val expr2 :: cases2x = harmonic(harmonize) {
      val expr1 = typed(tree.expr, pt.notApplied)
      val cases1 = typedCases(tree.cases, defn.ThrowableType, pt.notApplied)
      expr1 :: cases1
    }
    val finalizer1 = typed(tree.finalizer, defn.UnitType)
    val cases2 = cases2x.asInstanceOf[List[CaseDef]]
    assignType(cpy.Try(tree)(expr2, cases2, finalizer1), expr2, cases2)
  }

  def typedThrow(tree: untpd.Throw)(implicit ctx: Context): Tree = track("typedThrow") {
    val expr1 = typed(tree.expr, defn.ThrowableType)
    Throw(expr1).withPos(tree.pos)
  }

  def typedSeqLiteral(tree: untpd.SeqLiteral, pt: Type)(implicit ctx: Context): SeqLiteral = track("typedSeqLiteral") {
    val elemProto = pt.elemType match {
      case NoType => WildcardType
      case bounds: TypeBounds => WildcardType(bounds)
      case elemtp => elemtp
    }

    def assign(elems1: List[Tree], elemtpt1: Tree) =
      assignType(cpy.SeqLiteral(tree)(elems1, elemtpt1), elems1, elemtpt1)

    if (!tree.elemtpt.isEmpty) {
      val elemtpt1 = typed(tree.elemtpt, elemProto)
      val elems1 = tree.elems.mapconserve(typed(_, elemtpt1.tpe))
      assign(elems1, elemtpt1)
    } else {
      val elems1 = tree.elems.mapconserve(typed(_, elemProto))
      val elemtptType =
        if (isFullyDefined(elemProto, ForceDegree.none))
          elemProto
        else if (tree.elems.isEmpty && tree.isInstanceOf[Trees.JavaSeqLiteral[_]])
          defn.ObjectType // generic empty Java varargs are of type Object[]
        else
          ctx.typeComparer.lub(elems1.tpes)
      val elemtpt1 = typed(tree.elemtpt, elemtptType)
      assign(elems1, elemtpt1)
    }
  }

  def typedInlined(tree: untpd.Inlined, pt: Type)(implicit ctx: Context): Inlined = {
    val (exprCtx, bindings1) = typedBlockStats(tree.bindings)
    val expansion1 = typed(tree.expansion, pt)(inlineContext(tree.call)(exprCtx))
    assignType(cpy.Inlined(tree)(tree.call, bindings1.asInstanceOf[List[MemberDef]], expansion1),
        bindings1, expansion1)
  }

  def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): Tree = track("typedTypeTree") {
    tree match {
      case tree: untpd.DerivedTypeTree =>
        tree.ensureCompletions
        tree.getAttachment(untpd.OriginalSymbol) match {
          case Some(origSym) =>
            tree.derivedTree(origSym).withPos(tree.pos)
            // btw, no need to remove the attachment. The typed
            // tree is different from the untyped one, so the
            // untyped tree is no longer accessed after all
            // accesses with typedTypeTree are done.
          case None =>
            errorTree(tree, "Something's wrong: missing original symbol for type tree")
        }
      case _ =>
        tree.withType(
          if (isFullyDefined(pt, ForceDegree.none)) pt else UnspecifiedErrorType)
    }
  }

  def typedSingletonTypeTree(tree: untpd.SingletonTypeTree)(implicit ctx: Context): SingletonTypeTree = track("typedSingletonTypeTree") {
    val ref1 = typedExpr(tree.ref)
    checkStable(ref1.tpe, tree.pos)
    assignType(cpy.SingletonTypeTree(tree)(ref1), ref1)
  }

  def typedAndTypeTree(tree: untpd.AndTypeTree)(implicit ctx: Context): AndTypeTree = track("typedAndTypeTree") {
    val left1 = checkSimpleKinded(typed(tree.left))
    val right1 = checkSimpleKinded(typed(tree.right))
    assignType(cpy.AndTypeTree(tree)(left1, right1), left1, right1)
  }

  def typedOrTypeTree(tree: untpd.OrTypeTree)(implicit ctx: Context): OrTypeTree = track("typedOrTypeTree") {
    val where = "in a union type"
    val left1 = checkNotSingleton(checkSimpleKinded(typed(tree.left)), where)
    val right1 = checkNotSingleton(checkSimpleKinded(typed(tree.right)), where)
    assignType(cpy.OrTypeTree(tree)(left1, right1), left1, right1)
  }

  def typedRefinedTypeTree(tree: untpd.RefinedTypeTree)(implicit ctx: Context): RefinedTypeTree = track("typedRefinedTypeTree") {
    val tpt1 = if (tree.tpt.isEmpty) TypeTree(defn.ObjectType) else typedAheadType(tree.tpt)
    val refineClsDef = desugar.refinedTypeToClass(tpt1, tree.refinements).withPos(tree.pos)
    val refineCls = createSymbol(refineClsDef).asClass
    val TypeDef(_, impl: Template) = typed(refineClsDef)
    val refinements1 = impl.body
    assert(tree.refinements.hasSameLengthAs(refinements1), i"${tree.refinements}%, % > $refinements1%, %")
    val seen = mutable.Set[Symbol]()
    for (refinement <- refinements1) { // TODO: get clarity whether we want to enforce these conditions
      typr.println(s"adding refinement $refinement")
      checkRefinementNonCyclic(refinement, refineCls, seen)
      val rsym = refinement.symbol
      if (rsym.info.isInstanceOf[PolyType] && rsym.allOverriddenSymbols.isEmpty)
        ctx.error(PolymorphicMethodMissingTypeInParent(rsym, tpt1.symbol), refinement.pos)
    }
    assignType(cpy.RefinedTypeTree(tree)(tpt1, refinements1), tpt1, refinements1, refineCls)
  }

  def typedAppliedTypeTree(tree: untpd.AppliedTypeTree)(implicit ctx: Context): Tree = track("typedAppliedTypeTree") {
    val tpt1 = typed(tree.tpt, AnyTypeConstructorProto)(ctx.retractMode(Mode.Pattern))
    val tparams = tpt1.tpe.typeParams
    if (tparams.isEmpty) {
      ctx.error(TypeDoesNotTakeParameters(tpt1.tpe, tree.args), tree.pos)
      tpt1
    }
    else {
      var args = tree.args
      val args1 = {
        if (args.length != tparams.length) {
          wrongNumberOfTypeArgs(tpt1.tpe, tparams, args, tree.pos)
          args = args.take(tparams.length)
        }
        def typedArg(arg: untpd.Tree, tparam: ParamInfo) = {
          val (desugaredArg, argPt) =
            if (ctx.mode is Mode.Pattern)
              (if (untpd.isVarPattern(arg)) desugar.patternVar(arg) else arg, tparam.paramInfo)
            else
              (arg, WildcardType)
          if (tpt1.symbol.isClass)
            tparam match {
              case tparam: Symbol =>
                tparam.ensureCompleted() // This is needed to get the test `compileParSetSubset` to work
              case _ =>
            }
          if (desugaredArg.isType) {
            var res = typed(desugaredArg, argPt)
            arg match {
              case TypeBoundsTree(EmptyTree, EmptyTree)
              if tparam.paramInfo.isLambdaSub &&
                 tpt1.tpe.typeParamSymbols.nonEmpty &&
                 !ctx.mode.is(Mode.Pattern) =>
                // An unbounded `_` automatically adapts to type parameter bounds. This means:
                // If we have wildcard application C[_], where `C` is a class replace
                // with C[_ >: L <: H] where `L` and `H` are the bounds of the corresponding
                // type parameter in `C`, avoiding any referemces to parameters of `C`.
                // The transform does not apply for patters, where empty bounds translate to
                // wildcard identifiers `_` instead.
                res = res.withType(avoid(tparam.paramInfo, tpt1.tpe.typeParamSymbols))
              case _ =>
            }
            res
          }
          else desugaredArg.withType(UnspecifiedErrorType)
        }
        args.zipWithConserve(tparams)(typedArg(_, _)).asInstanceOf[List[Tree]]
      }
      val paramBounds = (tparams, args).zipped.map {
        case (tparam, TypeBoundsTree(EmptyTree, EmptyTree)) =>
          // if type argument is a wildcard, suppress kind checking since
          // there is no real argument.
          NoType
        case (tparam, _) =>
          tparam.paramInfo.bounds
      }
      val args2 = preCheckKinds(args1, paramBounds)
      // check that arguments conform to bounds is done in phase PostTyper
      assignType(cpy.AppliedTypeTree(tree)(tpt1, args2), tpt1, args2)
    }
  }

  def typedLambdaTypeTree(tree: untpd.LambdaTypeTree)(implicit ctx: Context): Tree = track("typedLambdaTypeTree") {
    val LambdaTypeTree(tparams, body) = tree
    indexAndAnnotate(tparams)
    val tparams1 = tparams.mapconserve(typed(_).asInstanceOf[TypeDef])
    val body1 = typedType(tree.body)
    assignType(cpy.LambdaTypeTree(tree)(tparams1, body1), tparams1, body1)
  }

  def typedByNameTypeTree(tree: untpd.ByNameTypeTree)(implicit ctx: Context): ByNameTypeTree = track("typedByNameTypeTree") {
    val result1 = typed(tree.result)
    assignType(cpy.ByNameTypeTree(tree)(result1), result1)
  }

  def typedTypeBoundsTree(tree: untpd.TypeBoundsTree, pt: Type)(implicit ctx: Context): Tree = track("typedTypeBoundsTree") {
    val TypeBoundsTree(lo, hi) = tree
    val lo1 = typed(lo)
    val hi1 = typed(hi)

    val lo2 = if (lo1.isEmpty) typed(untpd.TypeTree(defn.NothingType)) else lo1
    val hi2 = if (hi1.isEmpty) typed(untpd.TypeTree(defn.AnyType)) else hi1

    val tree1 = assignType(cpy.TypeBoundsTree(tree)(lo2, hi2), lo2, hi2)
    if (ctx.mode.is(Mode.Pattern)) {
      // Associate a pattern-bound type symbol with the wildcard.
      // The bounds of the type symbol can be constrained when comparing a pattern type
      // with an expected type in typedTyped. The type symbol and the defining Bind node
      // are eliminated once the enclosing pattern has been typechecked; see `indexPattern`
      // in `typedCase`.
      //val ptt = if (lo.isEmpty && hi.isEmpty) pt else
      if (ctx.isAfterTyper) tree1
      else {
        val wildcardSym = ctx.newPatternBoundSymbol(tpnme.WILDCARD, tree1.tpe & pt, tree.pos)
        untpd.Bind(tpnme.WILDCARD, tree1).withType(wildcardSym.typeRef)
      }
    }
    else tree1
  }

  def typedBind(tree: untpd.Bind, pt: Type)(implicit ctx: Context): Tree = track("typedBind") {
    val pt1 = fullyDefinedType(pt, "pattern variable", tree.pos)
    val body1 = typed(tree.body, pt1)
    body1 match {
      case UnApply(fn, Nil, arg :: Nil)
      if fn.symbol.exists && fn.symbol.owner == defn.ClassTagClass && !body1.tpe.isError =>
        // A typed pattern `x @ (e: T)` with an implicit `ctag: ClassTag[T]`
        // was rewritten to `x @ ctag(e)` by `tryWithClassTag`.
        // Rewrite further to `ctag(x @ e)`
        tpd.cpy.UnApply(body1)(fn, Nil,
            typed(untpd.Bind(tree.name, untpd.TypedSplice(arg)).withPos(tree.pos), arg.tpe) :: Nil)
      case _ =>
        if (tree.name == nme.WILDCARD) body1
        else {
          // for a singleton pattern like `x @ Nil`, `x` should get the type from the scrutinee
          // see tests/neg/i3200b.scala and SI-1503
          val symTp =
            if (body1.tpe.isInstanceOf[TermRef]) pt1
            else body1.tpe.underlyingIfRepeated(isJava = false)
          val sym = ctx.newPatternBoundSymbol(tree.name, symTp, tree.pos)
          if (ctx.mode.is(Mode.InPatternAlternative))
            ctx.error(i"Illegal variable ${sym.name} in pattern alternative", tree.pos)
          assignType(cpy.Bind(tree)(tree.name, body1), sym)
        }
    }
  }

  def typedAlternative(tree: untpd.Alternative, pt: Type)(implicit ctx: Context): Alternative = track("typedAlternative") {
    val nestedCtx = ctx.addMode(Mode.InPatternAlternative)
    val trees1 = tree.trees.mapconserve(typed(_, pt)(nestedCtx))
    assignType(cpy.Alternative(tree)(trees1), trees1)
  }

  def completeAnnotations(mdef: untpd.MemberDef, sym: Symbol)(implicit ctx: Context): Unit = {
    // necessary to force annotation trees to be computed.
    sym.annotations.foreach(_.ensureCompleted)
    lazy val annotCtx = {
      val c = ctx.outersIterator.dropWhile(_.owner == sym).next()
      c.property(ExprOwner) match {
        case Some(exprOwner) if c.owner.isClass =>
          // We need to evaluate annotation arguments in an expression context, since
          // classes defined in a such arguments should not be entered into the
          // enclosing class.
          c.exprContext(mdef, exprOwner)
        case _ => c
      }
    }
    // necessary in order to mark the typed ahead annotations as definitely typed:
    untpd.modsDeco(mdef).mods.annotations.foreach(typedAnnotation(_)(annotCtx))
  }

  def typedAnnotation(annot: untpd.Tree)(implicit ctx: Context): Tree = track("typedAnnotation") {
    typed(annot, defn.AnnotationType)
  }

  def typedValDef(vdef: untpd.ValDef, sym: Symbol)(implicit ctx: Context) = track("typedValDef") {
    val ValDef(name, tpt, _) = vdef
    completeAnnotations(vdef, sym)
    val tpt1 = checkSimpleKinded(typedType(tpt))
    val rhs1 = vdef.rhs match {
      case rhs @ Ident(nme.WILDCARD) => rhs withType tpt1.tpe
      case rhs => normalizeErasedRhs(typedExpr(rhs, tpt1.tpe), sym)
    }
    val vdef1 = assignType(cpy.ValDef(vdef)(name, tpt1, rhs1), sym)
    if (sym.is(Inline, butNot = DeferredOrTermParamOrAccessor))
      checkInlineConformant(rhs1, isFinal = sym.is(Final), em"right-hand side of inline $sym")
    patchIfLazy(vdef1)
    patchFinalVals(vdef1)
    vdef1
  }

  /** Add a @volitile to lazy vals when rewriting from Scala2 */
  private def patchIfLazy(vdef: ValDef)(implicit ctx: Context): Unit = {
    val sym = vdef.symbol
    if (sym.is(Lazy, butNot = Deferred | Module | Synthetic) && !sym.isVolatile &&
        ctx.scala2Mode && ctx.settings.rewrite.value.isDefined &&
        !ctx.isAfterTyper)
      patch(Position(toUntyped(vdef).pos.start), "@volatile ")
  }

  /** Adds inline to final vals with idempotent rhs
   *
   *  duplicating scalac behavior: for final vals that have rhs as constant, we do not create a field
   *  and instead return the value. This seemingly minor optimization has huge effect on initialization
   *  order and the values that can be observed during superconstructor call
   *
   *  see remark about idempotency in TreeInfo#constToLiteral
   */
  private def patchFinalVals(vdef: ValDef)(implicit ctx: Context): Unit = {
    def isFinalInlinableVal(sym: Symbol): Boolean = {
      sym.is(Final, butNot = Mutable) &&
      isIdempotentExpr(vdef.rhs) /* &&
      ctx.scala2Mode (stay compatible with Scala2 for now) */
    }
    val sym = vdef.symbol
    sym.info match {
      case info: ConstantType if isFinalInlinableVal(sym) && !ctx.settings.YnoInline.value => sym.setFlag(Inline)
      case _ =>
    }
  }

  def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = track("typedDefDef") {
    val DefDef(name, tparams, vparamss, tpt, _) = ddef
    completeAnnotations(ddef, sym)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val vparamss1 = vparamss nestedMapconserve (typed(_).asInstanceOf[ValDef])
    vparamss1.foreach(checkNoForwardDependencies)
    if (sym is Implicit) {
      checkImplicitParamsNotSingletons(vparamss1)
      checkImplicitConversionDefOK(sym)
    }
    val tpt1 = checkSimpleKinded(typedType(tpt))

    var rhsCtx = ctx
    if (sym.isConstructor && !sym.isPrimaryConstructor && tparams1.nonEmpty) {
      // for secondary constructors we need a context that "knows"
      // that their type parameters are aliases of the class type parameters.
      // See pos/i941.scala
      rhsCtx = ctx.fresh.setFreshGADTBounds
      (tparams1, sym.owner.typeParams).zipped.foreach ((tdef, tparam) =>
        rhsCtx.gadt.setBounds(tdef.symbol, TypeAlias(tparam.typeRef)))
    }
    val rhs1 = normalizeErasedRhs(typedExpr(ddef.rhs, tpt1.tpe)(rhsCtx), sym)

    // Overwrite inline body to make sure it is not evaluated twice
    if (sym.isInlineableMethod) Inliner.registerInlineInfo(sym, _ => rhs1)

    if (sym.isConstructor && !sym.isPrimaryConstructor)
      for (param <- tparams1 ::: vparamss1.flatten)
        checkRefsLegal(param, sym.owner, (name, sym) => sym.is(TypeParam), "secondary constructor")

    assignType(cpy.DefDef(ddef)(name, tparams1, vparamss1, tpt1, rhs1), sym)
      //todo: make sure dependent method types do not depend on implicits or by-name params
  }

  def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(implicit ctx: Context): Tree = track("typedTypeDef") {
    val TypeDef(name, rhs) = tdef
    completeAnnotations(tdef, sym)
    val rhs1 = tdef.rhs match {
      case rhs @ LambdaTypeTree(tparams, body) =>
        val tparams1 = tparams.map(typed(_)).asInstanceOf[List[TypeDef]]
        val body1 = typedType(body)
        assignType(cpy.LambdaTypeTree(rhs)(tparams1, body1), tparams1, body1)
      case rhs =>
        typedType(rhs)
    }
    assignType(cpy.TypeDef(tdef)(name, rhs1), sym)
  }

  def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context): Tree = track("typedClassDef") {
    if (!cls.info.isInstanceOf[ClassInfo]) return EmptyTree.assertingErrorsReported

    val TypeDef(name, impl @ Template(constr, parents, self, _)) = cdef
    val superCtx = ctx.superCallContext

    /** If `ref` is an implicitly parameterized trait, pass an implicit argument list.
     *  Otherwise, if `ref` is a parameterized trait, error.
     *  Note: Traits and classes currently always have at least an empty parameter list ()
     *        before the implicit parameters (this is inserted if not given in source).
     *        We skip this parameter list when deciding whether a trait is parameterless or not.
     *  @param ref   The tree referring to the (parent) trait
     *  @param psym  Its type symbol
     *  @param cinfo The info of its constructor
     */
    def maybeCall(ref: Tree, psym: Symbol, cinfo: Type): Tree = cinfo.stripPoly match {
      case cinfo @ MethodType(Nil) if cinfo.resultType.isImplicitMethod =>
        val icall = New(ref).select(nme.CONSTRUCTOR).appliedToNone
        typedExpr(untpd.TypedSplice(icall))(superCtx)
      case cinfo @ MethodType(Nil) if !cinfo.resultType.isInstanceOf[MethodType] =>
        ref
      case cinfo: MethodType =>
        if (!ctx.erasedTypes) { // after constructors arguments are passed in super call.
          typr.println(i"constr type: $cinfo")
          ctx.error(ParameterizedTypeLacksArguments(psym), ref.pos)
        }
        ref
      case _ =>
        ref
    }

    val seenParents = mutable.Set[Symbol]()

    def typedParent(tree: untpd.Tree): Tree = {
      var result = if (tree.isType) typedType(tree)(superCtx) else typedExpr(tree)(superCtx)
      val psym = result.tpe.typeSymbol
      if (seenParents.contains(psym) && !cls.isRefinementClass)
        ctx.error(i"$psym is extended twice", tree.pos)
      seenParents += psym
      if (tree.isType) {
        if (psym.is(Trait) && !cls.is(Trait) && !cls.superClass.isSubClass(psym))
          result = maybeCall(result, psym, psym.primaryConstructor.info)
      }
      else checkParentCall(result, cls)
      checkTraitInheritance(psym, cls, tree.pos)
      if (cls is Case) checkCaseInheritance(psym, cls, tree.pos)
      result
    }

    /** Checks if one of the decls is a type with the same name as class type member in selfType */
    def classExistsOnSelf(decls: Scope, self: tpd.ValDef): Boolean = {
      val selfType = self.tpt.tpe
      if (!selfType.exists || (selfType.classSymbol eq cls)) false
      else {
        def memberInSelfButNotThis(decl: Symbol) =
          selfType.member(decl.name).symbol.filter(other => other.isClass && other.owner != cls)
        decls.iterator.filter(_.isType).foldLeft(false) { (foundRedef, decl) =>
          val other = memberInSelfButNotThis(decl)
          if (other.exists) {
            val msg = CannotHaveSameNameAs(decl, other, CannotHaveSameNameAs.DefinedInSelf(self))
            ctx.error(msg, decl.pos)
          }
          foundRedef || other.exists
        }
      }
    }

    completeAnnotations(cdef, cls)
    val constr1 = typed(constr).asInstanceOf[DefDef]
    val parentsWithClass = ensureFirstTreeIsClass(parents mapconserve typedParent, cdef.namePos)
    val parents1 = ensureConstrCall(cls, parentsWithClass)(superCtx)
    val self1 = typed(self)(ctx.outer).asInstanceOf[ValDef] // outer context where class members are not visible
    if (self1.tpt.tpe.isError || classExistsOnSelf(cls.unforcedDecls, self1)) {
      // fail fast to avoid typing the body with an error type
      cdef.withType(UnspecifiedErrorType)
    } else {
      val dummy = localDummy(cls, impl)
      val body1 = addAccessorDefs(cls,
        typedStats(impl.body, dummy)(ctx.inClassContext(self1.symbol)))
      if (!ctx.isAfterTyper)
        cls.setNoInitsFlags((NoInitsInterface /: body1) ((fs, stat) => fs & defKind(stat)))

      // Expand comments and type usecases if `-Ycook-comments` is set.
      if (ctx.settings.YcookComments.value) {
        cookComments(body1.map(_.symbol), self1.symbol)(ctx.localContext(cdef, cls).setNewScope)
      }

      checkNoDoubleDeclaration(cls)
      val impl1 = cpy.Template(impl)(constr1, parents1, self1, body1)
        .withType(dummy.termRef)
      checkVariance(impl1)
      if (!cls.is(AbstractOrTrait) && !ctx.isAfterTyper)
        checkRealizableBounds(cls, cdef.namePos)
      if (cls.is(Case) && cls.derivesFrom(defn.EnumClass)) checkEnum(cdef, cls)
      val cdef1 = assignType(cpy.TypeDef(cdef)(name, impl1), cls)
      if (ctx.phase.isTyper && cdef1.tpe.derivesFrom(defn.DynamicClass) && !ctx.dynamicsEnabled) {
        val isRequired = parents1.exists(_.tpe.isRef(defn.DynamicClass))
        ctx.featureWarning(nme.dynamics.toString, "extension of type scala.Dynamic", isScala2Feature = true,
          cls, isRequired, cdef.pos)
      }

      checkNonCyclicInherited(cls.thisType, cls.classParents, cls.info.decls, cdef.pos)

      // check value class constraints
      checkDerivedValueClass(cls, body1)

      if (ctx.settings.YretainTrees.value) cls.treeOrProvider = cdef1

      cdef1

      // todo later: check that
      //  1. If class is non-abstract, it is instantiatable:
      //  - self type is s supertype of own type
      //  - all type members have consistent bounds
      // 2. all private type members have consistent bounds
      // 3. Types do not override classes.
      // 4. Polymorphic type defs override nothing.
    }
  }

  protected def addAccessorDefs(cls: Symbol, body: List[Tree])(implicit ctx: Context): List[Tree] =
    ctx.compilationUnit.inlineAccessors.addAccessorDefs(cls, body)

  /** Ensure that the first type in a list of parent types Ps points to a non-trait class.
   *  If that's not already the case, add one. The added class type CT is determined as follows.
   *  First, let C be the unique class such that
   *  - there is a parent P_i such that P_i derives from C, and
   *  - for every class D: If some parent P_j, j <= i derives from D, then C derives from D.
   *  Then, let CT be the smallest type which
   *  - has C as its class symbol, and
   *  - for all parents P_i: If P_i derives from C then P_i <:< CT.
   */
  def ensureFirstIsClass(parents: List[Type], pos: Position)(implicit ctx: Context): List[Type] = {
    def realClassParent(cls: Symbol): ClassSymbol =
      if (!cls.isClass) defn.ObjectClass
      else if (!(cls is Trait)) cls.asClass
      else cls.asClass.classParents match {
        case parentRef :: _ => realClassParent(parentRef.typeSymbol)
        case nil => defn.ObjectClass
      }
    def improve(candidate: ClassSymbol, parent: Type): ClassSymbol = {
      val pcls = realClassParent(parent.classSymbol)
      if (pcls derivesFrom candidate) pcls else candidate
    }
    parents match {
      case p :: _ if p.classSymbol.isRealClass => parents
      case _ =>
        val pcls = (defn.ObjectClass /: parents)(improve)
        typr.println(i"ensure first is class $parents%, % --> ${parents map (_ baseType pcls)}%, %")
        val first = ctx.typeComparer.glb(defn.ObjectType :: parents.map(_.baseType(pcls)))
        checkFeasibleParent(first, pos, em" in inferred superclass $first") :: parents
    }
  }

  /** Ensure that first parent tree refers to a real class. */
  def ensureFirstTreeIsClass(parents: List[Tree], pos: Position)(implicit ctx: Context): List[Tree] = parents match {
    case p :: ps if p.tpe.classSymbol.isRealClass => parents
    case _ => TypeTree(ensureFirstIsClass(parents.tpes, pos).head).withPos(pos) :: parents
  }

  /** If this is a real class, make sure its first parent is a
   *  constructor call. Cannot simply use a type. Overridden in ReTyper.
   */
  def ensureConstrCall(cls: ClassSymbol, parents: List[Tree])(implicit ctx: Context): List[Tree] = {
    val firstParent :: otherParents = parents
    if (firstParent.isType && !(cls is Trait) && !cls.is(JavaDefined))
      typed(untpd.New(untpd.TypedSplice(firstParent), Nil)) :: otherParents
    else parents
  }

  /** Overridden in retyper */
  def checkVariance(tree: Tree)(implicit ctx: Context) = VarianceChecker.check(tree)

  def localDummy(cls: ClassSymbol, impl: untpd.Template)(implicit ctx: Context): Symbol =
    ctx.newLocalDummy(cls, impl.pos)

  def typedImport(imp: untpd.Import, sym: Symbol)(implicit ctx: Context): Import = track("typedImport") {
    val expr1 = typedExpr(imp.expr, AnySelectionProto)
    checkStable(expr1.tpe, imp.expr.pos)
    if (!ctx.isAfterTyper) checkRealizable(expr1.tpe, imp.expr.pos)
    assignType(cpy.Import(imp)(expr1, imp.selectors), sym)
  }

  def typedPackageDef(tree: untpd.PackageDef)(implicit ctx: Context): Tree = track("typedPackageDef") {
    val pid1 = typedExpr(tree.pid, AnySelectionProto)(ctx.addMode(Mode.InPackageClauseName))
    val pkg = pid1.symbol
    pid1 match {
      case pid1: RefTree if pkg.exists =>
        if (!pkg.is(Package)) ctx.error(PackageNameAlreadyDefined(pkg), tree.pos)
        val packageCtx = ctx.packageContext(tree, pkg)
        val stats1 = typedStats(tree.stats, pkg.moduleClass)(packageCtx)
        cpy.PackageDef(tree)(pid1, stats1).withType(pkg.termRef)
      case _ =>
        // Package will not exist if a duplicate type has already been entered, see `tests/neg/1708.scala`
        errorTree(tree, i"package ${tree.pid.name} does not exist")
    }
  }

  def typedAnnotated(tree: untpd.Annotated, pt: Type)(implicit ctx: Context): Tree = track("typedAnnotated") {
    val annot1 = typedExpr(tree.annot, defn.AnnotationType)
    val arg1 = typed(tree.arg, pt)
    if (ctx.mode is Mode.Type)
      assignType(cpy.Annotated(tree)(arg1, annot1), arg1, annot1)
    else {
      val tpt = TypeTree(AnnotatedType(arg1.tpe.widenIfUnstable, Annotation(annot1)))
      assignType(cpy.Typed(tree)(arg1, tpt), tpt)
    }
  }

  def typedTypedSplice(tree: untpd.TypedSplice)(implicit ctx: Context): Tree =
    tree.tree match {
      case tree1: TypeTree => tree1  // no change owner necessary here ...
      case tree1: Ident => tree1     // ... or here, since these trees cannot contain bindings
      case tree1 =>
        if (ctx.owner ne tree.owner) tree1.changeOwner(tree.owner, ctx.owner)
        else tree1
    }


  def typedAsFunction(tree: untpd.PostfixOp, pt: Type)(implicit ctx: Context): Tree = {
    val untpd.PostfixOp(qual, Ident(nme.WILDCARD)) = tree
    val pt1 = if (defn.isFunctionType(pt)) pt else AnyFunctionProto
    val nestedCtx = ctx.fresh.setNewTyperState()
    val res = typed(qual, pt1)(nestedCtx)
    res match {
      case closure(_, _, _) =>
      case _ =>
        val recovered = typed(qual)(ctx.fresh.setExploreTyperState())
        ctx.errorOrMigrationWarning(OnlyFunctionsCanBeFollowedByUnderscore(recovered.tpe.widen), tree.pos)
        if (ctx.scala2Mode) {
          // Under -rewrite, patch `x _` to `(() => x)`
          patch(Position(tree.pos.start), "(() => ")
          patch(Position(qual.pos.end, tree.pos.end), ")")
          return typed(untpd.Function(Nil, qual), pt)
        }
    }
    nestedCtx.typerState.commit()
    if (ctx.settings.strict.value) {
      lazy val (prefix, suffix) = res match {
        case Block(mdef @ DefDef(_, _, vparams :: Nil, _, _) :: Nil, _: Closure) =>
          val arity = vparams.length
          if (arity > 0) ("", "") else ("(() => ", "())")
        case _ =>
          ("(() => ", ")")
      }
      def remedy =
        if ((prefix ++ suffix).isEmpty) "simply leave out the trailing ` _`"
        else s"use `$prefix<function>$suffix` instead"
      ctx.errorOrMigrationWarning(i"""The syntax `<function> _` is no longer supported;
                                     |you can $remedy""", tree.pos)
      if (ctx.scala2Mode) {
        patch(Position(tree.pos.start), prefix)
        patch(Position(qual.pos.end, tree.pos.end), suffix)
      }
    }
    res
  }

  /** Translate infix operation expression `l op r` to
   *
   *    l.op(r)   			    if `op` is left-associative
   *    { val x = l; r.op(l) }  if `op` is right-associative call-by-value and `l` is impure
   *    r.op(l)                 if `op` is right-associative call-by-name or `l` is pure
   */
  def typedInfixOp(tree: untpd.InfixOp, pt: Type)(implicit ctx: Context): Tree = {
    val untpd.InfixOp(l, op, r) = tree
    val app = typedApply(desugar.binop(l, op, r), pt)
    if (untpd.isLeftAssoc(op.name)) app
    else {
      val defs = new mutable.ListBuffer[Tree]
      def lift(app: Tree): Tree = (app: @unchecked) match {
        case Apply(fn, args) =>
          if (app.tpe.isError) app
          else tpd.cpy.Apply(app)(fn, LiftImpure.liftArgs(defs, fn.tpe, args))
        case Assign(lhs, rhs) =>
          tpd.cpy.Assign(app)(lhs, lift(rhs))
        case Block(stats, expr) =>
          tpd.cpy.Block(app)(stats, lift(expr))
      }
      Applications.wrapDefs(defs, lift(app))
    }
  }

  /** Retrieve symbol attached to given tree */
  protected def retrieveSym(tree: untpd.Tree)(implicit ctx: Context) = tree.removeAttachment(SymOfTree) match {
    case Some(sym) =>
      sym.ensureCompleted()
      sym
    case none =>
      NoSymbol
  }

  protected def localTyper(sym: Symbol): Typer = nestedTyper.remove(sym).get

  def typedUnadapted(initTree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typedUnadapted(initTree, pt, ctx.typerState.ownedVars)

  /** Typecheck tree without adapting it, returning a typed tree.
   *  @param initTree    the untyped tree
   *  @param pt          the expected result type
   *  @param locked      the set of type variables of the current typer state that cannot be interpolated
   *                     at the present time
   */
  def typedUnadapted(initTree: untpd.Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Tree = {
    record("typedUnadapted")
    val xtree = expanded(initTree)
    xtree.removeAttachment(TypedAhead) match {
      case Some(ttree) => ttree
      case none =>

        def typedNamed(tree: untpd.NameTree, pt: Type)(implicit ctx: Context): Tree = {
          val sym = retrieveSym(xtree)
          tree match {
            case tree: untpd.Ident => typedIdent(tree, pt)
            case tree: untpd.Select => typedSelect(tree, pt)
            case tree: untpd.Bind => typedBind(tree, pt)
            case tree: untpd.ValDef =>
              if (tree.isEmpty) tpd.EmptyValDef
              else typedValDef(tree, sym)(ctx.localContext(tree, sym).setNewScope)
            case tree: untpd.DefDef =>
              val typer1 = localTyper(sym)
              typer1.typedDefDef(tree, sym)(ctx.localContext(tree, sym).setTyper(typer1))
            case tree: untpd.TypeDef =>
              if (tree.isClassDef)
                typedClassDef(tree, sym.asClass)(ctx.localContext(tree, sym).setMode(ctx.mode &~ Mode.InSuperCall))
              else
                typedTypeDef(tree, sym)(ctx.localContext(tree, sym).setNewScope)
            case _ => typedUnadapted(desugar(tree), pt, locked)
          }
        }

        def typedUnnamed(tree: untpd.Tree): Tree = tree match {
          case tree: untpd.Apply =>
            if (ctx.mode is Mode.Pattern) typedUnApply(tree, pt) else typedApply(tree, pt)
          case tree: untpd.This => typedThis(tree)
          case tree: untpd.Literal => typedLiteral(tree)
          case tree: untpd.New => typedNew(tree, pt)
          case tree: untpd.Typed => typedTyped(tree, pt)
          case tree: untpd.NamedArg => typedNamedArg(tree, pt)
          case tree: untpd.Assign => typedAssign(tree, pt)
          case tree: untpd.Block => typedBlock(desugar.block(tree), pt)(ctx.fresh.setNewScope)
          case tree: untpd.If => typedIf(tree, pt)
          case tree: untpd.Function => typedFunction(tree, pt)
          case tree: untpd.Closure => typedClosure(tree, pt)
          case tree: untpd.Import => typedImport(tree, retrieveSym(tree))
          case tree: untpd.Match => typedMatch(tree, pt)
          case tree: untpd.Return => typedReturn(tree)
          case tree: untpd.Try => typedTry(tree, pt)
          case tree: untpd.Throw => typedThrow(tree)
          case tree: untpd.TypeApply => typedTypeApply(tree, pt)
          case tree: untpd.Super => typedSuper(tree, pt)
          case tree: untpd.SeqLiteral => typedSeqLiteral(tree, pt)
          case tree: untpd.Inlined => typedInlined(tree, pt)
          case tree: untpd.TypeTree => typedTypeTree(tree, pt)
          case tree: untpd.SingletonTypeTree => typedSingletonTypeTree(tree)
          case tree: untpd.AndTypeTree => typedAndTypeTree(tree)
          case tree: untpd.OrTypeTree => typedOrTypeTree(tree)
          case tree: untpd.RefinedTypeTree => typedRefinedTypeTree(tree)
          case tree: untpd.AppliedTypeTree => typedAppliedTypeTree(tree)
          case tree: untpd.LambdaTypeTree => typedLambdaTypeTree(tree)(ctx.localContext(tree, NoSymbol).setNewScope)
          case tree: untpd.ByNameTypeTree => typedByNameTypeTree(tree)
          case tree: untpd.TypeBoundsTree => typedTypeBoundsTree(tree, pt)
          case tree: untpd.Alternative => typedAlternative(tree, pt)
          case tree: untpd.PackageDef => typedPackageDef(tree)
          case tree: untpd.Annotated => typedAnnotated(tree, pt)
          case tree: untpd.TypedSplice => typedTypedSplice(tree)
          case tree: untpd.UnApply => typedUnApply(tree, pt)
          case tree: untpd.DependentTypeTree => typed(untpd.TypeTree().withPos(tree.pos), pt)
          case tree: untpd.InfixOp if ctx.mode.isExpr => typedInfixOp(tree, pt)
          case tree @ untpd.PostfixOp(qual, Ident(nme.WILDCARD)) => typedAsFunction(tree, pt)
          case untpd.EmptyTree => tpd.EmptyTree
          case _ => typedUnadapted(desugar(tree), pt, locked)
        }

        val ifpt = defn.asImplicitFunctionType(pt)
        val result = if (ifpt.exists &&
            xtree.isTerm &&
            !untpd.isImplicitClosure(xtree) &&
            !ctx.mode.is(Mode.ImplicitShadowing) &&
            !ctx.mode.is(Mode.Pattern) &&
            !ctx.isAfterTyper)
          makeImplicitFunction(xtree, ifpt)
        else xtree match {
          case xtree: untpd.NameTree => typedNamed(xtree, pt)
          case xtree => typedUnnamed(xtree)
        }
        simplify(result, pt, locked)
    }
  }

  /** Interpolate and simplify the type of the given tree. */
  protected def simplify(tree: Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): tree.type = {
    if (!tree.denot.isOverloaded) // for overloaded trees: resolve overloading before simplifying
      if (!tree.tpe.widen.isInstanceOf[MethodOrPoly] // wait with simplifying until method is fully applied
          || tree.isDef)                             // ... unless tree is a definition
      {
        interpolateTypeVars(tree, pt, locked)
        tree.overwriteType(tree.tpe.simplified)
      }
    tree
  }

  protected def makeImplicitFunction(tree: untpd.Tree, pt: Type)(implicit ctx: Context): Tree = {
    val defn.FunctionOf(formals, _, true, _) = pt.dropDependentRefinement
    val ifun = desugar.makeImplicitFunction(formals, tree)
    typr.println(i"make implicit function $tree / $pt ---> $ifun")
    typed(ifun, pt)
  }

  /** Typecheck and adapt tree, returning a typed tree. Parameters as for `typedUnadapted` */
  def typed(tree: untpd.Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Tree =
    trace(i"typing $tree", typr, show = true) {
      record(s"typed $getClass")
      record("typed total")
      assertPositioned(tree)
      try adapt(typedUnadapted(tree, pt, locked), pt, locked)
      catch {
        case ex: TypeError =>
          errorTree(tree, ex.toMessage, tree.pos.focus)
          // This uses tree.pos.focus instead of the default tree.pos, because:
          // - since tree can be a top-level definition, tree.pos can point to the whole definition
          // - that would in turn hide all other type errors inside tree.
          // TODO: might be even better to store positions inside TypeErrors.
      }
    }

  def typed(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, pt, ctx.typerState.ownedVars)

  def typedTrees(trees: List[untpd.Tree])(implicit ctx: Context): List[Tree] =
    trees mapconserve (typed(_))

  def typedStats(stats: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[tpd.Tree] = {
    val buf = new mutable.ListBuffer[Tree]
    val enumContexts = new mutable.HashMap[Symbol, Context]
      // A map from `enum` symbols to the contexts enclosing their definitions
    @tailrec def traverse(stats: List[untpd.Tree])(implicit ctx: Context): List[Tree] = stats match {
      case (imp: untpd.Import) :: rest =>
        val imp1 = typed(imp)
        buf += imp1
        traverse(rest)(ctx.importContext(imp, imp1.symbol))
      case (mdef: untpd.DefTree) :: rest =>
        mdef.removeAttachment(ExpandedTree) match {
          case Some(xtree) =>
            traverse(xtree :: rest)
          case none =>
            typed(mdef) match {
              case mdef1: DefDef if Inliner.hasBodyToInline(mdef1.symbol) =>
                buf += inlineExpansion(mdef1)
              case mdef1 =>
                import untpd.modsDeco
                mdef match {
                  case mdef: untpd.TypeDef if mdef.mods.isEnumClass =>
                    enumContexts(mdef1.symbol) = ctx
                  case _ =>
                }
                buf += mdef1
            }
            traverse(rest)
        }
      case Thicket(stats) :: rest =>
        traverse(stats ++ rest)
      case stat :: rest =>
        val stat1 = typed(stat)(ctx.exprContext(stat, exprOwner))
        if (!ctx.isAfterTyper && isPureExpr(stat1) && !stat1.tpe.isRef(defn.UnitClass))
          ctx.warning(em"a pure expression does nothing in statement position", stat.pos)
        buf += stat1
        traverse(rest)
      case nil =>
        buf.toList
    }
    val localCtx = {
      val exprOwnerOpt = if (exprOwner == ctx.owner) None else Some(exprOwner)
      ctx.withProperty(ExprOwner, exprOwnerOpt)
    }
    checkEnumCompanions(traverse(stats)(localCtx), enumContexts)
  }

  /** Given an inline method `mdef`, the method rewritten so that its body
   *  uses accessors to access non-public members.
   *  Overwritten in Retyper to return `mdef` unchanged.
   */
  protected def inlineExpansion(mdef: DefDef)(implicit ctx: Context): Tree =
    tpd.cpy.DefDef(mdef)(rhs = Inliner.bodyToInline(mdef.symbol))

  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, pt)(ctx retractMode Mode.PatternOrTypeBits)
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = // todo: retract mode between Type and Pattern?
    typed(tree, pt)(ctx addMode Mode.Type)
  def typedPattern(tree: untpd.Tree, selType: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, selType)(ctx addMode Mode.Pattern)

  def tryEither[T](op: Context => T)(fallBack: (T, TyperState) => T)(implicit ctx: Context) = {
    val nestedCtx = ctx.fresh.setNewTyperState()
    val result = op(nestedCtx)
    if (nestedCtx.reporter.hasErrors)
      fallBack(result, nestedCtx.typerState)
    else {
      nestedCtx.typerState.commit()
      result
    }
  }

  /** Try `op1`, if there are errors, try `op2`, if `op2` also causes errors, fall back
   *  to errors and result of `op1`.
   */
  def tryAlternatively[T](op1: Context => T)(op2: Context => T)(implicit ctx: Context): T =
    tryEither(op1) { (failedVal, failedState) =>
      tryEither(op2) { (_, _) =>
        failedState.commit()
        failedVal
      }
    }

  /** Is `pt` a prototype of an `apply` selection, or a parameterless function yielding one? */
  def isApplyProto(pt: Type)(implicit ctx: Context): Boolean = pt match {
    case pt: SelectionProto => pt.name == nme.apply
    case pt: FunProto       => pt.args.isEmpty && isApplyProto(pt.resultType)
    case pt: IgnoredProto   => isApplyProto(pt.ignored)
    case _                  => false
  }

  /** Potentially add apply node or implicit conversions. Before trying either,
   *  if the function is applied to an empty parameter list (), we try
   *
   *  0th strategy: If `tree` overrides a nullary method, mark the prototype
   *                so that the argument is dropped and return `tree` itself.
   *                (but do this at most once per tree).
   *
   *  After that, two strategies are tried, and the first that is successful is picked.
   *  If neither of the strategies are successful, continues with`fallBack`.
   *
   *  1st strategy: Try to insert `.apply` so that the result conforms to prototype `pt`.
   *                This strategy is not tried if the prototype represents already
   *                another `.apply` or `.apply()` selection.
   *
   *  2nd strategy: If tree is a select `qual.name`, try to insert an implicit conversion
   *    around the qualifier part `qual` so that the result conforms to the expected type
   *    with wildcard result type.
   */
  def tryInsertApplyOrImplicit(tree: Tree, pt: ProtoType, locked: TypeVars)(fallBack: => Tree)(implicit ctx: Context): Tree = {

    def isMethod(tree: Tree) = tree.tpe match {
      case ref: TermRef => ref.denot.alternatives.forall(_.info.widen.isInstanceOf[MethodicType])
      case _ => false
    }

    def isSyntheticApply(tree: Tree): Boolean = tree match {
      case tree: Select => tree.getAttachment(InsertedApply).isDefined
      case Apply(fn, _) => fn.getAttachment(InsertedApply).isDefined
      case _ => false
    }

    def tryApply(implicit ctx: Context) = {
      val sel = typedSelect(untpd.Select(untpd.TypedSplice(tree), nme.apply), pt)
      sel.pushAttachment(InsertedApply, ())
      if (sel.tpe.isError) sel
      else try adapt(simplify(sel, pt, locked), pt, locked) finally sel.removeAttachment(InsertedApply)
    }

    def tryImplicit =
      tryInsertImplicitOnQualifier(tree, pt, locked).getOrElse(fallBack)

    pt match {
      case pt @ FunProto(Nil, _, _)
      if tree.symbol.allOverriddenSymbols.exists(_.info.isNullaryMethod) &&
         tree.getAttachment(DroppedEmptyArgs).isEmpty =>
        tree.putAttachment(DroppedEmptyArgs, ())
        pt.markAsDropped()
        tree
      case _ =>
        if (isApplyProto(pt) || isMethod(tree) || isSyntheticApply(tree)) tryImplicit
        else tryEither(tryApply(_))((_, _) => tryImplicit)
     }
  }

  /** If this tree is a select node `qual.name`, try to insert an implicit conversion
   *  `c` around `qual` so that `c(qual).name` conforms to `pt`.
   */
  def tryInsertImplicitOnQualifier(tree: Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Option[Tree] = trace(i"try insert impl on qualifier $tree $pt") {
    tree match {
      case Select(qual, name) if name != nme.CONSTRUCTOR =>
        val qualProto = SelectionProto(name, pt, NoViewsAllowed, privateOK = false)
        tryEither { implicit ctx =>
          val qual1 = adapt(qual, qualProto, locked)
          if ((qual eq qual1) || ctx.reporter.hasErrors) None
          else Some(typed(cpy.Select(tree)(untpd.TypedSplice(qual1), name), pt, locked))
        } { (_, _) => None
        }
      case _ => None
    }
  }

  /** Perform the following adaptations of expression, pattern or type `tree` wrt to
   *  given prototype `pt`:
   *  (1) Resolve overloading
   *  (2) Apply parameterless functions
   *  (3) Apply polymorphic types to fresh instances of their type parameters and
   *      store these instances in context.undetparams,
   *      unless followed by explicit type application.
   *  (4) Do the following to unapplied methods used as values:
   *  (4.1) If the method has only implicit parameters pass implicit arguments
   *  (4.2) otherwise, if `pt` is a function type and method is not a constructor,
   *        convert to function by eta-expansion,
   *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt`
   *        and it is not a constructor, apply it to ()
   *  otherwise issue an error
   *  (5) Convert constructors in a pattern as follows:
   *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
   *        instance of its primary constructor that is a subtype of the expected type.
   *  (5.2) If constructor refers to an extractor, convert to application of
   *        unapply or unapplySeq method.
   *
   *  (6) Convert all other types to TypeTree nodes.
   *  (7) When in TYPEmode but not FUNmode or HKmode, check that types are fully parameterized
   *      (7.1) In HKmode, higher-kinded types are allowed, but they must have the expected kind-arity
   *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
   *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
   *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
   *  (10) If the expected type is Byte, Short or Char, and the expression
   *      is an integer fitting in the range of that type, convert it to that type.
   *  (11) Widen numeric literals to their expected type, if necessary
   *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is scala.Unit.
   *  (13) When in mode EXPRmode, apply AnnotationChecker conversion if expected type is annotated.
   *  (14) When in mode EXPRmode, apply a view
   *  If all this fails, error
   *  Parameters as for `typedUnadapted`.
   */
  def adapt(tree: Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Tree = /*>|>*/ track("adapt") /*<|<*/ {
    def showWithType(x: Any) = x match {
      case tree: tpd.Tree @unchecked => i"$tree of type ${tree.tpe}"
      case _ => String.valueOf(x)
    }
    adapt1(tree, pt, locked)
  }

  def adapt(tree: Tree, pt: Type)(implicit ctx: Context): Tree = {
    adapt(tree, pt, ctx.typerState.ownedVars)
  }

  private def adapt1(tree: Tree, pt: Type, locked: TypeVars)(implicit ctx: Context): Tree = {
    assert(pt.exists)
    def methodStr = err.refStr(methPart(tree).tpe)

    def readapt(tree: Tree)(implicit ctx: Context) = adapt(tree, pt, locked)
    def readaptSimplified(tree: Tree)(implicit ctx: Context) = readapt(simplify(tree, pt, locked))

    def missingArgs(mt: MethodType) = {
      ctx.error(MissingEmptyArgumentList(methPart(tree).symbol), tree.pos)
      tree.withType(mt.resultType)
    }

      def adaptOverloaded(ref: TermRef) = {
        val altDenots = ref.denot.alternatives
        typr.println(i"adapt overloaded $ref with alternatives ${altDenots map (_.info)}%, %")
        val alts = altDenots.map(TermRef(ref.prefix, ref.name, _))
        resolveOverloaded(alts, pt) match {
          case alt :: Nil =>
            readaptSimplified(tree.withType(alt))
          case Nil =>
            def noMatches =
              errorTree(tree,
                em"""none of the ${err.overloadedAltsStr(altDenots)}
                    |match ${err.expectedTypeStr(pt)}""")
            def hasEmptyParams(denot: SingleDenotation) = denot.info.paramInfoss == ListOfNil
            pt match {
              case pt: FunProto =>
                tryInsertApplyOrImplicit(tree, pt, locked)(noMatches)
              case _ =>
                if (altDenots exists (_.info.paramInfoss == ListOfNil))
                  typed(untpd.Apply(untpd.TypedSplice(tree), Nil), pt, locked)
                else
                  noMatches
            }
          case alts =>
            val remainingDenots = alts map (_.denot.asInstanceOf[SingleDenotation])
            errorTree(tree, AmbiguousOverload(tree, remainingDenots, pt)(err))
        }
      }

      def isUnary(tp: Type): Boolean = tp match {
        case tp: MethodicType =>
          tp.firstParamTypes match {
            case ptype :: Nil => !ptype.isRepeatedParam
            case _ => false
          }
        case tp: TermRef =>
          tp.denot.alternatives.forall(alt => isUnary(alt.info))
        case _ =>
          false
      }

      def adaptToArgs(wtp: Type, pt: FunProto): Tree = wtp match {
        case _: MethodOrPoly =>
          if (pt.args.lengthCompare(1) > 0 && isUnary(wtp) && ctx.canAutoTuple)
            adapt(tree, pt.tupled, locked)
          else
            tree
        case _ => tryInsertApplyOrImplicit(tree, pt, locked) {
          errorTree(tree, MethodDoesNotTakeParameters(tree))
        }
      }

    /** If `tp` is a TypeVar which is fully constrained (i.e. its upper bound `hi` conforms
     *  to its lower bound `lo`), replace `tp` by `hi`. This is necessary to
     *  keep the right constraints for some implicit search problems. The paradigmatic case
     *  is `implicitNums.scala`. Without the healing done in `followAlias`, we cannot infer
     *  implicitly[_3], where _2 is the typelevel number 3. The problem here is that if a
     *  prototype is, say, Succ[Succ[Zero]], we can infer that it's argument type is Succ[Zero].
     *  But if the prototype is N? >: Succ[Succ[Zero]] <: Succ[Succ[Zero]], the same
     *  decomposition does not work - we'd get a N?#M where M is the element type name of Succ
     *  instead.
     */
    def followAlias(tp: Type)(implicit ctx: Context): Type = {
      val constraint = ctx.typerState.constraint
      def inst(tp: Type): Type = tp match {
        case TypeBounds(lo, hi)
        if (lo eq hi) || ctx.test(implicit ctx => hi <:< lo) =>
          inst(lo)
        case tp: TypeParamRef =>
          constraint.typeVarOfParam(tp).orElse(tp)
        case _ => tp
      }
      tp match {
        case tp: TypeVar if constraint.contains(tp) => inst(constraint.entry(tp.origin))
        case _ => tp
      }
    }

    def adaptNoArgsImplicitMethod(wtp: MethodType): Tree = {
      assert(wtp.isImplicitMethod)
      val tvarsToInstantiate = tvarsInParams(tree, locked).distinct
      wtp.paramInfos.foreach(instantiateSelected(_, tvarsToInstantiate))
      val constr = ctx.typerState.constraint

      def dummyArg(tp: Type) = untpd.Ident(nme.???).withTypeUnchecked(tp)

      def addImplicitArgs(implicit ctx: Context) = {
        def implicitArgs(formals: List[Type]): List[Tree] = formals match {
          case Nil => Nil
          case formal :: formals1 =>
            val arg = inferImplicitArg(formal, tree.pos.endPos)
            arg.tpe match {
              case failed: SearchFailureType
              if !failed.isInstanceOf[AmbiguousImplicits] && !tree.symbol.hasDefaultParams =>
                // no need to search further, the adapt fails in any case
                // the reason why we continue inferring arguments in case of an AmbiguousImplicits
                // is that we need to know whether there are further errors.
                // If there are none, we have to propagate the ambiguity to the caller.
                arg :: formals1.map(dummyArg)
              case _ =>
                arg :: implicitArgs(formals1)
            }
        }
        def eraseErasedArgs(args: List[Tree]): List[Tree] = {
          if (!wtp.isErasedMethod) args
          else args.map { arg =>
            arg.tpe match {
              case tpe if tpe.isStable => arg
              case _: AmbiguousImplicits => arg
              case tpe => defaultValue(tpe)
            }
          }
        }
        val args = eraseErasedArgs(implicitArgs(wtp.paramInfos))


        def propagatedFailure(args: List[Tree]): Type = args match {
          case arg :: args1 =>
            arg.tpe match {
              case ambi: AmbiguousImplicits =>
                propagatedFailure(args1) match {
                  case NoType | (_: AmbiguousImplicits) => ambi
                  case failed => failed
                }
              case failed: SearchFailureType => failed
              case _ => propagatedFailure(args1)
            }
          case Nil => NoType
        }

        val propFail = propagatedFailure(args)

        def issueErrors(): Tree = {
          (wtp.paramNames, wtp.paramInfos, args).zipped.foreach { (paramName, formal, arg) =>
            arg.tpe match {
              case failure: SearchFailureType =>
                ctx.error(
                  missingArgMsg(arg, formal, implicitParamString(paramName, methodStr, tree)),
                  tree.pos.endPos)
              case _ =>
            }
          }
          untpd.Apply(tree, args).withType(propFail)
        }

        if (propFail.exists) {
          // If there are several arguments, some arguments might already
          // have influenced the context, binding variables, but later ones
          // might fail. In that case the constraint needs to be reset.
          ctx.typerState.constraint = constr

          // If method has default params, fall back to regular application
          // where all inferred implicits are passed as named args.
          if (methPart(tree).symbol.hasDefaultParams && !propFail.isInstanceOf[AmbiguousImplicits]) {
            val namedArgs = (wtp.paramNames, args).zipped.flatMap { (pname, arg) =>
              if (arg.tpe.isError) Nil else untpd.NamedArg(pname, untpd.TypedSplice(arg)) :: Nil
            }
            tryEither { implicit ctx =>
              typed(untpd.Apply(untpd.TypedSplice(tree), namedArgs), pt, locked)
            } { (_, _) =>
              issueErrors()
            }
          } else issueErrors()
        }
        else readaptSimplified(tpd.Apply(tree, args))
      }
      addImplicitArgs(argCtx(tree))
    }

    /** A synthetic apply should be eta-expanded if it is the apply of an implicit function
      *  class, and the expected type is a function type. This rule is needed so we can pass
      *  an implicit function to a regular function type. So the following is OK
      *
      *     val f: implicit A => B  =  ???
      *     val g: A => B = f
      *
      *  and the last line expands to
      *
      *     val g: A => B  =  (x$0: A) => f.apply(x$0)
      *
      *  One could be tempted not to eta expand the rhs, but that would violate the invariant
      *  that expressions of implicit function types are always implicit closures, which is
      *  exploited by ShortcutImplicits.
      *
      *  On the other hand, the following would give an error if there is no implicit
      *  instance of A available.
      *
      *     val x: AnyRef = f
      *
      *  That's intentional, we want to fail here, otherwise some unsuccesful implicit searches
      *  would go undetected.
      *
      *  Examples for these cases are found in run/implicitFuns.scala and neg/i2006.scala.
      */
    def adaptNoArgsUnappliedMethod(wtp: MethodType, functionExpected: Boolean, arity: Int): Tree = {
      def isExpandableApply =
        defn.isImplicitFunctionClass(tree.symbol.maybeOwner) && functionExpected

      /** Is reference to this symbol `f` automatically expanded to `f()`? */
      def isAutoApplied(sym: Symbol): Boolean = {
        sym.isConstructor ||
        sym.matchNullaryLoosely ||
        ctx.testScala2Mode(MissingEmptyArgumentList(sym), tree.pos,
            patch(tree.pos.endPos, "()"))
      }

      // Reasons NOT to eta expand:
      //  - we reference a constructor
      //  - we are in a pattern
      //  - the current tree is a synthetic apply which is not expandable (eta-expasion would simply undo that)
      if (arity >= 0 &&
          !tree.symbol.isConstructor &&
          !ctx.mode.is(Mode.Pattern) &&
          !(isSyntheticApply(tree) && !isExpandableApply))
        simplify(typed(etaExpand(tree, wtp, arity), pt), pt, locked)
      else if (wtp.paramInfos.isEmpty && isAutoApplied(tree.symbol))
        readaptSimplified(tpd.Apply(tree, Nil))
      else if (wtp.isImplicitMethod)
        err.typeMismatch(tree, pt)
      else
        missingArgs(wtp)
    }

    def adaptNoArgsOther(wtp: Type) = {
      ctx.typeComparer.GADTused = false
      if (defn.isImplicitFunctionClass(wtp.underlyingClassRef(refinementOK = false).classSymbol) &&
          !untpd.isImplicitClosure(tree) &&
          !isApplyProto(pt) &&
          !ctx.mode.is(Mode.Pattern) &&
          !ctx.isAfterTyper) {
        typr.println(i"insert apply on implicit $tree")
        typed(untpd.Select(untpd.TypedSplice(tree), nme.apply), pt, locked)
      }
      else if (ctx.mode is Mode.Pattern) {
        checkEqualityEvidence(tree, pt)
        tree
      }
      else if (tree.tpe <:< pt) {
        if (pt.hasAnnotation(defn.InlineParamAnnot))
          checkInlineConformant(tree, isFinal = false, "argument to inline parameter")
        if (Inliner.hasBodyToInline(tree.symbol) &&
            !ctx.owner.ownersIterator.exists(_.isInlineableMethod) &&
            !ctx.settings.YnoInline.value &&
            !ctx.isAfterTyper &&
            !ctx.reporter.hasErrors)
          readaptSimplified(Inliner.inlineCall(tree, pt))
        else if (ctx.typeComparer.GADTused && pt.isValueType)
          // Insert an explicit cast, so that -Ycheck in later phases succeeds.
          // I suspect, but am not 100% sure that this might affect inferred types,
          // if the expected type is a supertype of the GADT bound. It would be good to come
          // up with a test case for this.
          tree.asInstance(pt)
        else
          tree
      }
      else wtp match {
        case wtp: MethodType => missingArgs(wtp)
        case _ =>
          typr.println(i"adapt to subtype ${tree.tpe} !<:< $pt")
          //typr.println(TypeComparer.explained(implicit ctx => tree.tpe <:< pt))
          adaptToSubType(wtp)
      }
    }

    // Follow proxies and approximate type paramrefs by their upper bound
    // in the current constraint in order to figure out robustly
    // whether an expected type is some sort of function type.
    def underlyingApplied(tp: Type): Type = tp.stripTypeVar match {
      case tp: RefinedType => tp
      case tp: AppliedType => tp
      case tp: TypeParamRef => underlyingApplied(ctx.typeComparer.bounds(tp).hi)
      case tp: TypeProxy => underlyingApplied(tp.superType)
      case _ => tp
    }

    def adaptNoArgs(wtp: Type): Tree = {
      val ptNorm = underlyingApplied(pt)
      lazy val functionExpected = defn.isFunctionType(ptNorm)
      lazy val resultMatch = constrainResult(wtp, followAlias(pt))
      wtp match {
        case wtp: ExprType =>
          readaptSimplified(tree.withType(wtp.resultType))
        case wtp: MethodType if wtp.isImplicitMethod && (resultMatch || !functionExpected) =>
          if (resultMatch || ctx.mode.is(Mode.ImplicitsEnabled)) adaptNoArgsImplicitMethod(wtp)
          else {
            // Don't proceed with implicit search if result type cannot match - the search
            // will likely be under-constrained, which means that an unbounded number of alternatives
            // is tried. See strawman-contrib MapDecoratorTest.scala for an example where this happens.
            err.typeMismatch(tree, pt)
          }
        case wtp: MethodType if !pt.isSingleton =>
          val arity =
            if (functionExpected)
              if (!isFullyDefined(pt, ForceDegree.none) && isFullyDefined(wtp, ForceDegree.none))
                // if method type is fully defined, but expected type is not,
                // prioritize method parameter types as parameter types of the eta-expanded closure
                0
              else defn.functionArity(ptNorm)
            else {
              val nparams = wtp.paramInfos.length
              if (nparams > 0 || pt.eq(AnyFunctionProto)) nparams
              else -1 // no eta expansion in this case
            }
            adaptNoArgsUnappliedMethod(wtp, functionExpected, arity)
        case _ =>
          adaptNoArgsOther(wtp)
      }
    }

    /** Adapt an expression of constant type to a different constant type `tpe`. */
    def adaptConstant(tree: Tree, tpe: ConstantType): Tree = {
      def lit = Literal(tpe.value).withPos(tree.pos)
      tree match {
        case Literal(c) => lit
        case tree @ Block(stats, expr) => tpd.cpy.Block(tree)(stats, adaptConstant(expr, tpe))
        case tree =>
          if (isIdempotentExpr(tree)) lit // See discussion in phase Literalize why we demand isIdempotentExpr
          else Block(tree :: Nil, lit)
      }
    }

    def toSAM(tree: Tree): Tree = tree match {
      case tree: Block => tpd.cpy.Block(tree)(tree.stats, toSAM(tree.expr))
      case tree: Closure => cpy.Closure(tree)(tpt = TypeTree(pt)).withType(pt)
    }

    def adaptToSubType(wtp: Type): Tree = {
      // try converting a constant to the target type
      val folded = ConstFold(tree, pt)
      if (folded ne tree) return adaptConstant(folded, folded.tpe.asInstanceOf[ConstantType])
      // drop type if prototype is Unit
      if (pt isRef defn.UnitClass)
        // local adaptation makes sure every adapted tree conforms to its pt
        // so will take the code path that decides on inlining
        return tpd.Block(adapt(tree, WildcardType, locked) :: Nil, Literal(Constant(())))
      // convert function literal to SAM closure
      tree match {
        case closure(Nil, id @ Ident(nme.ANON_FUN), _)
        if defn.isFunctionType(wtp) && !defn.isFunctionType(pt) =>
          pt match {
            case SAMType(sam)
            if wtp <:< sam.toFunctionType() =>
              // was ... && isFullyDefined(pt, ForceDegree.noBottom)
              // but this prevents case blocks from implementing polymorphic partial functions,
              // since we do not know the result parameter a priori. Have to wait until the
              // body is typechecked.
              return toSAM(tree)
            case _ =>
          }
        case _ =>
      }
      // try an implicit conversion
      val prevConstraint = ctx.typerState.constraint
      def recover(failure: SearchFailureType) =
        if (isFullyDefined(wtp, force = ForceDegree.all) &&
            ctx.typerState.constraint.ne(prevConstraint)) readapt(tree)
        else err.typeMismatch(tree, pt, failure)
      if (ctx.mode.is(Mode.ImplicitsEnabled) && tree.typeOpt.isValueType)
        inferView(tree, pt) match {
          case SearchSuccess(inferred, _, _) =>
            checkImplicitConversionUseOK(inferred.symbol, tree.pos)
            readapt(inferred)(ctx.retractMode(Mode.ImplicitsEnabled))
          case failure: SearchFailure =>
            if (pt.isInstanceOf[ProtoType] && !failure.isAmbiguous)
              // don't report the failure but return the tree unchanged. This
              // will cause a failure at the next level out, which usually gives
              // a better error message.
              tree
            else recover(failure.reason)
        }
      else recover(NoMatchingImplicits)
    }

    def adaptType(tp: Type): Tree = {
      val tree1 =
        if ((pt eq AnyTypeConstructorProto) || tp.typeParamSymbols.isEmpty) tree
        else {
          val tp1 =
            if (ctx.compilationUnit.isJava)
              // Cook raw type
              AppliedType(tree.tpe, tp.typeParams.map(Function.const(TypeBounds.empty)))
            else
              // Eta-expand higher-kinded type
              tree.tpe.EtaExpand(tp.typeParamSymbols)
          tree.withType(tp1)
        }
      if ((ctx.mode is Mode.Pattern) || tree1.tpe <:< pt) tree1
      else err.typeMismatch(tree1, pt)
    }

    /** If tree has an error type but no errors are reported yet, issue
     *  the error message stored in the type.
     *  One way this can happen is if implicit search causes symbols and types
     *  to be completed. The types are stored by `typedAhead` so that they can be
     *  retrieved later and thus avoid duplication of typechecking work.
     *  But if the implicit search causing the `typedAhead` fails locally but
     *  another alternative succeeds we can be left with an ErrorType in the
     *  tree that went unreported. A scenario where this happens is i1802.scala.
     */
    def ensureReported(tp: Type) = tp match {
      case err: ErrorType if !ctx.reporter.errorsReported => ctx.error(err.msg, tree.pos)
      case _ =>
    }

    tree match {
      case _: MemberDef | _: PackageDef | _: Import | _: WithoutTypeOrPos[_] => tree
      case _ => tree.tpe.widen match {
        case tp: FlexType =>
          ensureReported(tp)
          tree
        case ref: TermRef =>
          pt match {
            case pt: FunProto
            if pt.args.lengthCompare(1) > 0 && isUnary(ref) && ctx.canAutoTuple =>
              adapt(tree, pt.tupled, locked)
            case _ =>
              adaptOverloaded(ref)
          }
        case poly: PolyType if !(ctx.mode is Mode.Type) =>
          if (pt.isInstanceOf[PolyProto]) tree
          else {
            var typeArgs = tree match {
              case Select(qual, nme.CONSTRUCTOR) => qual.tpe.widenDealias.argTypesLo.map(TypeTree)
              case _ => Nil
            }
            if (typeArgs.isEmpty) typeArgs = constrained(poly, tree)._2
            convertNewGenericArray(readapt(tree.appliedToTypeTrees(typeArgs)))
          }
        case wtp =>
          if (isStructuralTermSelect(tree)) readaptSimplified(handleStructural(tree))
          else pt match {
            case pt: FunProto =>
              adaptToArgs(wtp, pt)
            case pt: PolyProto =>
              tryInsertApplyOrImplicit(tree, pt, locked)(tree) // error will be reported in typedTypeApply
            case _ =>
              if (ctx.mode is Mode.Type) adaptType(tree.tpe)
              else adaptNoArgs(wtp)
          }
      }
    }
  }

  /** Check that `tree == x: pt` is typeable. Used when checking a pattern
    * against a selector of type `pt`. This implementation accounts for
    * user-defined definitions of `==`.
    *
    * Overwritten to no-op in ReTyper.
    */
  protected def checkEqualityEvidence(tree: tpd.Tree, pt: Type)(implicit ctx: Context) : Unit = {
    tree match {
      case _: RefTree | _: Literal
        if !isVarPattern(tree) &&
          !(tree.tpe <:< pt) (ctx.addMode(Mode.GADTflexible)) =>
        val cmp =
          untpd.Apply(
            untpd.Select(untpd.TypedSplice(tree), nme.EQ),
            untpd.TypedSplice(dummyTreeOfType(pt)))
        typedExpr(cmp, defn.BooleanType)
      case _ =>
    }
  }
}
