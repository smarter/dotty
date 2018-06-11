package dotty.tools.dotc
package transform

import core._
import Decorators._, Flags._, Types._, Contexts._, Symbols._, Constants._
import Flags._
import ast.Trees._
import ast.{TreeTypeMap, untpd}
import util.Positions._
import StdNames._
import tasty.TreePickler.Hole
import MegaPhase.MiniPhase
import SymUtils._
import NameKinds._
import dotty.tools.dotc.ast.tpd.Tree
import dotty.tools.dotc.core.DenotTransformers.InfoTransformer
import typer.Implicits.SearchFailureType

import scala.collection.mutable
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.quoted._


/** Translates quoted terms and types to `unpickle` method calls.
 *  Checks that the phase consistency principle (PCP) holds.
 *
 *
 *  Transforms top level quote
 *   ```
 *   '{ ...
 *      val x1 = ???
 *      val x2 = ???
 *      ...
 *      ~{ ... '{ ... x1 ... x2 ...} ... }
 *      ...
 *    }
 *    ```
 *  to
 *    ```
 *     unpickle(
 *       [[ // PICKLED TASTY
 *         ...
 *         val x1 = ???
 *         val x2 = ???
 *         ...
 *         Hole(0 | x1, x2)
 *         ...
 *       ]],
 *       List(
 *         (args: Seq[Any]) => {
 *           val x1$1 = args(0).asInstanceOf[Expr[T]]
 *           val x2$1 = args(1).asInstanceOf[Expr[T]] // can be asInstanceOf[Type[T]]
 *           ...
 *           { ... '{ ... x1$1.unary_~ ... x2$1.unary_~ ...} ... }
 *         }
 *       )
 *     )
 *    ```
 *  and then performs the same transformation on `'{ ... x1$1.unary_~ ... x2$1.unary_~ ...}`.
 *
 *
 *  For inline macro definitions we assume that we have a single ~ directly as the RHS.
 *  We will transform the definition from
 *    ```
 *    inline def foo[T1, ...](inline x1: X, ..., y1: Y, ....): Z = ~{ ... T1 ... x ... '(y) ... }
 *    ```
 *  to
 *    ```
 *    inline def foo[T1, ...](inline x1: X, ..., y1: Y, ....): Seq[Any] => Object = { (args: Seq[Any]) => {
 *      val T1$1 = args(0).asInstanceOf[Type[T1]]
 *      ...
 *      val x1$1 = args(0).asInstanceOf[X]
 *      ...
 *      val y1$1 = args(1).asInstanceOf[Expr[Y]]
 *      ...
 *      { ... x1$1 .... '{ ... T1$1.unary_~ ... x1$1.toExpr.unary_~ ... y1$1.unary_~ ... } ... }
 *    }
 *    ```
 *  Where `inline` parameters with type Boolean, Byte, Short, Int, Long, Float, Double, Char and String are
 *  passed as their actual runtime value. See `isStage0Value`. Other `inline` arguments such as functions are handled
 *  like `y1: Y`.
 *
 *  Note: the parameters of `foo` are kept for simple overloading resolution but they are not used in the body of `foo`.
 *
 *  At inline site we will call reflectively the static method `foo` with dummy parameters, which will return a
 *  precompiled version of the function that will evaluate the `Expr[Z]` that `foo` produces. The lambda is then called
 *  at the inline site with the lifted arguments of the inlined call.
 */
class ReifyQuotes extends MacroTransformWithImplicits with InfoTransformer {
  import ast.tpd._

  /** Classloader used for loading macros */
  private[this] var myMacroClassLoader: java.lang.ClassLoader = _
  private def macroClassLoader(implicit ctx: Context): ClassLoader = {
    if (myMacroClassLoader == null) {
      val urls = ctx.settings.classpath.value.split(':').map(cp => java.nio.file.Paths.get(cp).toUri.toURL)
      myMacroClassLoader = new java.net.URLClassLoader(urls, getClass.getClassLoader)
    }
    myMacroClassLoader
  }

  override def phaseName: String = "reifyQuotes"

  override def run(implicit ctx: Context): Unit =
    if (ctx.compilationUnit.containsQuotesOrSplices) super.run

  protected def newTransformer(implicit ctx: Context): Transformer =
    new Reifier(inQuote = false, null, 0, new LevelInfo, new mutable.ListBuffer[Tree])

  private class LevelInfo {
    /** A map from locally defined symbols to the staging levels of their definitions */
    val levelOf = new mutable.HashMap[Symbol, Int]

    /** Register a reference defined in a quote but used in another quote nested in a splice.
     *  Returns a version of the reference that needs to be used in its place.
     *     '{
     *        val x = ???
     *        { ... '{ ... x ... } ... }.unary_~
     *      }
     *  Eta expanding the `x` in `{ ... '{ ... x ... } ... }.unary_~` will return a `x$1.unary_~` for which the `x$1`
     *  be created by some outer reifier.
     *
     *  This transformation is only applied to definitions at staging level 1.
     *
     *  See `isCaptured`
     */
    val capturers = new mutable.HashMap[Symbol, Tree => Tree]
  }

  /** The main transformer class
   *  @param  inQuote    we are within a `'(...)` context that is not shadowed by a nested `~(...)`
   *  @param  outer      the next outer reifier, null is this is the topmost transformer
   *  @param  level      the current level, where quotes add one and splices subtract one level
   *  @param  levels     a stacked map from symbols to the levels in which they were defined
   *  @param  embedded   a list of embedded quotes (if `inSplice = true`) or splices (if `inQuote = true`
   */
  private class Reifier(inQuote: Boolean, val outer: Reifier, val level: Int, levels: LevelInfo,
      val embedded: mutable.ListBuffer[Tree]) extends ImplicitsTransformer {
    import levels._
    assert(level >= 0)

    /** A nested reifier for a quote (if `isQuote = true`) or a splice (if not) */
    def nested(isQuote: Boolean): Reifier = {
      val nestedEmbedded = if (level > 1 || (level == 1 && isQuote)) embedded else new mutable.ListBuffer[Tree]
      new Reifier(isQuote, this, if (isQuote) level + 1 else level - 1, levels, nestedEmbedded)
    }

    /** We are in a `~(...)` context that is not shadowed by a nested `'(...)` */
    def inSplice = outer != null && !inQuote

    /** We are not in a `~(...)` or a `'(...)` */
    def isRoot = outer == null

    /** A map from type ref T to expressions of type `quoted.Type[T]`".
     *  These will be turned into splices using `addTags` and represent type variables
     *  that can be possibly healed.
     */
    val importedTags = new mutable.LinkedHashMap[TypeRef, Tree]()

    /** A map from type ref T to expressions of type `quoted.Type[T]`" like `importedTags`
      * These will be turned into splices using `addTags` and represent types spliced
      * explicitly.
      */
    val explicitTags = new mutable.LinkedHashSet[TypeRef]()

    /** A stack of entered symbols, to be unwound after scope exit */
    var enteredSyms: List[Symbol] = Nil

    /** Assuming importedTags = `Type1 -> tag1, ..., TypeN -> tagN`, the expression
     *
     *      { type <Type1> = <tag1>.unary_~
     *        ...
     *        type <TypeN> = <tagN>.unary_~
     *        <expr>
     *      }
     *
     *  references to `TypeI` in `expr` are rewired to point to the locally
     *  defined versions. As a side effect, prepend the expressions `tag1, ..., `tagN`
     *  as splices to `embedded`.
     */
    private def addTags(expr: Tree)(implicit ctx: Context): Tree = {

      def mkTagSymbolAndAssignType(typeRef: TypeRef, tag: Tree): Tree = {
        val rhs = transform(tag.select(tpnme.UNARY_~))
        val alias = ctx.typeAssigner.assignType(untpd.TypeBoundsTree(rhs, rhs), rhs, rhs)

        val original = typeRef.symbol.asType

        val local = ctx.newSymbol(
          owner = ctx.owner,
          name = UniqueName.fresh("T".toTermName).toTypeName,
          flags = Synthetic,
          info = TypeAlias(tag.tpe.select(tpnme.UNARY_~)),
          coord = typeRef.prefix.termSymbol.coord).asType

        ctx.typeAssigner.assignType(untpd.TypeDef(local.name, alias), local)
      }

      if (importedTags.isEmpty && explicitTags.isEmpty) expr
      else {
        val itags = importedTags.toList
        // The tree of the tag for each tag comes from implicit search in `tryHeal`
        val typeDefs = for ((tref, tag) <- itags) yield {
          mkTagSymbolAndAssignType(tref, tag)
        }
        importedTags.clear()

        // The tree of the tag for each tag comes from a type ref e.g., ~t
        val explicitTypeDefs = for (tref <- explicitTags) yield {
          val tag = ref(tref.prefix.termSymbol)
          mkTagSymbolAndAssignType(tref, tag)
        }
        val tagsExplicitTypeDefsPairs = explicitTags.zip(explicitTypeDefs)
        explicitTags.clear()

        // Maps type splices to type references of tags e.g., ~t -> some type T$1
        val map: Map[Type, Type] = {
          tagsExplicitTypeDefsPairs.map(x => (x._1, x._2.symbol.typeRef)) ++
          (itags.map(_._1) zip typeDefs.map(_.symbol.typeRef))
        }.toMap
        val tMap = new TypeMap() {
          override def apply(tp: Type): Type = map.getOrElse(tp, mapOver(tp))
        }

        Block(typeDefs ++ explicitTypeDefs,
          new TreeTypeMap(
            typeMap = tMap,
            substFrom = itags.map(_._1.symbol),
            substTo = typeDefs.map(_.symbol)
          ).apply(expr))
      }
    }

    /** Enter staging level of symbol defined by `tree`, if applicable. */
    def markDef(tree: Tree)(implicit ctx: Context) = tree match {
      case tree: DefTree =>
        val sym = tree.symbol
        if ((sym.isClass || !sym.maybeOwner.isType) && !levelOf.contains(sym)) {
          levelOf(sym) = level
          enteredSyms = sym :: enteredSyms
        }
      case _ =>
    }

    /** Does the level of `sym` match the current level?
     *  An exception is made for inline vals in macros. These are also OK if their level
     *  is one higher than the current level, because on execution such values
     *  are constant expression trees and we can pull out the constant from the tree.
     */
    def levelOK(sym: Symbol)(implicit ctx: Context): Boolean = levelOf.get(sym) match {
      case Some(l) =>
        l == level ||
        l == 1 && level == 0 && isStage0Value(sym)
      case None =>
        !sym.is(Param) || levelOK(sym.owner)
    }

    /** Issue a "splice outside quote" error unless we ar in the body of an inline method */
    def spliceOutsideQuotes(pos: Position)(implicit ctx: Context): Unit =
      ctx.error(i"splice outside quotes", pos)

    /** Try to heal phase-inconsistent reference to type `T` using a local type definition.
     *  @return None      if successful
     *  @return Some(msg) if unsuccessful where `msg` is a potentially empty error message
     *                    to be added to the "inconsistent phase" message.
     */
    def tryHeal(tp: Type, pos: Position)(implicit ctx: Context): Option[String] = tp match {
      case tp: TypeRef =>
        if (level == 0) {
          assert(ctx.owner.ownersIterator.exists(_.is(Macro)))
          None
        } else {
          val reqType = defn.QuotedTypeType.appliedTo(tp)
          val tag = ctx.typer.inferImplicitArg(reqType, pos)
          tag.tpe match {
            case fail: SearchFailureType =>
              Some(i"""
                      |
                      | The access would be accepted with the right type tag, but
                      | ${ctx.typer.missingArgMsg(tag, reqType, "")}""")
            case _ =>
              importedTags(tp) = nested(isQuote = false).transform(tag)
              None
          }
        }
      case _ =>
        Some("")
    }

    /** Check reference to `sym` for phase consistency, where `tp` is the underlying type
     *  by which we refer to `sym`.
     */
    def check(sym: Symbol, tp: Type, pos: Position)(implicit ctx: Context): Unit = {
      val isThis = tp.isInstanceOf[ThisType]
      def symStr =
        if (!isThis) sym.show
        else if (sym.is(ModuleClass)) sym.sourceModule.show
        else i"${sym.name}.this"
      if (!isThis && sym.maybeOwner.isType && !sym.is(Param))
        check(sym.owner, sym.owner.thisType, pos)
      else if (level == 1 && sym.isType && sym.is(Param) && sym.owner.is(Macro) && !outer.isRoot)
        importedTags(sym.typeRef) = capturers(sym)(ref(sym))
      else if (sym.exists && !sym.isStaticOwner && !levelOK(sym))
        for (errMsg <- tryHeal(tp, pos))
          ctx.error(em"""access to $symStr from wrong staging level:
                        | - the definition is at level ${levelOf.getOrElse(sym, 0)},
                        | - but the access is at level $level.$errMsg""", pos)
    }

    /** Check all named types and this-types in a given type for phase consistency. */
    def checkType(pos: Position)(implicit ctx: Context): TypeAccumulator[Unit] = new TypeAccumulator[Unit] {
      def apply(acc: Unit, tp: Type): Unit = reporting.trace(i"check type level $tp at $level") {
        tp match {
          case tp: TypeRef if tp.symbol.isSplice =>
            if (inQuote) {
              explicitTags += tp
              outer.checkType(pos).foldOver(acc, tp)
            }
            else {
              if (tp.isTerm) spliceOutsideQuotes(pos)
              tp
            }
          case tp: NamedType =>
            check(tp.symbol, tp, pos)
            if (!tp.symbol.is(Param))
              foldOver(acc, tp)
          case tp: ThisType =>
            check(tp.cls, tp, pos)
            foldOver(acc, tp)
          case _ =>
            foldOver(acc, tp)
        }
      }
    }

    /** If `tree` refers to a locally defined symbol (either directly, or in a pickled type),
     *  check that its staging level matches the current level. References to types
     *  that are phase-incorrect can still be healed as follows:
     *
     *  If `T` is a reference to a type at the wrong level, heal it by setting things up
     *  so that we later add a type definition
     *
     *     type T' = ~quoted.Type[T]
     *
     *  to the quoted text and rename T to T' in it. This is done later in `reify` via
     *  `addTags`. `checkLevel` itself only records what needs to be done in the
     *  `typeTagOfRef` field of the current `Splice` structure.
     */
    private def checkLevel(tree: Tree)(implicit ctx: Context): Tree = {
      tree match {
        case (_: Ident) | (_: This) =>
          check(tree.symbol, tree.tpe, tree.pos)
        case (_: UnApply)  | (_: TypeTree) =>
          checkType(tree.pos).apply((), tree.tpe)
        case Select(qual, OuterSelectName(_, levels)) =>
          checkType(tree.pos).apply((), tree.tpe.widen)
        case _: Bind =>
          checkType(tree.pos).apply((), tree.symbol.info)
        case _: Template =>
          checkType(tree.pos).apply((), tree.symbol.owner.asClass.givenSelfType)
        case _ =>
      }
      tree
    }

    /** Split `body` into a core and a list of embedded splices.
     *  Then if inside a splice, make a hole from these parts.
     *  If outside a splice, generate a call tp `scala.quoted.Unpickler.unpickleType` or
     *  `scala.quoted.Unpickler.unpickleExpr` that matches `tpe` with
     *  core and splices as arguments.
     */
    private def quotation(body: Tree, quote: Tree)(implicit ctx: Context): Tree = {
      val isType = quote.symbol eq defn.QuotedType_apply
      if (body.symbol.isSplice) {
        // simplify `'(~x)` to `x` and then transform it
        val Select(splice, _) = body
        transform(splice)
      }
      else if (level > 0) {
        val body1 = nested(isQuote = true).transform(body)
        // Keep quotes as trees to reduce pickled size and have a Expr.show without pickled quotes
        if (isType) ref(defn.QuotedType_apply).appliedToType(body1.tpe.widen)
        else ref(defn.QuotedExpr_apply).appliedToType(body1.tpe.widen).appliedTo(body1)
      }
      else body match {
        case body: RefTree if isCaptured(body.symbol, level + 1) =>
          if (isStage0Value(body.symbol)) {
            // Optimization: avoid the full conversion when capturing inlined `x`
            // in '{ x } to '{ x$1.toExpr.unary_~ } and go directly to `x$1.toExpr`
            liftValue(capturers(body.symbol)(body))
          } else {
            // Optimization: avoid the full conversion when capturing `x`
            // in '{ x } to '{ x$1.unary_~ } and go directly to `x$1`
            capturers(body.symbol)(body)
          }
        case _=>
          val (body1, splices) = nested(isQuote = true).split(body)
          pickledQuote(body1, splices, body.tpe, isType).withPos(quote.pos)
      }
    }

    private def pickledQuote(body: Tree, splices: List[Tree], originalTp: Type, isType: Boolean)(implicit ctx: Context) = {
      def pickleAsValue[T](value: T) =
        ref(defn.Unpickler_liftedExpr).appliedToType(originalTp.widen).appliedTo(Literal(Constant(value)))
      def pickleAsTasty() = {
        val meth =
          if (isType) ref(defn.Unpickler_unpickleType).appliedToType(originalTp)
          else ref(defn.Unpickler_unpickleExpr).appliedToType(originalTp.widen)
        meth.appliedTo(
          liftList(PickledQuotes.pickleQuote(body).map(x => Literal(Constant(x))), defn.StringType),
          liftList(splices, defn.AnyType))
      }
      if (splices.nonEmpty) pickleAsTasty()
      else if (isType) {
        def tag(tagName: String) = ref(defn.QuotedTypeModule).select(tagName.toTermName)
        if (body.symbol == defn.UnitClass) tag("UnitTag")
        else if (body.symbol == defn.BooleanClass) tag("BooleanTag")
        else if (body.symbol == defn.ByteClass) tag("ByteTag")
        else if (body.symbol == defn.CharClass) tag("CharTag")
        else if (body.symbol == defn.ShortClass) tag("ShortTag")
        else if (body.symbol == defn.IntClass) tag("IntTag")
        else if (body.symbol == defn.LongClass) tag("LongTag")
        else if (body.symbol == defn.FloatClass) tag("FloatTag")
        else if (body.symbol == defn.DoubleClass) tag("DoubleTag")
        else pickleAsTasty()
      }
      else ReifyQuotes.toValue(body) match {
        case Some(value) => pickleAsValue(value)
        case _ => pickleAsTasty()
      }
    }

    /** If inside a quote, split the body of the splice into a core and a list of embedded quotes
     *  and make a hole from these parts. Otherwise issue an error, unless we
     *  are in the body of an inline method.
     */
    private def splice(splice: Select)(implicit ctx: Context): Tree = {
      if (level > 1) {
        val body1 = nested(isQuote = false).transform(splice.qualifier)
        body1.select(splice.name)
      }
      else if (!inQuote && level == 0) {
        spliceOutsideQuotes(splice.pos)
        splice
      }
      else {
        val (body1, quotes) = nested(isQuote = false).split(splice.qualifier)
        makeHole(body1, quotes, splice.tpe).withPos(splice.pos)
      }
    }

    /** Transforms the contents of a nested splice
     *  Assuming
     *     '{
     *        val x = ???
     *        val y = ???
     *        { ... '{ ... x .. y ... } ... }.unary_~
     *      }
     *  then the spliced subexpression
     *     { ... '{ ... x ... y ... } ... }
     *  will be transformed to
     *     (args: Seq[Any]) => {
     *       val x$1 = args(0).asInstanceOf[Expr[Any]] // or .asInstanceOf[Type[Any]]
     *       val y$1 = args(1).asInstanceOf[Expr[Any]] // or .asInstanceOf[Type[Any]]
     *       { ... '{ ... x$1.unary_~ ... y$1.unary_~ ... } ... }
     *     }
     *
     *  See: `capture`
     *
     *  At the same time register `embedded` trees `x` and `y` to place as arguments of the hole
     *  placed in the original code.
     *     '{
     *        val x = ???
     *        val y = ???
     *        Hole(0 | x, y)
     *      }
     */
    private def makeLambda(tree: Tree)(implicit ctx: Context): Tree = {
      def body(arg: Tree)(implicit ctx: Context): Tree = {
        var i = 0
        transformWithCapturer(tree)(
          (captured: mutable.Map[Symbol, Tree]) => {
            (tree: Tree) => {
              def newCapture = {
                val tpw = tree.tpe.widen
                val argTpe =
                  if (tree.isType) defn.QuotedTypeType.appliedTo(tpw)
                  else if (isStage0Value(tree.symbol)) tpw
                  else defn.QuotedExprType.appliedTo(tpw)
                val selectArg = arg.select(nme.apply).appliedTo(Literal(Constant(i))).asInstance(argTpe)
                val capturedArg = SyntheticValDef(UniqueName.fresh(tree.symbol.name.toTermName).toTermName, selectArg)
                i += 1
                embedded += tree
                captured.put(tree.symbol, capturedArg)
                capturedArg
              }
              ref(captured.getOrElseUpdate(tree.symbol, newCapture).symbol)
            }
          }
        )
      }

      val lambdaOwner = ctx.owner.ownersIterator.find(o => levelOf.getOrElse(o, level) == level).get
      val tpe = MethodType(defn.SeqType.appliedTo(defn.AnyType) :: Nil, tree.tpe.widen)
      val meth = ctx.newSymbol(lambdaOwner, UniqueName.fresh(nme.ANON_FUN), Synthetic | Method, tpe)
      Closure(meth, tss => body(tss.head.head)(ctx.withOwner(meth)).changeOwner(ctx.owner, meth))
    }

    private def transformWithCapturer(tree: Tree)(capturer: mutable.Map[Symbol, Tree] => Tree => Tree)(implicit ctx: Context): Tree = {
      val captured = mutable.LinkedHashMap.empty[Symbol, Tree]
      val captured2 = capturer(captured)

      def registerCapturer(sym: Symbol): Unit = capturers.put(sym, captured2)
      def forceCapture(sym: Symbol): Unit = captured2(ref(sym))

      outer.enteredSyms.foreach(registerCapturer)

      if (ctx.owner.owner.is(Macro)) {
        registerCapturer(defn.TastyUniverse_compilationUniverse)
        // Force a macro to have the context in first position
        forceCapture(defn.TastyUniverse_compilationUniverse)
        // Force all parameters of the macro to be created in the definition order
        outer.enteredSyms.reverse.foreach(forceCapture)
      }

      val tree2 = transform(tree)
      capturers --= outer.enteredSyms

      seq(captured.result().valuesIterator.toList, tree2)
    }

    /** Returns true if this tree will be captured by `makeLambda` */
    private def isCaptured(sym: Symbol, level: Int)(implicit ctx: Context): Boolean = {
      // Check phase consistency and presence of capturer
      ( (level == 1 && levelOf.get(sym).contains(1)) ||
        (level == 0 && isStage0Value(sym))
      ) && capturers.contains(sym)
    }

    /** Transform `tree` and return the resulting tree and all `embedded` quotes
     *  or splices as a pair, after performing the `addTags` transform.
     */
    private def split(tree: Tree)(implicit ctx: Context): (Tree, List[Tree]) = {
      val tree1 = if (inQuote) addTags(transform(tree)) else makeLambda(tree)
      (tree1, embedded.toList)
    }

    /** Register `body` as an `embedded` quote or splice
     *  and return a hole with `splices` as arguments and the given type `tpe`.
     */
    private def makeHole(body: Tree, splices: List[Tree], tpe: Type)(implicit ctx: Context): Hole = {
      val idx = embedded.length
      embedded += body
      Hole(idx, splices).withType(tpe).asInstanceOf[Hole]
    }

    override def transform(tree: Tree)(implicit ctx: Context): Tree =
      reporting.trace(i"reify $tree at $level", show = true) {
        def mapOverTree(lastEntered: List[Symbol]) =
          try super.transform(tree)
          finally
            while (enteredSyms ne lastEntered) {
              levelOf -= enteredSyms.head
              enteredSyms = enteredSyms.tail
            }
        tree match {
          case Quoted(quotedTree) =>
            quotation(quotedTree, tree)
          case tree: TypeTree if tree.tpe.typeSymbol.isSplice =>
            val splicedType = tree.tpe.asInstanceOf[TypeRef].prefix.termSymbol
            splice(ref(splicedType).select(tpnme.UNARY_~))
          case tree: Select if tree.symbol.isSplice =>
            splice(tree)
          case tree: RefTree if isCaptured(tree.symbol, level) =>
            val capturer = capturers(tree.symbol)
            def captureAndSplice(t: Tree) =
              splice(t.select(if (tree.isTerm) nme.UNARY_~ else tpnme.UNARY_~))
            if (!isStage0Value(tree.symbol)) captureAndSplice(capturer(tree))
            else if (level == 0) capturer(tree)
            else captureAndSplice(liftValue(capturer(tree)))
          case Block(stats, _) =>
            val last = enteredSyms
            stats.foreach(markDef)
            mapOverTree(last)
          case Inlined(call, bindings, InlineSplice(expansion @ Select(body, name))) =>
            assert(call.symbol.is(Macro))
            val tree2 =
              if (level == 0) {
                // Simplification of the call done in PostTyper for non-macros can also be performed now
                // see PostTyper `case Inlined(...) =>` for description of the simplification
                val call2 = Ident(call.symbol.topLevelClass.typeRef).withPos(call.pos)
                val spliced = Splicer.splice(body, call, bindings, tree.pos, macroClassLoader).withPos(tree.pos)
                if (ctx.reporter.hasErrors) EmptyTree
                else transform(cpy.Inlined(tree)(call2, bindings, spliced))
              }
              else super.transform(tree)

            // due to value-discarding which converts an { e } into { e; () })
            if (tree.tpe =:= defn.UnitType) Block(tree2 :: Nil, Literal(Constant(())))
            else tree2
          case _: Import =>
            tree
          case tree: DefDef if tree.symbol.is(Macro) && level == 0 =>
            tree.rhs match {
              case InlineSplice(_) =>
                if (!tree.symbol.isStatic)
                  ctx.error("Inline macro method must be a static method.", tree.pos)
                markDef(tree)
                val reifier = nested(isQuote = true)
                reifier.transform(tree) // Ignore output, we only need the its embedding
                assert(reifier.embedded.size == 1)
                val lambda = reifier.embedded.head
                // replace macro code by lambda used to evaluate the macro expansion
                cpy.DefDef(tree)(tpt = TypeTree(macroReturnType), rhs = lambda)
              case _ =>
                ctx.error(
                  """Malformed inline macro.
                    |
                    |Expected the ~ to be at the top of the RHS:
                    |  inline def foo(...): Int = ~impl(...)
                    |or
                    |  inline def foo(...): Int = ~{
                    |    val x = 1
                    |    impl(... x ...)
                    |  }
                  """.stripMargin, tree.rhs.pos)
                EmptyTree
            }
          case _ =>
            markDef(tree)
            checkLevel(mapOverTree(enteredSyms))
        }
      }

    private def liftValue(tree: Tree)(implicit ctx: Context): Tree = {
      val reqType = defn.QuotedLiftableType.appliedTo(tree.tpe.widen)
      val liftable = ctx.typer.inferImplicitArg(reqType, tree.pos)
      liftable.tpe match {
        case fail: SearchFailureType =>
          ctx.error(i"""
                  |
                  | The access would be accepted with the right Liftable, but
                  | ${ctx.typer.missingArgMsg(liftable, reqType, "")}""")
          EmptyTree
        case _ =>
          liftable.select("toExpr".toTermName).appliedTo(tree)
      }
    }

    private def isStage0Value(sym: Symbol)(implicit ctx: Context): Boolean =
      (sym.is(Inline) && sym.owner.is(Macro) && !defn.isFunctionType(sym.info)) ||
      sym == defn.TastyUniverse_compilationUniverse // intrinsic value at stage 0

    private def liftList(list: List[Tree], tpe: Type)(implicit ctx: Context): Tree = {
      list.foldRight[Tree](ref(defn.NilModule)) { (x, acc) =>
        acc.select("::".toTermName).appliedToType(tpe).appliedTo(x)
      }
    }

    /** InlineSplice is used to detect cases where the expansion
     *  consists of a (possibly multiple & nested) block or a sole expression.
     */
    object InlineSplice {
      def unapply(tree: Tree)(implicit ctx: Context): Option[Select] = {
        tree match {
          case expansion: Select if expansion.symbol.isSplice => Some(expansion)
          case Block(List(stat), Literal(Constant(()))) => unapply(stat)
          case Block(Nil, expr) => unapply(expr)
          case Typed(expr, tpt) => unapply(expr)
          case _ => None
        }
      }
    }
  }

  def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = {
    /** Transforms the return type of
     *    inline def foo(...): X = ~(...)
     *  to
     *    inline def foo(...): Seq[Any] => Expr[Any] = (args: Seq[Any]) => ...
     */
    def transform(tp: Type): Type = tp match {
      case tp: MethodType => MethodType(tp.paramNames, tp.paramInfos, transform(tp.resType))
      case tp: PolyType => PolyType(tp.paramNames, tp.paramInfos, transform(tp.resType))
      case tp: ExprType => ExprType(transform(tp.resType))
      case _ => macroReturnType
    }
    transform(tp)
  }

  override protected def mayChange(sym: Symbol)(implicit ctx: Context): Boolean =
    ctx.compilationUnit.containsQuotesOrSplices && sym.isTerm && sym.is(Macro)

  /** Returns the type of the compiled macro as a lambda: Seq[Any] => Object */
  private def macroReturnType(implicit ctx: Context): Type =
    defn.FunctionType(1).appliedTo(defn.SeqType.appliedTo(defn.AnyType), defn.ObjectType)
}

object ReifyQuotes {
  def toValue(tree: Tree): Option[Any] = tree match {
    case Literal(Constant(c)) => Some(c)
    case Block(Nil, e) => toValue(e)
    case Inlined(_, Nil, e) => toValue(e)
    case _ => None
  }
}
