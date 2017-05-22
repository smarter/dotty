package dotty.tools
package dotc
package ast

import core._
import util.Positions._, Types._, Contexts._, Constants._, Names._, NameOps._, Flags._
import Denotations._, SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._
import Decorators._
import util.Property
import language.higherKinds
import collection.mutable.ListBuffer
import reflect.ClassTag

object untpd extends Trees.Instance[Untyped] with UntypedTreeInfo {

  // ----- Tree cases that exist in untyped form only ------------------

  trait OpTree extends Tree {
    def op: Ident
    override def isTerm = op.name.isTermName
    override def isType = op.name.isTypeName
  }

  /** A typed subtree of an untyped tree needs to be wrapped in a TypedSlice
   *  @param owner  The current owner at the time the tree was defined
   */
  abstract case class TypedSplice(tree: tpd.Tree)(val owner: Symbol) extends ProxyTree {
    def forwardTo = tree
  }

  object TypedSplice {
    def apply(tree: tpd.Tree)(implicit ctx: Context): TypedSplice =
      new TypedSplice(tree)(ctx.owner) {}
  }

  /** mods object name impl */
  case class ModuleDef(name: TermName, impl: Template)
    extends MemberDef {
    type ThisTree[-T >: Untyped] <: Trees.NameTree[T] with Trees.MemberDef[T] with ModuleDef
    def withName(name: Name)(implicit ctx: Context) = cpy.ModuleDef(this)(name.toTermName, impl)
  }

  case class ParsedTry(expr: Tree, handler: Tree, finalizer: Tree) extends TermTree

  case class SymbolLit(str: String) extends TermTree

  /** An interpolated string
   *  @param segments  a list of two element tickets consisting of string literal and argument tree,
   *                   possibly with a simple string literal as last element of the list
   */
  case class InterpolatedString(id: TermName, segments: List[Tree]) extends TermTree

  case class Function(args: List[Tree], body: Tree) extends Tree {
    override def isTerm = body.isTerm
    override def isType = body.isType
  }

  /** An implicit function type */
  class ImplicitFunction(args: List[Tree], body: Tree) extends Function(args, body) {
    override def toString = s"ImplicitFunction($args, $body)"
  }

  /** A function created from a wildcard expression
   *  @param  placeHolderParams  a list of definitions of synthetic parameters
   *  @param  body               the function body where wildcards are replaced by
   *                             references to synthetic parameters.
   */
  class WildcardFunction(placeholderParams: List[ValDef], body: Tree) extends Function(placeholderParams, body)

  case class InfixOp(left: Tree, op: Ident, right: Tree) extends OpTree
  case class PostfixOp(od: Tree, op: Ident) extends OpTree
  case class PrefixOp(op: Ident, od: Tree) extends OpTree
  case class Parens(t: Tree) extends ProxyTree {
    def forwardTo = t
  }
  case class Tuple(trees: List[Tree]) extends Tree {
    override def isTerm = trees.isEmpty || trees.head.isTerm
    override def isType = !isTerm
  }
  case class Throw(expr: Tree) extends TermTree
  case class WhileDo(cond: Tree, body: Tree) extends TermTree
  case class DoWhile(body: Tree, cond: Tree) extends TermTree
  case class ForYield(enums: List[Tree], expr: Tree) extends TermTree
  case class ForDo(enums: List[Tree], body: Tree) extends TermTree
  case class GenFrom(pat: Tree, expr: Tree) extends Tree
  case class GenAlias(pat: Tree, expr: Tree) extends Tree
  case class ContextBounds(bounds: TypeBoundsTree, cxBounds: List[Tree]) extends TypTree
  case class PatDef(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) extends DefTree

  @sharable object EmptyTypeIdent extends Ident(tpnme.EMPTY) with WithoutTypeOrPos[Untyped] {
    override def isEmpty = true
  }

  /** A block arising from a right-associative infix operation, where, e.g.
   *
   *     a +: b
   *
   *  is expanded to
   *
   *     { val x = a; b.+:(x) }
   */
  class InfixOpBlock(leftOperand: Tree, rightOp: Tree) extends Block(leftOperand :: Nil, rightOp)

  // ----- Modifiers -----------------------------------------------------
  /** Mod is intended to record syntactic information about modifiers, it's
    * NOT a replacement of FlagSet.
    *
    * For any query about semantic information, check `flags` instead.
    */
  sealed abstract class Mod(val flags: FlagSet) extends Positioned

  object Mod {
    case class Private() extends Mod(Flags.Private)

    case class Protected() extends Mod(Flags.Protected)

    case class Val() extends Mod(Flags.EmptyFlags)

    case class Var() extends Mod(Flags.Mutable)

    case class Implicit() extends Mod(Flags.ImplicitCommon)

    case class Final() extends Mod(Flags.Final)

    case class Sealed() extends Mod(Flags.Sealed)

    case class Override() extends Mod(Flags.Override)

    case class Abstract() extends Mod(Flags.Abstract)

    case class Lazy() extends Mod(Flags.Lazy)

    case class Inline() extends Mod(Flags.Inline)

    case class Type() extends Mod(Flags.EmptyFlags)

    case class Enum() extends Mod(Flags.EmptyFlags)

    case class EnumCase() extends Mod(Flags.EmptyFlags)
  }

  /** Modifiers and annotations for definitions
    *
    *  @param flags          The set flags
   *  @param privateWithin  If a private or protected has is followed by a
   *                        qualifier [q], the name q, "" as a typename otherwise.
   *  @param annotations    The annotations preceding the modifiers
   */
  case class Modifiers (
    flags: FlagSet = EmptyFlags,
    privateWithin: TypeName = tpnme.EMPTY,
    annotations: List[Tree] = Nil,
    mods: List[Mod] = Nil) extends Positioned with Cloneable {

    def is(fs: FlagSet): Boolean = flags is fs
    def is(fc: FlagConjunction): Boolean = flags is fc
    def is(fc: FlagSet, butNot: FlagSet): Boolean = flags.is(fc, butNot = butNot)

    def | (fs: FlagSet): Modifiers = withFlags(flags | fs)
    def & (fs: FlagSet): Modifiers = withFlags(flags & fs)
    def &~(fs: FlagSet): Modifiers = withFlags(flags &~ fs)

    def toTypeFlags: Modifiers = withFlags(flags.toTypeFlags)
    def toTermFlags: Modifiers = withFlags(flags.toTermFlags)

    def withFlags(flags: FlagSet) =
      if (this.flags == flags) this
      else copy(flags = flags)

   def withAddedMod(mod: Mod): Modifiers =
     if (mods.exists(_ eq mod)) this
     else withMods(mods :+ mod)

   def withMods(ms: List[Mod]): Modifiers =
     if (mods eq ms) this
     else copy(mods = ms)

   def withAddedAnnotation(annot: Tree): Modifiers =
      if (annotations.exists(_ eq annot)) this
      else withAnnotations(annotations :+ annot)

    def withAnnotations(annots: List[Tree]): Modifiers =
      if (annots eq annotations) this
      else copy(annotations = annots)

    def withPrivateWithin(pw: TypeName) =
      if (pw.isEmpty) this
      else copy(privateWithin = pw)

    def hasFlags = flags != EmptyFlags
    def hasAnnotations = annotations.nonEmpty
    def hasPrivateWithin = privateWithin != tpnme.EMPTY
    def hasMod[T: ClassTag] = {
      val cls = implicitly[ClassTag[T]].runtimeClass
      mods.exists(mod => cls.isAssignableFrom(mod.getClass))
    }
  }

  @sharable val EmptyModifiers: Modifiers = new Modifiers()

  // ----- TypeTrees that refer to other tree's symbols -------------------

  /** A type tree that gets its type from some other tree's symbol. Enters the
   *  type tree in the References attachment of the `from` tree as a side effect.
   */
  abstract class DerivedTypeTree extends TypeTree {

    private var myWatched: Tree = EmptyTree

    /** The watched tree; used only for printing */
    def watched: Tree = myWatched

    /** Install the derived type tree as a dependency on `original` */
    def watching(original: DefTree): this.type = {
      myWatched = original
      val existing = original.attachmentOrElse(References, Nil)
      original.putAttachment(References, this :: existing)
      this
    }

    /** A hook to ensure that all necessary symbols are completed so that
     *  OriginalSymbol attachments are propagated to this tree
     */
    def ensureCompletions(implicit ctx: Context): Unit = ()

    /** The method that computes the type of this tree */
    def derivedType(originalSym: Symbol)(implicit ctx: Context): Type
  }

    /** Property key containing TypeTrees whose type is computed
   *  from the symbol in this type. These type trees have marker trees
   *  TypeRefOfSym or InfoOfSym as their originals.
   */
  val References = new Property.Key[List[Tree]]

  /** Property key for TypeTrees marked with TypeRefOfSym or InfoOfSym
   *  which contains the symbol of the original tree from which this
   *  TypeTree is derived.
   */
  val OriginalSymbol = new Property.Key[Symbol]

  // ------ Creation methods for untyped only -----------------

  def Ident(name: Name): Ident = new Ident(name)
  def BackquotedIdent(name: Name): BackquotedIdent = new BackquotedIdent(name)
  def Select(qualifier: Tree, name: Name): Select = new Select(qualifier, name)
  def SelectWithSig(qualifier: Tree, name: Name, sig: Signature): Select = new SelectWithSig(qualifier, name, sig)
  def This(qual: Ident): This = new This(qual)
  def Super(qual: Tree, mix: Ident): Super = new Super(qual, mix)
  def Apply(fun: Tree, args: List[Tree]): Apply = new Apply(fun, args)
  def TypeApply(fun: Tree, args: List[Tree]): TypeApply = new TypeApply(fun, args)
  def Literal(const: Constant): Literal = new Literal(const)
  def New(tpt: Tree): New = new New(tpt)
  def Typed(expr: Tree, tpt: Tree): Typed = new Typed(expr, tpt)
  def NamedArg(name: Name, arg: Tree): NamedArg = new NamedArg(name, arg)
  def Assign(lhs: Tree, rhs: Tree): Assign = new Assign(lhs, rhs)
  def Block(stats: List[Tree], expr: Tree): Block = new Block(stats, expr)
  def If(cond: Tree, thenp: Tree, elsep: Tree): If = new If(cond, thenp, elsep)
  def Closure(env: List[Tree], meth: Tree, tpt: Tree): Closure = new Closure(env, meth, tpt)
  def Match(selector: Tree, cases: List[CaseDef]): Match = new Match(selector, cases)
  def CaseDef(pat: Tree, guard: Tree, body: Tree): CaseDef = new CaseDef(pat, guard, body)
  def Return(expr: Tree, from: Tree): Return = new Return(expr, from)
  def Try(expr: Tree, cases: List[CaseDef], finalizer: Tree): Try = new Try(expr, cases, finalizer)
  def SeqLiteral(elems: List[Tree], elemtpt: Tree): SeqLiteral = new SeqLiteral(elems, elemtpt)
  def JavaSeqLiteral(elems: List[Tree], elemtpt: Tree): JavaSeqLiteral = new JavaSeqLiteral(elems, elemtpt)
  def Inlined(call: tpd.Tree, bindings: List[MemberDef], expansion: Tree): Inlined = new Inlined(call, bindings, expansion)
  def TypeTree() = new TypeTree()
  def SingletonTypeTree(ref: Tree): SingletonTypeTree = new SingletonTypeTree(ref)
  def AndTypeTree(left: Tree, right: Tree): AndTypeTree = new AndTypeTree(left, right)
  def OrTypeTree(left: Tree, right: Tree): OrTypeTree = new OrTypeTree(left, right)
  def RefinedTypeTree(tpt: Tree, refinements: List[Tree]): RefinedTypeTree = new RefinedTypeTree(tpt, refinements)
  def AppliedTypeTree(tpt: Tree, args: List[Tree]): AppliedTypeTree = new AppliedTypeTree(tpt, args)
  def LambdaTypeTree(tparams: List[TypeDef], body: Tree): LambdaTypeTree = new LambdaTypeTree(tparams, body)
  def ByNameTypeTree(result: Tree): ByNameTypeTree = new ByNameTypeTree(result)
  def TypeBoundsTree(lo: Tree, hi: Tree): TypeBoundsTree = new TypeBoundsTree(lo, hi)
  def Bind(name: Name, body: Tree): Bind = new Bind(name, body)
  def Alternative(trees: List[Tree]): Alternative = new Alternative(trees)
  def UnApply(fun: Tree, implicits: List[Tree], patterns: List[Tree]): UnApply = new UnApply(fun, implicits, patterns)
  def ValDef(name: TermName, tpt: Tree, rhs: LazyTree): ValDef = new ValDef(name, tpt, rhs)
  def DefDef(name: TermName, tparams: List[TypeDef], vparamss: List[List[ValDef]], tpt: Tree, rhs: LazyTree): DefDef = new DefDef(name, tparams, vparamss, tpt, rhs)
  def TypeDef(name: TypeName, rhs: Tree): TypeDef = new TypeDef(name, rhs)
  def Template(constr: DefDef, parents: List[Tree], self: ValDef, body: LazyTreeList): Template = new Template(constr, parents, self, body)
  def Import(expr: Tree, selectors: List[untpd.Tree]): Import = new Import(expr, selectors)
  def PackageDef(pid: RefTree, stats: List[Tree]): PackageDef = new PackageDef(pid, stats)
  def Annotated(arg: Tree, annot: Tree): Annotated = new Annotated(arg, annot)

  // ------ Additional creation methods for untyped only -----------------

  /**     new pre.C[Ts](args1)...(args_n)
   *  ==>
   *      (new pre.C).<init>[Ts](args1)...(args_n)
   */
  def New(tpt: Tree, argss: List[List[Tree]])(implicit ctx: Context): Tree = {
    val (tycon, targs) = tpt match {
      case AppliedTypeTree(tycon, targs) =>
        (tycon, targs)
      case TypedSplice(AppliedTypeTree(tycon, targs)) =>
        (TypedSplice(tycon), targs map (TypedSplice(_)))
      case TypedSplice(tpt1: Tree) =>
        val argTypes = tpt1.tpe.argTypesLo
        val tycon = tpt1.tpe.withoutArgs(argTypes)
        def wrap(tpe: Type) = TypeTree(tpe) withPos tpt.pos
        (wrap(tycon), argTypes map wrap)
      case _ =>
        (tpt, Nil)
    }
    var prefix: Tree = Select(New(tycon), nme.CONSTRUCTOR)
    if (targs.nonEmpty) prefix = TypeApply(prefix, targs)
    ensureApplied((prefix /: argss)(Apply(_, _)))
  }

  def Block(stat: Tree, expr: Tree): Block =
    Block(stat :: Nil, expr)

  def Apply(fn: Tree, arg: Tree): Apply =
    Apply(fn, arg :: Nil)

  def ensureApplied(tpt: Tree) = tpt match {
    case _: Apply => tpt
    case _ => Apply(tpt, Nil)
  }

  def AppliedTypeTree(tpt: Tree, arg: Tree): AppliedTypeTree =
    AppliedTypeTree(tpt, arg :: Nil)

  def TypeTree(tpe: Type)(implicit ctx: Context): TypedSplice = TypedSplice(TypeTree().withTypeUnchecked(tpe))

  def unitLiteral = Literal(Constant(()))

  def ref(tp: NamedType, pos: Position = NoPosition)(implicit ctx: Context): Tree =
    TypedSplice(tpd.ref(tp, pos))

  def rootDot(name: Name) = Select(Ident(nme.ROOTPKG), name)
  def scalaDot(name: Name) = Select(rootDot(nme.scala_), name)
  def scalaUnit = scalaDot(tpnme.Unit)
  def scalaAny = scalaDot(tpnme.Any)

  def makeConstructor(tparams: List[TypeDef], vparamss: List[List[ValDef]], rhs: Tree = EmptyTree)(implicit ctx: Context): DefDef =
    DefDef(nme.CONSTRUCTOR, tparams, vparamss, TypeTree(), rhs)

  def emptyConstructor(implicit ctx: Context): DefDef =
    makeConstructor(Nil, Nil)

  def makeSelfDef(name: TermName, tpt: Tree)(implicit ctx: Context) =
    ValDef(name, tpt, EmptyTree).withFlags(PrivateLocal)

  def makeTupleOrParens(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => Parens(t)
    case _ => Tuple(ts)
  }

  def makeTuple(ts: List[Tree])(implicit ctx: Context) = ts match {
    case t :: Nil => t
    case _ => Tuple(ts)
  }

  def makeParameter(pname: TermName, tpe: Tree, mods: Modifiers = EmptyModifiers)(implicit ctx: Context): ValDef =
    ValDef(pname, tpe, EmptyTree).withMods(mods | Param)

  def makeSyntheticParameter(n: Int = 1, tpt: Tree = TypeTree())(implicit ctx: Context): ValDef =
    ValDef(nme.syntheticParamName(n), tpt, EmptyTree).withFlags(SyntheticTermParam)

  def lambdaAbstract(tparams: List[TypeDef], tpt: Tree)(implicit ctx: Context) =
    if (tparams.isEmpty) tpt else LambdaTypeTree(tparams, tpt)

  /** A reference to given definition. If definition is a repeated
   *  parameter, the reference will be a repeated argument.
   */
  def refOfDef(tree: MemberDef)(implicit ctx: Context) = tree match {
    case ValDef(_, PostfixOp(_, Ident(nme.raw.STAR)), _) => repeated(Ident(tree.name))
    case _ => Ident(tree.name)
  }

  /** A repeated argument such as `arg: _*` */
  def repeated(arg: Tree)(implicit ctx: Context) = Typed(arg, Ident(tpnme.WILDCARD_STAR))

// ----- Accessing modifiers ----------------------------------------------------

  abstract class ModsDecorator { def mods: Modifiers }

  implicit class modsDeco(val mdef: MemberDef)(implicit ctx: Context) {
    def mods = mdef.rawMods
  }

// --------- Copier/Transformer/Accumulator classes for untyped trees -----

  override val cpy: UntypedTreeCopier = new UntypedTreeCopier

  class UntypedTreeCopier extends TreeCopier {

    def postProcess(tree: Tree, copied: Tree): copied.ThisTree[Untyped] =
      copied.asInstanceOf[copied.ThisTree[Untyped]]

    def postProcess(tree: Tree, copied: MemberDef): copied.ThisTree[Untyped] = {
      tree match {
        case tree: MemberDef => copied.withMods(tree.rawMods)
        case _ => copied
      }
    }.asInstanceOf[copied.ThisTree[Untyped]]

    def ModuleDef(tree: Tree)(name: TermName, impl: Template) = tree match {
      case tree: ModuleDef if (name eq tree.name) && (impl eq tree.impl) => tree
      case _ => finalize(tree, untpd.ModuleDef(name, impl))
    }
    def ParsedTry(tree: Tree)(expr: Tree, handler: Tree, finalizer: Tree) = tree match {
      case tree: ParsedTry
        if (expr eq tree.expr) && (handler eq tree.handler) && (finalizer eq tree.finalizer) => tree
      case _ => finalize(tree, untpd.ParsedTry(expr, handler, finalizer))
    }
    def SymbolLit(tree: Tree)(str: String) = tree match {
      case tree: SymbolLit if str == tree.str => tree
      case _ => finalize(tree, untpd.SymbolLit(str))
    }
    def InterpolatedString(tree: Tree)(id: TermName, segments: List[Tree]) = tree match {
      case tree: InterpolatedString if (id eq tree.id) && (segments eq tree.segments) => tree
      case _ => finalize(tree, untpd.InterpolatedString(id, segments))
    }
    def Function(tree: Tree)(args: List[Tree], body: Tree) = tree match {
      case tree: Function if (args eq tree.args) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.Function(args, body))
    }
    def InfixOp(tree: Tree)(left: Tree, op: Ident, right: Tree) = tree match {
      case tree: InfixOp if (left eq tree.left) && (op eq tree.op) && (right eq tree.right) => tree
      case _ => finalize(tree, untpd.InfixOp(left, op, right))
    }
    def PostfixOp(tree: Tree)(od: Tree, op: Ident) = tree match {
      case tree: PostfixOp if (od eq tree.od) && (op eq tree.op) => tree
      case _ => finalize(tree, untpd.PostfixOp(od, op))
    }
    def PrefixOp(tree: Tree)(op: Ident, od: Tree) = tree match {
      case tree: PrefixOp if (op eq tree.op) && (od eq tree.od) => tree
      case _ => finalize(tree, untpd.PrefixOp(op, od))
    }
    def Parens(tree: Tree)(t: Tree) = tree match {
      case tree: Parens if t eq tree.t => tree
      case _ => finalize(tree, untpd.Parens(t))
    }
    def Tuple(tree: Tree)(trees: List[Tree]) = tree match {
      case tree: Tuple if trees eq tree.trees => tree
      case _ => finalize(tree, untpd.Tuple(trees))
    }
    def Throw(tree: Tree)(expr: Tree) = tree match {
      case tree: Throw if expr eq tree.expr => tree
      case _ => finalize(tree, untpd.Throw(expr))
    }
    def WhileDo(tree: Tree)(cond: Tree, body: Tree) = tree match {
      case tree: WhileDo if (cond eq tree.cond) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.WhileDo(cond, body))
    }
    def DoWhile(tree: Tree)(body: Tree, cond: Tree) = tree match {
      case tree: DoWhile if (body eq tree.body) && (cond eq tree.cond) => tree
      case _ => finalize(tree, untpd.DoWhile(body, cond))
    }
    def ForYield(tree: Tree)(enums: List[Tree], expr: Tree) = tree match {
      case tree: ForYield if (enums eq tree.enums) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.ForYield(enums, expr))
    }
    def ForDo(tree: Tree)(enums: List[Tree], body: Tree) = tree match {
      case tree: ForDo if (enums eq tree.enums) && (body eq tree.body) => tree
      case _ => finalize(tree, untpd.ForDo(enums, body))
    }
    def GenFrom(tree: Tree)(pat: Tree, expr: Tree) = tree match {
      case tree: GenFrom if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.GenFrom(pat, expr))
    }
    def GenAlias(tree: Tree)(pat: Tree, expr: Tree) = tree match {
      case tree: GenAlias if (pat eq tree.pat) && (expr eq tree.expr) => tree
      case _ => finalize(tree, untpd.GenAlias(pat, expr))
    }
    def ContextBounds(tree: Tree)(bounds: TypeBoundsTree, cxBounds: List[Tree]) = tree match {
      case tree: ContextBounds if (bounds eq tree.bounds) && (cxBounds eq tree.cxBounds) => tree
      case _ => finalize(tree, untpd.ContextBounds(bounds, cxBounds))
    }
    def PatDef(tree: Tree)(mods: Modifiers, pats: List[Tree], tpt: Tree, rhs: Tree) = tree match {
      case tree: PatDef if (mods eq tree.mods) && (pats eq tree.pats) && (tpt eq tree.tpt) && (rhs eq tree.rhs) => tree
      case _ => finalize(tree, untpd.PatDef(mods, pats, tpt, rhs))
    }
  }

  abstract class UntypedTreeMap(cpy: UntypedTreeCopier = untpd.cpy) extends TreeMap(cpy) {
    override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case ModuleDef(name, impl) =>
        cpy.ModuleDef(tree)(name, transformSub(impl))
      case ParsedTry(expr, handler, finalizer) =>
        cpy.ParsedTry(tree)(transform(expr), transform(handler), transform(finalizer))
      case SymbolLit(str) =>
        cpy.SymbolLit(tree)(str)
      case InterpolatedString(id, segments) =>
        cpy.InterpolatedString(tree)(id, segments.mapConserve(transform))
      case Function(args, body) =>
        cpy.Function(tree)(transform(args), transform(body))
      case InfixOp(left, op, right) =>
        cpy.InfixOp(tree)(transform(left), op, transform(right))
      case PostfixOp(od, op) =>
        cpy.PostfixOp(tree)(transform(od), op)
      case PrefixOp(op, od) =>
        cpy.PrefixOp(tree)(op, transform(od))
      case Parens(t) =>
        cpy.Parens(tree)(transform(t))
      case Tuple(trees) =>
        cpy.Tuple(tree)(transform(trees))
      case Throw(expr) =>
        cpy.Throw(tree)(transform(expr))
      case WhileDo(cond, body) =>
        cpy.WhileDo(tree)(transform(cond), transform(body))
      case DoWhile(body, cond) =>
        cpy.DoWhile(tree)(transform(body), transform(cond))
      case ForYield(enums, expr) =>
        cpy.ForYield(tree)(transform(enums), transform(expr))
      case ForDo(enums, body) =>
        cpy.ForDo(tree)(transform(enums), transform(body))
      case GenFrom(pat, expr) =>
        cpy.GenFrom(tree)(transform(pat), transform(expr))
      case GenAlias(pat, expr) =>
        cpy.GenAlias(tree)(transform(pat), transform(expr))
      case ContextBounds(bounds, cxBounds) =>
        cpy.ContextBounds(tree)(transformSub(bounds), transform(cxBounds))
      case PatDef(mods, pats, tpt, rhs) =>
        cpy.PatDef(tree)(mods, transform(pats), transform(tpt), transform(rhs))
      case TypedSplice(_) =>
        tree
      case _ =>
        super.transform(tree)
    }
  }

  abstract class UntypedTreeAccumulator[X] extends TreeAccumulator[X] {
    override def foldOver(x: X, tree: Tree)(implicit ctx: Context): X = tree match {
      case ModuleDef(name, impl) =>
        this(x, impl)
      case ParsedTry(expr, handler, finalizer) =>
        this(this(this(x, expr), handler), finalizer)
      case SymbolLit(str) =>
        x
      case InterpolatedString(id, segments) =>
        this(x, segments)
      case Function(args, body) =>
        this(this(x, args), body)
      case InfixOp(left, op, right) =>
        this(this(this(x, left), op), right)
      case PostfixOp(od, op) =>
        this(this(x, od), op)
      case PrefixOp(op, od) =>
        this(this(x, op), od)
      case Parens(t) =>
        this(x, t)
      case Tuple(trees) =>
        this(x, trees)
      case Throw(expr) =>
        this(x, expr)
      case WhileDo(cond, body) =>
        this(this(x, cond), body)
      case DoWhile(body, cond) =>
        this(this(x, body), cond)
      case ForYield(enums, expr) =>
        this(this(x, enums), expr)
      case ForDo(enums, body) =>
        this(this(x, enums), body)
      case GenFrom(pat, expr) =>
        this(this(x, pat), expr)
      case GenAlias(pat, expr) =>
        this(this(x, pat), expr)
      case ContextBounds(bounds, cxBounds) =>
        this(this(x, bounds), cxBounds)
      case PatDef(mods, pats, tpt, rhs) =>
        this(this(this(x, pats), tpt), rhs)
      case TypedSplice(tree) =>
        this(x, tree)
      case _ =>
        super.foldOver(x, tree)
    }
  }

  /** Fold `f` over all tree nodes, in depth-first, prefix order */
  class UntypedDeepFolder[X](f: (X, Tree) => X) extends UntypedTreeAccumulator[X] {
    def apply(x: X, tree: Tree)(implicit ctx: Context): X = foldOver(f(x, tree), tree)
  }
}
