package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Constants._
import TypeErasure.ErasedValueType
import Contexts.Context, Scopes.Scope, Denotations._, SymDenotations._, Annotations.Annotation
import StdNames.nme
import ast.{Trees, untpd}
import typer.Namer
import typer.ProtoTypes.{SelectionProto, ViewProto, FunProto, IgnoredProto, dummyTreeOfType}
import Trees._
import scala.annotation.switch

class RefinedPrinter(_ctx: Context) extends PlainPrinter(_ctx) {

  override protected def recursionLimitExceeded() = {}

  protected val PrintableFlags = (SourceModifierFlags | Label | Module | Local).toCommonFlags

  /** The closest enclosing DefDef, TypeDef, or ClassDef node */
  private var currentOwner: untpd.Tree = untpd.EmptyTree

  def atOwner(owner: Tree[_ >: Untyped])(op: => Text): Text = {
    val saved = currentOwner
    currentOwner = owner
    try op
    finally { currentOwner = saved }
  }

  private def ownerIsClass = currentOwner match {
    case owner: TypeDef[_] => owner.isClassDef
    case owner: untpd.ModuleDef => true
    case _ => false
  }

  override def nameString(name: Name): String = name.decode.toString

  override protected def simpleNameString(sym: Symbol): String =
    sym.name.decode.toString

  override protected def fullNameOwner(sym: Symbol) = {
    val owner = super.fullNameOwner(sym)
    if (owner is ModuleClass) owner.sourceModule else owner
  }

  override def toTextRef(tp: SingletonType): Text = controlled {
    tp match {
      case tp: ThisType =>
        if (tp.cls.isAnonymousClass) return "this"
        if (tp.cls is ModuleClass) return fullNameString(tp.cls.sourceModule)
      case _ =>
    }
    super.toTextRef(tp)
  }

  override def toTextPrefix(tp: Type): Text = controlled {
    def isOmittable(sym: Symbol) = isOmittablePrefix(sym) && !ctx.settings.verbose.value
    tp match {
      case tp: ThisType =>
        if (isOmittable(tp.cls)) return ""
      case tp @ TermRef(pre, _) =>
        val sym = tp.symbol
        if (sym.isPackageObject) return toTextPrefix(pre)
        if (isOmittable(sym)) return ""
      case _ =>
    }
    super.toTextPrefix(tp)
  }

  override protected def refinementNameString(tp: RefinedType): String =
    if (tp.parent.isInstanceOf[WildcardType] || tp.refinedName == nme.WILDCARD)
      super.refinementNameString(tp)
    else {
      val tsym = tp.parent.member(tp.refinedName).symbol
      if (!tsym.exists) super.refinementNameString(tp)
      else {
        val name = tsym.originalName
        nameString(if (tsym is ExpandedTypeParam) name.asTypeName.unexpandedName() else name)
      }
    }

  override def toText(tp: Type): Text = controlled {
    def argText(arg: Type): Text = arg match {
      case arg: TypeBounds => "_" ~ toTextGlobal(arg)
      case _ => toTextGlobal(arg)
    }
    def toTextTuple(args: List[Type]): Text =
      "(" ~ toTextGlobal(args, ", ") ~ ")"
    def toTextFunction(args: List[Type]): Text =
      changePrec(GlobalPrec) {
        val argStr: Text =
          if (args.length == 2 && !defn.isTupleType(args.head))
            atPrec(InfixPrec) { toText(args.head) }
          else
            toTextTuple(args.init)
        argStr ~ " => " ~ toText(args.last)
      }
    tp match {
      case tp: RefinedType =>
        val args = tp.argInfos(interpolate = false)
        if (args.nonEmpty) {
          val tycon = tp.unrefine
          val cls = tycon.typeSymbol
          if (cls.typeParams.length == args.length) {
            if (tycon.isRepeatedParam) return toTextLocal(args.head) ~ "*"
            if (defn.FunctionClasses contains cls) return toTextFunction(args)
            if (defn.TupleClasses contains cls) return toTextTuple(args)
          }
          return (toTextLocal(tycon) ~ "[" ~ Text(args map argText, ", ") ~ "]").close
        }
      case tp: TypeRef =>
        val hideType = tp.symbol is TypeParam | TypeArgument | ExpandedName
        if (hideType && !ctx.phase.erasedTypes && !tp.symbol.isCompleting) {
          tp.info match {
            case TypeAlias(alias) => return toText(alias)
            case _ => if (tp.prefix.isInstanceOf[ThisType]) return nameString(tp.symbol)
          }
        }
        else if (tp.symbol.isAnonymousClass && !ctx.settings.uniqid.value)
          return toText(tp.info)
      case ExprType(result) =>
        return "=> " ~ toText(result)
      case ErasedValueType(clazz, underlying) =>
        return "ErasedValueType(" ~ toText(clazz.typeRef) ~ ", " ~ toText(underlying) ~ ")"
      case tp: ClassInfo =>
        return toTextParents(tp.instantiatedParents) ~ "{...}"
      case JavaArrayType(elemtp) =>
        return toText(elemtp) ~ "[]"
      case tp: SelectionProto =>
        return "?{ " ~ toText(tp.name) ~ ": " ~ toText(tp.memberProto) ~ " }"
      case tp: ViewProto =>
        return toText(tp.argType) ~ " ?=>? " ~ toText(tp.resultType)
      case FunProto(args, resultType, _) =>
        val argsText = args match {
          case dummyTreeOfType(tp) :: Nil if !(tp isRef defn.NullClass) => "null: " ~ toText(tp)
          case _ => toTextGlobal(args, ", ")
        }
        return "FunProto(" ~ argsText ~ "):" ~ toText(resultType)
      case tp: IgnoredProto =>
        return "?"
      case _ =>
    }
    super.toText(tp)
  }

  def blockText[T >: Untyped](trees: List[Tree[T]]): Text =
    "{" ~ toText(trees, "\n") ~ "}"

  override def toText[T >: Untyped](tree: Tree[T]): Text = controlled {

    def optDotPrefix(name: Name) = optText(name)(_ ~ ".")

    def optAscription(tpt: untpd.Tree) = optText(tpt)(": " ~ _)
      // Dotty deviation: called with an untpd.Tree, so cannot be a untpd.Tree[T] (seems to be a Scala2 problem to allow this)
      // More deviations marked below as // DD

    def tparamsText[T >: Untyped](params: List[Tree[T]]): Text =
      "[" ~ toText(params, ", ") ~ "]" provided params.nonEmpty

    def addVparamssText(txt: Text, vparamss: List[List[ValDef[T]]]): Text =
      (txt /: vparamss)((txt, vparams) => txt ~ "(" ~ toText(vparams, ", ") ~ ")")



    def caseBlockText(tree: Tree[T]): Text = tree match {
      case Block(stats, expr) => toText(stats :+ expr, "\n")
      case expr => toText(expr)
    }

    def enumText(tree: untpd.Tree) = tree match { // DD
      case _: untpd.GenFrom | _: untpd.GenAlias => toText(tree)
      case _ => "if " ~ toText(tree)
    }

    def forText(enums: List[untpd.Tree], expr: untpd.Tree, sep: String): Text = // DD
      changePrec(GlobalPrec) { "for " ~ Text(enums map enumText, "; ") ~ sep ~ toText(expr) }

    def cxBoundToText(bound: untpd.Tree): Text = bound match { // DD
      case AppliedTypeTree(tpt, _) => " : " ~ toText(tpt)
      case untpd.Function(_, tpt) => " <% " ~ toText(tpt)
    }

    def constrText(tree: untpd.Tree): Text = toTextLocal(tree).stripPrefix("new ") // DD

    def annotText(tree: untpd.Tree): Text = "@" ~ constrText(tree) // DD

    def useSymbol =
      tree.hasType && tree.symbol.exists && ctx.settings.YprintSyms.value

    def modText(mods: untpd.Modifiers, kw: String): Text = { // DD
      val suppressKw = if (ownerIsClass) mods is ParamAndLocal else mods is Param
      val flagMask = if (suppressKw) PrintableFlags &~ Private else PrintableFlags
      val flagsText: Text =
        if (useSymbol) toTextFlags(tree.symbol)
        else (mods.flags & flagMask).toString
      Text(mods.annotations.map(annotText), " ") ~~ flagsText ~~ (kw provided !suppressKw)
    }

    def argText(arg: Tree[T]): Text = arg match {
      case arg: TypeBoundsTree[_] => "_" ~ toTextGlobal(arg)
      case arg: TypeTree[_] =>
        arg.typeOpt match {
          case tp: TypeBounds => "_" ~ toTextGlobal(arg)
          case _ => toTextGlobal(arg)
        }
      case _ => toTextGlobal(arg)
    }

    def dclTextOr(treeText: => Text) =
      if (useSymbol)
        annotsText(tree.symbol) ~~ dclText(tree.symbol) ~
        ( " <in " ~ toText(tree.symbol.owner) ~ ">" provided ctx.settings.debugOwners.value)
      else treeText

    def idText(tree: untpd.Tree): Text = {
      if (ctx.settings.uniqid.value && tree.hasType && tree.symbol.exists) s"#${tree.symbol.id}" else ""
    }

    def nameIdText(tree: untpd.NameTree): Text =
      toText(tree.name) ~ idText(tree)

    import untpd._

    var txt: Text = tree match {
      case id: Trees.BackquotedIdent[_] =>
        "`" ~ toText(id.name) ~ "`"
      case Ident(name) =>
        tree.typeOpt match {
          case tp: NamedType if name != nme.WILDCARD =>
            val pre = if (tp.symbol is JavaStatic) tp.prefix.widen else tp.prefix
            toTextPrefix(pre) ~ selectionString(tp)
          case _ => toText(name)
        }
      case tree @ Select(qual, name) =>
        toTextLocal(qual) ~ ("." ~ nameIdText(tree) provided name != nme.CONSTRUCTOR)
      case This(name) =>
        optDotPrefix(name) ~ "this" ~ idText(tree)
      case Super(This(name), mix) =>
        optDotPrefix(name) ~ "super" ~ optText(mix)("[" ~ _ ~ "]")
      case Apply(fun, args) =>
        if (fun.hasType && fun.symbol == defn.throwMethod)
          changePrec (GlobalPrec) {
            "throw " ~ toText(args.head)
          }
        else
          toTextLocal(fun) ~ "(" ~ toTextGlobal(args, ", ") ~ ")"
      case TypeApply(fun, args) =>
        toTextLocal(fun) ~ "[" ~ toTextGlobal(args, ", ") ~ "]"
      case Literal(c) =>
        toText(c)
      case New(tpt) =>
        "new " ~ toTextLocal(tpt)
      case Pair(l, r) =>
        "(" ~ toTextGlobal(l) ~ ", " ~ toTextGlobal(r) ~ ")"
      case Typed(expr, tpt) =>
        changePrec(InfixPrec) { toText(expr) ~ ": " ~ toText(tpt) }
      case NamedArg(name, arg) =>
        toText(name) ~ " = " ~ toText(arg)
      case Assign(lhs, rhs) =>
        changePrec(GlobalPrec) { toTextLocal(lhs) ~ " = " ~ toText(rhs) }
      case Block(stats, expr) =>
        blockText(stats :+ expr)
      case If(cond, thenp, elsep) =>
        changePrec(GlobalPrec) {
          "if " ~ toText(cond) ~ (" then" provided !cond.isInstanceOf[Parens]) ~~ toText(thenp) ~ optText(elsep)(" else " ~ _)
        }
      case Closure(env, ref, target) =>
        "closure(" ~ (toTextGlobal(env, ", ") ~ " | " provided env.nonEmpty) ~
        toTextGlobal(ref) ~ (":" ~ toText(target) provided !target.isEmpty) ~ ")"
      case Match(sel, cases) =>
        if (sel.isEmpty) blockText(cases)
        else changePrec(GlobalPrec) { toText(sel) ~ " match " ~ blockText(cases) }
      case CaseDef(pat, guard, body) =>
        "case " ~ toText(pat) ~ optText(guard)(" if " ~ _) ~ " => " ~ caseBlockText(body)
      case Return(expr, from) =>
        changePrec(GlobalPrec) { "return" ~ optText(expr)(" " ~ _) }
      case Try(expr, cases, finalizer) =>
        changePrec(GlobalPrec) {
          "try " ~ toText(expr) ~ optText(cases)(" catch " ~ _) ~ optText(finalizer)(" finally " ~ _)
        }
      case Throw(expr) =>
        changePrec(GlobalPrec) {
          "throw " ~ toText(expr)
        }
      case SeqLiteral(elems) =>
        "[" ~ toTextGlobal(elems, ",") ~ "]"
      case tpt: untpd.DerivedTypeTree =>
        "<derived typetree watching " ~ summarized(toText(tpt.watched)) ~ ">"
      case TypeTree(orig) =>
        if (tree.hasType) toText(tree.typeOpt) else toText(orig)
      case SingletonTypeTree(ref) =>
        toTextLocal(ref) ~ ".type"
      case SelectFromTypeTree(qual, name) =>
        toTextLocal(qual) ~ "#" ~ toText(name)
      case AndTypeTree(l, r) =>
        changePrec(AndPrec) { toText(l) ~ " & " ~ toText(r) }
      case OrTypeTree(l, r) =>
        changePrec(OrPrec) { toText(l) ~ " | " ~ toText(r) }
      case RefinedTypeTree(tpt, refines) =>
        toTextLocal(tpt) ~ " " ~ blockText(refines)
      case AppliedTypeTree(tpt, args) =>
        toTextLocal(tpt) ~ "[" ~ Text(args map argText, ", ") ~ "]"
      case TypeBoundsTree(lo, hi) =>
        optText(lo)(" >: " ~ _) ~ optText(hi)(" <: " ~ _)
      case Bind(name, body) =>
        changePrec(InfixPrec) { toText(name) ~ " @ " ~ toText(body) }
      case Alternative(trees) =>
        changePrec(OrPrec) { toText(trees, " | ") }
      case UnApply(fun, implicits, patterns) =>
        val extractor = fun match {
          case Select(extractor, nme.unapply) => extractor
          case _ => fun
        }
        toTextLocal(extractor) ~
        "(" ~ toTextGlobal(patterns, ", ") ~ ")" ~
        ("(" ~ toTextGlobal(implicits, ", ") ~ ")" provided implicits.nonEmpty)
      case tree @ ValDef(name, tpt, rhs) =>
        dclTextOr {
          modText(tree.mods, if (tree.mods is Mutable) "var" else "val") ~~ nameIdText(tree) ~
            optAscription(tpt)
        } ~ optText(rhs)(" = " ~ _)
      case tree @ DefDef(name, tparams, vparamss, tpt, rhs) =>
        atOwner(tree) {
          dclTextOr {
            val first = modText(tree.mods, "def") ~~ nameIdText(tree) ~ tparamsText(tparams)
            addVparamssText(first, vparamss) ~ optAscription(tpt)
          } ~ optText(rhs)(" = " ~ _)
        }
      case tree @ TypeDef(name, rhs) =>
        atOwner(tree) {
          def typeDefText(rhsText: Text) =
            dclTextOr {
              val rhsText1 = if (tree.hasType) toText(tree.symbol.info) else rhsText
              modText(tree.mods, "type") ~~ nameIdText(tree) ~ tparamsText(tree.tparams) ~ rhsText1
            }
          rhs match {
            case impl: Template =>
              modText(tree.mods, if (tree.mods is Trait) "trait" else "class") ~~ nameIdText(tree) ~ toText(impl) ~
              (if (tree.hasType && ctx.settings.verbose.value) s"[decls = ${tree.symbol.info.decls}]" else "")
            case rhs: TypeBoundsTree =>
              typeDefText(toText(rhs))
            case _ =>
              typeDefText(optText(rhs)(" = " ~ _))
          }
        }
      case Template(constr @ DefDef(_, tparams, vparamss, _, rhs), parents, self, stats) =>
        val tparamsTxt = tparamsText(tparams)
        val primaryConstrs = if (rhs.isEmpty) Nil else constr :: Nil
        val prefix: Text =
          if (vparamss.isEmpty || primaryConstrs.nonEmpty) tparamsTxt
          else {
            var modsText = modText(constr.mods, "")
            if (constr.mods.hasAnnotations && !constr.mods.hasFlags) modsText = modsText ~~ " this"
            addVparamssText(tparamsTxt ~~ modsText, vparamss)
          }
        val parentsText = Text(parents map constrText, " with ")
        val selfText = {
          val selfName = if (self.name == nme.WILDCARD) "this" else self.name.toString
          (selfName ~ optText(self.tpt)(": " ~ _) ~ " =>").close
        } provided !self.isEmpty
        val bodyText = "{" ~~ selfText ~~ toTextGlobal(primaryConstrs ::: stats, "\n") ~ "}"
        prefix ~~ (" extends" provided ownerIsClass) ~~ parentsText ~~ bodyText
      case Import(expr, selectors) =>
        def selectorText(sel: Tree): Text = sel match {
          case Pair(l, r) => toTextGlobal(l) ~ " => " ~ toTextGlobal(r)
          case _ => toTextGlobal(sel)
        }
        val selectorsText: Text = selectors match {
          case Ident(name) :: Nil => toText(name)
          case _ => "{" ~ Text(selectors map selectorText, ", ") ~ "}"
        }
        "import " ~ toTextLocal(expr) ~ "." ~ selectorsText
      case PackageDef(pid, stats) =>
        val statsText = stats match {
          case (pdef: PackageDef) :: Nil => toText(pdef)
          case _ => toTextGlobal(stats, "\n")
        }
        val bodyText =
          if (currentPrecedence == TopLevelPrec) "\n" ~ statsText else " {" ~ statsText ~ "}"
        "package " ~ toTextLocal(pid) ~ bodyText
      case Annotated(annot, arg) =>
        toTextLocal(arg) ~~ annotText(annot)
      case EmptyTree =>
        "<empty>"
      case TypedSplice(t) =>
        toText(t)
      case tree @ ModuleDef(name, impl) =>
        atOwner(tree) {
          modText(tree.mods, "object") ~~ nameIdText(tree) ~ toText(impl)
        }
      case SymbolLit(str) =>
        "'" + str
      case InterpolatedString(id, strings, elems) =>
        def interleave(strs: List[Text], elems: List[Text]): Text = ((strs, elems): @unchecked) match {
          case (Nil, Nil) => ""
          case (str :: Nil, Nil) => str
          case (str :: strs1, elem :: elems1) => str ~ elem ~ interleave(strs1, elems1)
        }
        val strTexts = strings map (str => Str(escapedString(str.const.stringValue)))
        val elemsTexts = elems map (elem => "{" ~ toTextGlobal(elem) ~ "}")
        toText(id) ~ "\"" ~ interleave(strTexts, elemsTexts) ~ "\""
      case Function(args, body) =>
        var implicitSeen: Boolean = false
        def argToText(arg: Tree) = arg match {
          case arg @ ValDef(name, tpt, _) =>
            val implicitText =
              if ((arg.mods is Implicit) && !implicitSeen) { implicitSeen = true; "implicit " }
              else ""
            implicitText ~ toText(name) ~ optAscription(tpt)
          case _ =>
            toText(arg)
        }
        val argsText = args match {
          case (arg @ ValDef(_, tpt, _)) :: Nil if tpt.isEmpty => argToText(arg)
          case _ => "(" ~ Text(args map argToText, ", ") ~ ")"
        }
        changePrec(GlobalPrec) {
          argsText ~ " => " ~ toText(body)
        }
      case InfixOp(l, op, r) =>
        val opPrec = parsing.precedence(op)
        changePrec(opPrec) { toText(l) ~ " " ~ toText(op) ~ " " ~ toText(r) }
      case PostfixOp(l, op) =>
        changePrec(InfixPrec) { toText(l) ~ " " ~ toText(op) }
      case PrefixOp(op, r) =>
        changePrec(DotPrec) { toText(op) ~ " " ~ toText(r) }
      case Parens(t) =>
        "(" ~ toTextGlobal(t) ~ ")"
      case Tuple(ts) =>
        "(" ~ toTextGlobal(ts, ", ") ~ ")"
      case WhileDo(cond, body) =>
        changePrec(GlobalPrec) { "while " ~ toText(cond) ~ " do " ~ toText(body) }
      case DoWhile(cond, body) =>
        changePrec(GlobalPrec) { "do " ~ toText(body) ~ " while " ~ toText(cond) }
      case ForYield(enums, expr) =>
        forText(enums, expr, " yield ")
      case ForDo(enums, expr) =>
        forText(enums, expr, " do ")
      case GenFrom(pat, expr) =>
        toText(pat) ~ " <- " ~ toText(expr)
      case GenAlias(pat, expr) =>
        toText(pat) ~ " = " ~ toText(expr)
      case ContextBounds(bounds, cxBounds) =>
        (toText(bounds) /: cxBounds) {(t, cxb) =>
          t ~ cxBoundToText(cxb)
        }
      case PatDef(mods, pats, tpt, rhs) =>
        modText(mods, "val") ~~ toText(pats, ", ") ~ optAscription(tpt) ~
          optText(rhs)(" = " ~ _)
      case Thicket(trees) =>
        "Thicket {" ~~ toTextGlobal(trees, "\n") ~~ "}"
      case _ =>
        tree.fallbackToText(this)
    }
    if (ctx.settings.printtypes.value && tree.hasType) {
      val tp = tree.typeOpt match {
        case tp: TermRef if tree.isInstanceOf[RefTree] && !tp.denot.isOverloaded => tp.underlying
        case tp => tp
      }
      if (tree.isType) txt = toText(tp)
      else if (!tree.isDef) txt = "<" ~ txt ~ ":" ~ toText(tp) ~ ">"
    }
    if (ctx.settings.Yprintpos.value && !tree.isInstanceOf[WithoutTypeOrPos[_]])
      txt = txt ~ "@" ~ tree.pos.toString
    tree match {
      case Block(_, _) | Template(_, _, _, _) => txt
      case _ => txt.close
    }
  }

  def optText(name: Name)(encl: Text => Text): Text =
    if (name.isEmpty) "" else encl(toText(name))

  def optText[T >: Untyped](tree: Tree[T])(encl: Text => Text): Text =
    if (tree.isEmpty) "" else encl(toText(tree))

  def optText[T >: Untyped](tree: List[Tree[T]])(encl: Text => Text): Text =
    if (tree.exists(!_.isEmpty)) encl(blockText(tree)) else ""

  override protected def polyParamName(name: TypeName): TypeName =
    name.unexpandedName()

  override protected def treatAsTypeParam(sym: Symbol): Boolean = sym is TypeParam

  override protected def treatAsTypeArg(sym: Symbol) =
    sym.isType && (sym is ProtectedLocal) &&
      (sym.allOverriddenSymbols exists (_ is TypeParam))

  override def toText(sym: Symbol): Text = {
    if (sym.name == nme.IMPORT) {
      def importString(tree: untpd.Tree) = s"import ${tree.show}"
      sym.infoOrCompleter match {
        case info: Namer#Completer => return importString(info.original)
        case info: ImportType => return importString(info.expr)
        case _ =>
      }
    }
    super.toText(sym)
  }

  override def kindString(sym: Symbol) = {
    val flags = sym.flagsUNSAFE
    if (flags is Package) "package"
    else if (sym.isPackageObject) "package object"
    else if (flags is Module) "object"
    else if (flags is ImplClass) "class"
    else if (sym.isClassConstructor) "constructor"
    else super.kindString(sym)
  }

  override protected def keyString(sym: Symbol): String = {
    val flags = sym.flagsUNSAFE
    if (sym.isType && sym.owner.isTerm) ""
    else super.keyString(sym)
  }

  override def toTextFlags(sym: Symbol) =
    if (ctx.settings.debugFlags.value)
      super.toTextFlags(sym)
    else {
      var flags = sym.flagsUNSAFE
      if (flags is TypeParam) flags = flags &~ Protected
      Text((flags & SourceModifierFlags).flagStrings map stringToText, " ")
    }

  override def toText(denot: Denotation): Text = denot match {
    case denot: MultiDenotation => denot.toString
    case NoDenotation => "NoDenotation"
    case _ =>
      if (denot.symbol.exists) toText(denot.symbol)
      else "some " ~ toText(denot.info)
  }

  override def plain = new PlainPrinter(_ctx)
}
