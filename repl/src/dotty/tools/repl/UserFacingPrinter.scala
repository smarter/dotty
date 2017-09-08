package dotty.tools
package repl

import dotc.ast.Trees.{ Untyped, Tree }
import dotc.core.Annotations.Annotation
import dotc.core.Constants.Constant
import dotc.core.Contexts.Context
import dotc.core.Denotations.{ Denotation, MultiDenotation, SingleDenotation }
import dotc.core.Flags._
import dotc.core.TypeApplications.{ AnyAppliedType, EtaExpansion }
import dotc.core.Names._
import dotc.core.NameOps._
import dotc.core.StdNames._
import dotc.core.Decorators._
import dotc.core.Scopes.Scope
import dotc.core.Symbols.{ Symbol, ClassSymbol, defn }
import dotc.core.SymDenotations.NoDenotation
import dotc.core.Types._
import dotc.printing.Texts._
import dotc.printing.{ GlobalPrec, DotPrec, Printer, PlainPrinter }
import dotc.typer.Implicits.SearchResult
import dotc.typer.ImportInfo

class UserFacingPrinter(_ctx: Context) extends PlainPrinter(_ctx) {

  private def panic(msg: String): Nothing = throw new AssertionError(msg)

  private[this] def getPkgCls(path: String) =
    _ctx.requiredPackage(path).moduleClass.asClass

  private lazy val collectionPkg = getPkgCls("scala.collection")
  private lazy val immutablePkg  = getPkgCls("scala.collection.immutable")
  private lazy val scalaPkg      = defn.ScalaPackageClass
  private lazy val javaLangPkg   = defn.JavaLangPackageVal.moduleClass.asClass

  def standardPkg(pkgSym: Symbol) = pkgSym match {
    case `scalaPkg` | `collectionPkg` | `immutablePkg` | `javaLangPkg` => true
    case _ => false
  }

  def wrappedName(pkgSym: Symbol) =
    pkgSym.name.toTermName == nme.EMPTY_PACKAGE ||
    pkgSym.name.isReplWrapperName

  def wellKnownPkg(pkgSym: Symbol) = standardPkg(pkgSym) || wrappedName(pkgSym)

  override protected def keyString(sym: Symbol): String = {
    val flags = sym.flags
    if (flags is Package) ""
    else if (sym.isPackageObject) "package object"
    else if (flags.is(Module) && flags.is(Case)) "case object"
    else if (sym.isClass && flags.is(Case)) "case class"
    else if (flags.is(Lazy)) "lazy val"
    else if (flags is Module) "object"
    else if (sym.isTerm && !flags.is(Param) && flags.is(Implicit)) "implicit val"
    else super.keyString(sym)
  }

  override def nameString(name: Name): String =
    if (name.isReplAssignName) name.decode.toString.takeWhile(_ != '$')
    else name.decode.toString

  override def toText(sym: Symbol): Text =
    if (sym.name.isReplAssignName) nameString(sym.name)
    else keyString(sym) ~~ nameString(sym.name.stripModuleClassSuffix)

  override def dclText(sym: Symbol): Text =
    toText(sym) ~ {
      if (sym.is(Method)) toText(sym.info)
      else if (sym.isClass) ""
      else if (sym.isType && sym.info.isInstanceOf[TypeAlias]) " =" ~~ toText(sym.info)
      else if (sym.isType) ""
      else {
        ":" ~~ toText(sym.info)
      }
    }

  override def toText(denot: Denotation): Text = denot match {
    case NoDenotation =>
      panic("NoDenotation encountered in UserFacingPrinter")
    case denot: MultiDenotation =>
      panic("MultiDenotation not allowed in UserFacingPrinter")
    case _ =>
      toText(denot.symbol)
  }

  override def toText(const: Constant): Text = Str(const.value.toString)

  override def toText(tp: Type): Text = tp match {
    case tp: AnnotatedType => toText(tp.tpe) ~~ toText(tp.annot)
    case tp: ConstantType => toText(tp.value)
    case tp: TypeAlias => toText(tp.underlying)
    case ExprType(result) => ":" ~~ toText(result)
    case TypeBounds(lo, hi) =>
      { if (lo != defn.NothingType) toText(lo) ~~ ">: _" else Str("_") } ~~
      { if (hi != defn.AnyType) "<:" ~~ toText(hi) else Text() }
    case tp: TypeRef => tp.info match {
      case TypeAlias(alias) => toText(alias)
      case _ => toText(tp.info)
    }
    case tp: ParamRef => {
      val name = tp.paramName.unexpandedName.invariantName.toString
      if (tp.isInstanceOf[TermParamRef]) name ~ ".type"
      else name
    }
    case EtaExpansion(tycon) => toText(tycon)
    case PolyType(params, res) =>
      "[" ~ Fluid(params.map(tl => toText(tl.paramRef)).intersperse(Str(", "))) ~ "]" ~ toText(res)
    case tp: MethodType => {
      def paramText(name: TermName, tp: Type) = toText(name) ~ ": " ~ toText(tp)
      changePrec(GlobalPrec) {
        (if (tp.isImplicit) "(implicit " else "(") ~
          Text((tp.paramNames, tp.paramInfos).zipped map paramText, ", ") ~
        (if (tp.resultType.isInstanceOf[MethodType]) ")" else "): ") ~
        toText(tp.resultType)
      }
    }
    case AnyAppliedType(tycon, args) => {
      def toTextInfixType(tycon: Type, args: List[Type]): Text = {
        // TODO: blatant copy from `RefinedPrinter`
        val l :: r :: Nil = args
        val isRightAssoc = tycon.typeSymbol.name.endsWith(":")
        val leftArg = if (isRightAssoc && l.isInfixType) "(" ~ toText(l) ~ ")" else toText(l)
        val rightArg = if (!isRightAssoc && r.isInfixType) "(" ~ toText(r) ~ ")" else toText(r)
        leftArg ~~ atPrec(DotPrec) { tycon.toText(this) } ~~ rightArg
      }
      if (tp.isInfixType) toTextInfixType(tycon, args)
      else {
        toText(tycon) ~ "[" ~ Fluid(args.reverse.map(toText).intersperse(Str(", "))) ~ "]"
      }
    }
    case tp: ClassInfo => {
      if (wellKnownPkg(tp.cls.owner))
        nameString(tp.cls.name)
      else {
        def printPkg(sym: ClassSymbol): Text =
          if (sym.owner == defn.RootClass || wrappedName(sym.owner))
            nameString(sym.name.stripModuleClassSuffix)
          else
            printPkg(sym.owner.asClass) ~ "." ~ toText(sym)

        printPkg(tp.cls.owner.asClass) ~ "." ~ nameString(tp.cls.name)
      }
    }
  }

  override lazy val plain = new PlainPrinter(_ctx)
}
