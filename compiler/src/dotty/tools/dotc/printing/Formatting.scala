package dotty.tools.dotc
package printing

import core._
import Texts._, Types._, Flags._, Names._, Symbols._, NameOps._, Contexts._
import collection.mutable
import collection.Map
import Decorators._
import scala.annotation.switch
import scala.util.control.NonFatal
import reporting.diagnostic.MessageContainer
import util.DiffUtil
import Highlighting._
import SyntaxHighlighting._

object Formatting {

  /** General purpose string formatter, with the following features:
   *
   *  1) On all Showables, `show` is called instead of `toString`
   *  2) Exceptions raised by a `show` are handled by falling back to `toString`.
   *  3) Sequences can be formatted using the desired separator between two `%` signs,
   *     eg `i"myList = (${myList}%, %)"`
   *  4) Safe handling of multi-line margins. Left margins are skipped om the parts
   *     of the string context *before* inserting the arguments. That way, we guard
   *     against accidentally treating an interpolated value as a margin.
   */
  class StringFormatter(protected val sc: StringContext) {

    protected def showArg(arg: Any)(implicit ctx: Context): String = arg match {
      case arg: Showable =>
        try arg.show
        catch {
          case NonFatal(ex)
          if !ctx.mode.is(Mode.PrintShowExceptions) &&
             !ctx.settings.YshowPrintErrors.value =>
            s"[cannot display due to $ex, raw string = $toString]"
        }
      case _ => arg.toString
    }

    private def treatArg(arg: Any, suffix: String)(implicit ctx: Context): (Any, String) = arg match {
      case arg: Seq[_] if suffix.nonEmpty && suffix.head == '%' =>
        val (rawsep, rest) = suffix.tail.span(_ != '%')
        val sep = StringContext.treatEscapes(rawsep)
        if (rest.nonEmpty) (arg.map(showArg).mkString(sep), rest.tail)
        else (arg, suffix)
      case _ =>
        (showArg(arg), suffix)
    }

    def assemble(args: Seq[Any])(implicit ctx: Context): String = {
      def isLineBreak(c: Char) = c == '\n' || c == '\f' // compatible with StringLike#isLineBreak
      def stripTrailingPart(s: String) = {
        val (pre, post) = s.span(c => !isLineBreak(c))
        pre ++ post.stripMargin
      }
      val (prefix, suffixes) = sc.parts.toList match {
        case head :: tail => (head.stripMargin, tail map stripTrailingPart)
        case Nil => ("", Nil)
      }
      val (args1, suffixes1) = (args, suffixes).zipped.map(treatArg(_, _)).unzip
      new StringContext(prefix :: suffixes1.toList: _*).s(args1: _*)
    }
  }

  /** The `em` string interpolator works like the `i` string interpolator, but marks nonsensical errors
   *  using `<nonsensical>...</nonsensical>` tags.
   *  Note: Instead of these tags, it would be nicer to return a data structure containing the message string
   *  and a boolean indicating whether the message is sensical, but then we cannot use string operations
   *  like concatenation, stripMargin etc on the values returned by em"...", and in the current error
   *  message composition methods, this is crucial.
   */
  class ErrorMessageFormatter(sc: StringContext) extends StringFormatter(sc) {
    override protected def showArg(arg: Any)(implicit ctx: Context): String =
      wrapNonSensical(arg, super.showArg(arg))
  }

  class SyntaxFormatter(sc: StringContext) extends StringFormatter(sc) {
    override protected def showArg(arg: Any)(implicit ctx: Context): String =
      arg match {
        case arg: Showable if ctx.settings.color.value != "never" =>
          val highlighted =
            SyntaxHighlighting(wrapNonSensical(arg, super.showArg(arg)))
          new String(highlighted.toArray)
        case hl: Highlight =>
          hl.show
        case hb: HighlightBuffer =>
          hb.toString
        case str: String if ctx.settings.color.value != "never" =>
          new String(SyntaxHighlighting(str).toArray)
        case _ => super.showArg(arg)
      }
  }

  private def wrapNonSensical(arg: Any /* Type | Symbol */, str: String)(implicit ctx: Context): String = {
    import MessageContainer._
    def isSensical(arg: Any): Boolean = arg match {
      case tpe: Type =>
        tpe.exists && !tpe.isErroneous
      case sym: Symbol if sym.isCompleted =>
        sym.info match {
          case _: ErrorType | TypeAlias(_: ErrorType) | NoType => false
          case _ => true
        }
      case _ => true
    }

    if (isSensical(arg)) str
    else nonSensicalStartTag + str + nonSensicalEndTag
  }

  private type Recorded = AnyRef /*Symbol | TypeParamRef | SkolemType */

  private class Seen extends mutable.HashMap[String, List[Recorded]] {

    override def default(key: String) = Nil

    def record(str: String, entry: Recorded)(implicit ctx: Context): String = {
      def followAlias(e1: Recorded): Recorded = e1 match {
        case e1: Symbol if e1.isAliasType =>
          val underlying = e1.typeRef.underlyingClassRef(refinementOK = false).typeSymbol
          if (underlying.name == e1.name) underlying else e1
        case _ => e1
      }
      lazy val dealiased = followAlias(entry)
      var alts = apply(str).dropWhile(alt => dealiased ne followAlias(alt))
      if (alts.isEmpty) {
        alts = entry :: apply(str)
        update(str, alts)
      }
      str + "'" * (alts.length - 1)
    }
  }

  private class ExplainingPrinter(seen: Seen)(_ctx: Context) extends RefinedPrinter(_ctx) {
    override def simpleNameString(sym: Symbol): String =
      if ((sym is ModuleClass) && sym.sourceModule.exists) simpleNameString(sym.sourceModule)
      else seen.record(super.simpleNameString(sym), sym)

    override def ParamRefNameString(param: ParamRef): String =
      seen.record(super.ParamRefNameString(param), param)

    override def toTextRef(tp: SingletonType): Text = tp match {
      case tp: SkolemType => seen.record(tp.repr.toString, tp)
      case _ => super.toTextRef(tp)
    }
  }

  /** Create explanation for single `Recorded` type or symbol */
  def explanation(entry: AnyRef)(implicit ctx: Context): String = {
    def boundStr(bound: Type, default: ClassSymbol, cmp: String) =
      if (bound.isRef(default)) "" else i"$cmp $bound"

    def boundsStr(bounds: TypeBounds): String = {
      val lo = boundStr(bounds.lo, defn.NothingClass, ">:")
      val hi = boundStr(bounds.hi, defn.AnyClass, "<:")
      if (lo.isEmpty) hi
      else if (hi.isEmpty) lo
      else s"$lo and $hi"
    }

    def addendum(cat: String, info: Type): String = info match {
      case bounds @ TypeBounds(lo, hi) if bounds ne TypeBounds.empty =>
        if (lo eq hi) i" which is an alias of $lo"
        else i" with $cat ${boundsStr(bounds)}"
      case _ =>
        ""
    }

    entry match {
      case param: TypeParamRef =>
        s"is a type variable${addendum("constraint", ctx.typeComparer.bounds(param))}"
      case sym: Symbol =>
        s"is a ${ctx.printer.kindString(sym)}${sym.showExtendedLocation}${addendum("bounds", sym.info)}"
      case tp: SkolemType =>
        s"is an unknown value of type ${tp.widen.show}"
    }
  }

  /** Turns a `Seen` into a `String` to produce an explanation for types on the
    * form `where: T is...`
    *
    * @return string disambiguating types
    */
  private def explanations(seen: Seen)(implicit ctx: Context): String = {
    def needsExplanation(entry: Recorded) = entry match {
      case param: TypeParamRef => ctx.typerState.constraint.contains(param)
      case skolem: SkolemType => true
      case _ => false
    }

    val toExplain: List[(String, Recorded)] = seen.toList.flatMap {
      case (str, entry :: Nil) =>
        if (needsExplanation(entry)) (str, entry) :: Nil else Nil
      case (str, entries) =>
        entries.map(alt => (seen.record(str, alt), alt))
    }.sortBy(_._1)

    def columnar(parts: List[(String, String)]): List[String] = {
      lazy val maxLen = parts.map(_._1.length).max
      parts.map {
        case (leader, trailer) =>
          val variable = hl"$leader"
          s"""$variable${" " * (maxLen - leader.length)} $trailer"""
      }
    }

    val explainParts = toExplain.map { case (str, entry) => (str, explanation(entry)) }
    val explainLines = columnar(explainParts)
    if (explainLines.isEmpty) "" else i"where:    $explainLines%\n          %\n"
  }

  /** Context with correct printer set for explanations */
  private def explainCtx(seen: Seen)(implicit ctx: Context): Context = ctx.printer match {
    case dp: ExplainingPrinter =>
      ctx // re-use outer printer and defer explanation to it
    case _ => ctx.fresh.setPrinterFn(ctx => new ExplainingPrinter(seen)(ctx))
  }

  /** Entrypoint for explanation string interpolator:
    *
    * ```
    * ex"disambiguate $tpe1 and $tpe2"
    * ```
    */
  def explained(op: Context => String)(implicit ctx: Context): String = {
    val seen = new Seen
    val msg = op(explainCtx(seen))
    val addendum = explanations(seen)
    if (addendum.isEmpty) msg else msg ++ "\n\n" ++ addendum
  }

  /** When getting a type mismatch it is useful to disambiguate placeholders like:
    *
    * ```
    * found:    List[Int]
    * required: List[T]
    * where:    T is a type in the initalizer of value s which is an alias of
    *           String
    * ```
    *
    * @return the `where` section as well as the printing context for the
    *         placeholders - `("T is a...", printCtx)`
    */
  def disambiguateTypes(args: Type*)(implicit ctx: Context): (String, Context) = {
    val seen = new Seen
    val printCtx = explainCtx(seen)
    args.foreach(_.show(printCtx)) // showing each member will put it into `seen`
    (explanations(seen), printCtx)
  }

  /** This method will produce a colored type diff from the given arguments.
    * The idea is to do this for known cases that are useful and then fall back
    * on regular syntax highlighting for the cases which are unhandled.
    *
    * Please not that if used in combination with `disambiguateTypes` the
    * correct `Context` for printing should also be passed when calling the
    * method.
    *
    * @return the (found, expected, changePercentage) with coloring to
    *         highlight the difference
    */
  def typeDiff(found: Type, expected: Type)(implicit ctx: Context): (String, String) = {
    val fnd = wrapNonSensical(found, found.show)
    val exp = wrapNonSensical(expected, expected.show)

    DiffUtil.mkColoredTypeDiff(fnd, exp) match {
      case _ if ctx.settings.color.value == "never" => (fnd, exp)
      case (fnd, exp, change) if change < 0.5 => (fnd, exp)
      case _ => (fnd, exp)
    }
  }
}
