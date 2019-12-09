package dotty.tools
package dotc

import util.{FreshNameCreator, SourceFile}
import ast.{tpd, untpd}
import tpd.{Tree, TreeTraverser}
import typer.PrepareInlineable.InlineAccessors
import typer.Nullables
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.transform.SymUtils._
import util.{NoSource, SourceFile}
import util.Spans.Span
import core.Decorators._

class CompilationUnit protected (val source: SourceFile) {

  override def toString: String = source.toString

  private[this] var _untpdTree: untpd.Tree = untpd.EmptyTree
  def untpdTree: untpd.Tree = _untpdTree
  def untpdTree_=(x: untpd.Tree): Unit = _untpdTree = x

  private[this] var _tpdTree: tpd.Tree = tpd.EmptyTree
  def tpdTree: tpd.Tree = _tpdTree
  def tpdTree_=(x: tpd.Tree): Unit = _tpdTree = x

  def isJava: Boolean = source.file.name.endsWith(".java")

  private[this] var _pickled: Map[ClassSymbol, Array[Byte]] = Map()
  /** Pickled TASTY binaries, indexed by class. */
  def pickled: Map[ClassSymbol, Array[Byte]] = _pickled
  def pickled_=(x: Map[ClassSymbol, Array[Byte]]): Unit = _pickled = x

  /** The fresh name creator for the current unit.
   *  FIXME(#7661): This is not fine-grained enough to enable reproducible builds,
   *  see https://github.com/scala/scala/commit/f50ec3c866263448d803139e119b33afb04ec2bc
   */
  val freshNames: FreshNameCreator = new FreshNameCreator.Default

  private[this] var _needsStaging: Boolean = false
  /** Will be set to `true` if contains `Quote`.
   *  The information is used in phase `Staging` in order to avoid traversing trees that need no transformations.
   */
  def needsStaging: Boolean = _needsStaging
  def needsStaging_=(x: Boolean): Unit = _needsStaging = x

  /** A structure containing a temporary map for generating inline accessors */
  val inlineAccessors: InlineAccessors = new InlineAccessors

  private[this] var _suspended: Boolean = false
  def suspended: Boolean = _suspended
  def suspended_=(x: Boolean): Unit = _suspended = x

  def suspend()(given ctx: Context): Nothing =
    if !suspended then
      if (ctx.settings.XprintSuspension.value)
        ctx.echo(i"suspended: $this")
      suspended = true
      ctx.run.suspendedUnits += this
    throw CompilationUnit.SuspendException()

  private var myAssignmentSpans: Map[Int, List[Span]] = null

  /** A map from (name-) offsets of all local variables in this compilation unit
   *  that can be tracked for being not null to the list of spans of assignments
   *  to these variables.
   */
  def assignmentSpans(given Context): Map[Int, List[Span]] =
    if myAssignmentSpans == null then myAssignmentSpans = Nullables.assignmentSpans
    myAssignmentSpans
}

object CompilationUnit {

  class SuspendException extends Exception

  /** Make a compilation unit for top class `clsd` with the contents of the `unpickled` tree */
  def apply(clsd: ClassDenotation, unpickled: Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit =
    apply(new SourceFile(clsd.symbol.associatedFile, Array.empty[Char]), unpickled, forceTrees)

  /** Make a compilation unit, given picked bytes and unpickled tree */
  def apply(source: SourceFile, unpickled: Tree, forceTrees: Boolean)(implicit ctx: Context): CompilationUnit = {
    assert(!unpickled.isEmpty, unpickled)
    val unit1 = new CompilationUnit(source)
    unit1.tpdTree = unpickled
    if (forceTrees) {
      val force = new Force
      force.traverse(unit1.tpdTree)
      unit1.needsStaging = force.needsStaging
    }
    unit1
  }

  /** Create a compilation unit corresponding to `source`.
   *  If `mustExist` is true, this will fail if `source` does not exist.
   */
  def apply(source: SourceFile, mustExist: Boolean = true)(implicit ctx: Context): CompilationUnit = {
    val src =
      if (!mustExist)
        source
      else if (source.file.isDirectory) {
        ctx.error(s"expected file, received directory '${source.file.path}'")
        NoSource
      }
      else if (!source.file.exists) {
        ctx.error(s"not found: ${source.file.path}")
        NoSource
      }
      else source
    new CompilationUnit(source)
  }

  /** Force the tree to be loaded */
  private class Force extends TreeTraverser {
    var needsStaging = false
    def traverse(tree: Tree)(implicit ctx: Context): Unit = {
      if (tree.symbol.isQuote)
        needsStaging = true
      traverseChildren(tree)
    }
  }
}

object NoCompilationUnit extends CompilationUnit(NoSource) {
  override def untpdTree_=(x: untpd.Tree): Unit = ???
  override def tpdTree_=(x: tpd.Tree): Unit = ???
  override def pickled_=(x: Map[ClassSymbol, Array[Byte]]): Unit = ???
  override def needsStaging_=(x: Boolean): Unit = ???
  override def suspended_=(x: Boolean): Unit = ???
}
