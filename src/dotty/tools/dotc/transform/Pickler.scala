package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Decorators._
import tasty._
import config.Printers.{noPrinter, pickling}
import java.io.PrintStream
import Periods._
import Phases._
import Symbols._
import Flags.Module
import util.SourceFile
import collection.mutable
import java.security.MessageDigest
import java.io._
import scala.meta.internal.ast.Source
import ast.tpd

import scala.meta.internal.hosts.dotty.contexts._
import scala.meta._

/** This phase pickles trees */
class Pickler extends Phase {
  import ast.tpd._

  override def phaseName: String = "pickler"

  private def output(name: String, msg: String) = {
    val s = new PrintStream(name)
    s.print(msg)
    s.close
  }

  private val beforePickling = new mutable.HashMap[ClassSymbol, String]

  /** Drop any elements of this list that are linked module classes of other elements in the list */
  private def dropCompanionModuleClasses(clss: List[ClassSymbol])(implicit ctx: Context): List[ClassSymbol] = {
    val companionModuleClasses =
      clss.filterNot(_ is Module).map(_.linkedClass).filterNot(_.isAbsent)
    clss.filterNot(companionModuleClasses.contains)
  }

  override def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    pickling.println(i"unpickling in run ${ctx.runId}")

    for { cls <- dropCompanionModuleClasses(topLevelClasses(unit.tpdTree))
          tree <- sliceTopLevel(unit.tpdTree, cls) } {
      if (ctx.settings.YtestPickler.value) beforePickling(cls) = tree.show
      val pickler = new TastyPickler()
      unit.picklers += (cls -> pickler)
      val treePkl = new TreePickler(pickler)
      treePkl.pickle(tree :: Nil)
      pickler.addrOfTree = treePkl.buf.addrOfTree
      pickler.addrOfSym = treePkl.addrOfSym
      if (unit.source.exists) {
        pickleSourcefile(pickler, unit.source)
        pickleScalametaSyntactic(pickler, unit.source)
        pickleScalametaSemantic(pickler, unit.tpdTree)
      }
      if (tree.pos.exists)
        new PositionPickler(pickler, treePkl.buf.addrOfTree).picklePositions(tree :: Nil, tree.pos)

      def rawBytes = // not needed right now, but useful to print raw format.
        pickler.assembleParts().iterator.grouped(10).toList.zipWithIndex.map {
          case (row, i) => s"${i}0: ${row.mkString(" ")}"
        }
      // println(i"rawBytes = \n$rawBytes%\n%") // DEBUG
      if (pickling ne noPrinter) {
        println(i"**** pickled info of $cls")
        new TastyPrinter(pickler.assembleParts()).printContents()
      }
    }
  }

  private def pickleSourcefile(pickler: TastyPickler, source: SourceFile): Unit = {
    val buf = new TastyBuffer(10)
    pickler.newSection("Sourcefile", buf)
    buf.writeNat(pickler.nameBuffer.nameIndex(source.file.path).index)
  }

  private def pickleScalametaSyntactic(pickler: TastyPickler, source: SourceFile): Unit = {
    val syntacticBuf = new TastyBuffer(20)
    pickler.newSection("ScalametaSyntactic", syntacticBuf)
    val text = source.content.mkString

    val sha1 = MessageDigest.getInstance("SHA-1")
    sha1.reset()
    sha1.update(text.getBytes("UTF-8"))
    val hash = sha1.digest().map(b => "%02X".format(b)).mkString

    val dialectBytes = "Scala211".getBytes("UTF-8")
    val hashBytes = hash.getBytes("UTF-8")
    syntacticBuf.writeNat(dialectBytes.length)
    syntacticBuf.writeBytes(dialectBytes, dialectBytes.length)
    syntacticBuf.writeNat(hashBytes.length)
    syntacticBuf.writeBytes(hashBytes, hashBytes.length)
  }

  private def pickleScalametaSemantic(pickler: TastyPickler, tree: tpd.Tree)(implicit ctx: Context): Unit = {
    val adapter = new Adapter(null)
    import adapter._
    val semSource = new conversions.ScalahostAdapterReflectTree(tree).toMeta
    println(semSource.toString)
    println(semSource.show[Semantics])

    val baos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(baos)
    oos.writeObject(semSource)
    oos.close()
    baos.close()
    val semanticBlob = baos.toByteArray()
    val semanticBuf = new TastyBuffer(semanticBlob.length)
    pickler.newSection("ScalametaSemantic", semanticBuf)
    semanticBuf.writeNat(semanticBlob.length)
    semanticBuf.writeBytes(semanticBlob, semanticBlob.length)
  }

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] = {
    val result = super.runOn(units)
    if (ctx.settings.YtestPickler.value)
      testUnpickler(units)(ctx.fresh.setPeriod(Period(ctx.runId + 1, FirstPhaseId)))
    result
  }

  private def testUnpickler(units: List[CompilationUnit])(implicit ctx: Context): Unit = {
    pickling.println(i"testing unpickler at run ${ctx.runId}")
    ctx.definitions.init
    val unpicklers =
      for (unit <- units; (cls, pickler) <- unit.picklers) yield {
        val unpickler = new DottyUnpickler(pickler.assembleParts())
        unpickler.enter(roots = Set())
        cls -> unpickler
      }
    pickling.println("************* entered toplevel ***********")
    for ((cls, unpickler) <- unpicklers) {
      val (unpickled, source) = unpickler.body(readPositions = true)
      testSame(i"$unpickled%\n%", beforePickling(cls), cls, source)
    }
  }

  private def testSame(unpickled: String, previous: String, cls: ClassSymbol, source: SourceFile)(implicit ctx: Context) =
    if (previous != unpickled) {
      output("before-pickling.txt", previous)
      output("after-pickling.txt", unpickled)
      ctx.error(s"""pickling difference for ${cls.fullName} in $source, for details:
                   |
                   |  diff before-pickling.txt after-pickling.txt""".stripMargin)
    }
}
