package scala.meta
package internal.hosts.dotty
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.debug._
import java.io.File
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.{classTag, ClassTag}
import scala.reflect.internal.MissingRequirementError
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.reflect.internal.NoPhase
import scala.reflect.internal.Phase
import scala.reflect.internal.util.BatchSourceFile
import scala.reflect.io.PlainFile
import scala.meta.artifacts.Artifact.Adhoc
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.{Context => ContextApi}
import scala.meta.internal.hosts.dotty.{Adapter => AdapterApi}
import scala.meta.internal.hosts.dotty.converters.{Api => ConverterApi}
import scala.meta.internal.ast.mergeTrees
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.tools.nsc.backend.JavaPlatform
import scala.tools.nsc.plugins.Plugin
import scala.meta.dialects.Scala211
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.flags._
import scala.meta.internal.prettyprinters._

import dotty.tools.dotc.core.Contexts.{Context => DottyContext}
import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.core.{Names => dna}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

//@context(translateExceptions = false)
class Adapter[G <: ScalaGlobal, T >: dtr.Untyped <: dty.Type](val global: G, initialDomain: Domain = Domain())(implicit val ctx: DottyContext)
  extends ConverterApi[T](global)(ctx) with ContextApi with AdapterApi[G, T] {

  val cpy: TreeCopier = null
  implicit def modsDeco(mdef: MemberDef)(implicit ctx: DottyContext): ModsDeco = ???

  //initializeFromDomain(initialDomain)

  // ======= SEMANTIC CONTEXT =======

  implicit lazy val c: ScalametaSemanticContext = this

  def dialect: scala.meta.dialects.Scala211.type = {
    scala.meta.dialects.Scala211
  }

  def domain: mapi.Domain = {
    currentDomain
  }

  private[meta] def typecheck(tree: mapi.Tree): mapi.Tree = {
    if (tree.isTypechecked) return tree
    ???
  }

  private[meta] def defns(untypedRef: mapi.Ref): Seq[mapi.Member] = {
    val ref = typecheck(untypedRef).require[m.Ref]
    val result = ref match {
      case pname: m.Name => pname.toLsymbols.map(_.toMmember(pname.toGprefix))
      case m.Term.Select(_, pname) => defns(pname)
      case m.Type.Select(_, pname) => defns(pname)
      case m.Type.Project(_, pname) => defns(pname)
      case m.Type.Singleton(pref) => defns(pref)
      case m.Pat.Type.Project(_, pname) => defns(pname)
      case m.Ctor.Ref.Select(_, pname) => defns(pname)
      case m.Ctor.Ref.Project(_, pname) => defns(pname)
      case m.Ctor.Ref.Function(pname) => defns(pname)
      case _: m.Import.Selector => ???
    }
    result.map(_.forceTypechecked)
  }

  private[meta] def members(untypedTpe: mapi.Type): Seq[mapi.Member] = {
    ???
  }

  private[meta] def supermembers(untypedMember: mapi.Member): Seq[mapi.Member] = {
    ???
  }

  private[meta] def submembers(untypedMember: mapi.Member): Seq[mapi.Member] = {
    ???
  }

  private[meta] def isSubtype(untypedTpe1: mapi.Type, untypedTpe2: mapi.Type): Boolean = {
    val tpe1 = typecheck(untypedTpe1).require[m.Type]
    val tpe2 = typecheck(untypedTpe2).require[m.Type]
    val gtpe1 = tpe1.toGtype
    val gtpe2 = tpe2.toGtype
    gtpe1 <:< gtpe2
  }

  private[meta] def lub(untypedTpes: Seq[mapi.Type]): mapi.Type = {
    ???
  }

  private[meta] def glb(untypedTpes: Seq[mapi.Type]): mapi.Type = {
    ???
  }

  private[meta] def supertypes(untypedTpe: mapi.Type): Seq[mapi.Type] = {
    ???
  }

  private[meta] def widen(untypedTpe: mapi.Type): mapi.Type = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val widened = gtpe.widen.toMtype
    widened.forceTypechecked
  }

  private[meta] def dealias(untypedTpe: mapi.Type): mapi.Type = {
    val tpe = typecheck(untypedTpe).require[m.Type]
    val gtpe = tpe.toGtype
    val dealiased = gtpe.dealias.toMtype
    dealiased.forceTypechecked
  }

  // ======= INTERACTIVE CONTEXT =======

  private[meta] def load(artifacts: Seq[mapi.Artifact]): Seq[mapi.Artifact] = {
    Debug.logScalahost(println(s"loading ${artifacts.length} artifacts from $artifacts"))

    val artifactClasspath = artifacts.flatMap(_.binaries).map(p => new File(p.path).toURI.toURL)
    Debug.logScalahost(println(s"indexing artifact classpath: $artifactClasspath"))
    val globalClasspath = global.classPath.asURLs.toSet
    Debug.logScalahost(println(s"global classpath: ${global.classPath.asURLs}"))
    val deltaClasspath = artifactClasspath.filter(entry => !globalClasspath(entry))
    //if (deltaClasspath.nonEmpty) global.extendCompilerClassPath(deltaClasspath: _*)

    val artifactSources = artifacts.flatMap(_.sources.map(_.require[m.Source]))
    val (typedSources, untypedSources) = artifactSources.partition(_.isTypechecked)
    Debug.logScalahost(println(s"indexing ${artifactSources.length} artifact sources (${typedSources.length} out of ${artifactSources.length} are TYPECHECKED)"))
    typedSources.foreach(source => {
      // TODO: looks like we really need some kind of a Source.id
      Debug.logScalahost(println(s"indexing ${source.show[Summary]}"))
      indexAll(source.require[m.Source])
    })

    assert(untypedSources.isEmpty)
    
    artifacts
  }

  // ============== Adapter ==============

  private[meta] def toMtree[T <: mapi.Tree : ClassTag](gtree: g.Tree): T = {
    XtensionGtreeToMtree(gtree).toMtree[T]
  }

  private[meta] def toMtype(gtpe: dty.Type): m.Type = {
    XtensionGtypeToMtype(gtpe).toMtype
  }

  private[meta] def toMtypeArg(gtpe: dty.Type): m.Type.Arg = {
    XtensionGtypeToMtype(gtpe).toMtypeArg
  }

  private[meta] def toMmember(gsym: dsy.Symbol, gpre: dty.Type): m.Member = {
    XtensionLsymbolToMmember(gsym.toLogical).toMmember(gpre)
  }

  // private[meta] def toMannot(gannot: g.AnnotationInfo): m.Mod.Annot = {
  //   XtensionGannotToMannot(gannot).toMannot
  // }

  private[meta] def toGtree(mtree: mapi.Tree): g.Tree = {
    XtensionMtreeToGtree(mtree.require[m.Tree]).toGtree
  }

  private[meta] def toGtype(mtpe: mapi.Type.Arg): dty.Type = {
    XtensionMtypeToGtype(mtpe.require[m.Type.Arg]).toGtype
  }

  private[meta] def toGsymbols(mname: mapi.Name): Seq[dsy.Symbol] = {
    XtensionMnameToLsymbols(mname.require[m.Name]).toLsymbols.flatMap(_.gsymbols)
  }

  private[meta] def toGsymbols(mmember: mapi.Member): Seq[dsy.Symbol] = {
    XtensionMmemberToLsymbols(mmember.require[m.Member]).toLsymbols.flatMap(_.gsymbols)
  }

  // ======= INTERNAL BOOKKEEPING =======

  private var currentDomain: Domain = _
  private def initializeFromDomain(initialDomain: Domain): Unit = withWriteOnlyIndices {
    val fromScratch = global.currentRun == null
    def fail(reason: String, ex: Option[Exception]) = {
      val status = if (fromScratch) "scratch" else "a pre-existing compiler"
      throw new InfrastructureException(s"can't initialize a semantic Adapter from $status: " + reason, ex)
    }

    Debug.logScalahost({
      println(s"initializing semantic Adapter from $global and $initialDomain")
      if (fromScratch) println("starting from scratch") else println("wrapping a pre-existing global")
    })

    try {
      // NOTE: This is necessary for semantic APIs to work correctly,
      // because otherwise the underlying global isn't going to have its symtab populated.
      // It would probably be possible to avoid this by creating completers that
      // lazily read scala.meta trees and then lazily convert them to symtab entries,
      // but that's way beyond our technological level at the moment.
      val domainClasspath = initialDomain.artifacts.flatMap(_.binaries).map(p => new File(p.path).toURI.toURL)
      Debug.logScalahost(println(s"indexing domain classpath: $domainClasspath"))
      if (fromScratch) {
        // NOTE: Make sure that the internal classpath cache hasn't been initialized yet.
        // If it has, we're in trouble, because our modifications to settings.classpath.value
        // aren't going to get propagated. Of course, I tried to sidestep this problem
        // by using something like `global.extendCompilerClassPath(domainClasspath: _*)`,
        // but unfortunately it throws an obscure assertion error, so I just gave up.
        Debug.logScalahost(println(s"setting the classpath..."))
        val m_currentClassPath = classOf[JavaPlatform].getDeclaredMethod("currentClassPath")
        m_currentClassPath.setAccessible(true)
        val currentClassPath = m_currentClassPath.invoke(global.platform).asInstanceOf[Option[_]]
        require(currentClassPath.isEmpty)
        global.settings.classpath.value = domainClasspath.map(_.getPath).mkString(File.pathSeparator)

        /*
        // NOTE: Install the scalahost plugin, because we need its analyzer hijacks.
        // In order to do that, we need to force the initialization of `Global.plugins` to hijack it afterwards.
        // Alternatively, we could try to register scalahost in compiler.settings in Compiler.scala,
        // but for that we need to know our classpath, and I don't know how to do that.
        Debug.logScalahost(println(s"ensuring the scalahost plugin..."))
        val _ = global.plugins
        if (!global.plugins.exists(_.name == "scalahost")) {
          val f_plugins = global.getClass.getDeclaredField("plugins")
          f_plugins.setAccessible(true)
          val plugins = f_plugins.get(global).asInstanceOf[List[Plugin]]
          val f_roughPluginsList = global.getClass.getDeclaredField("roughPluginsList")
          f_roughPluginsList.setAccessible(true)
          val roughPluginsList = f_roughPluginsList.get(global).asInstanceOf[List[Plugin]]
          val scalahostPlugin = new ScalahostPlugin(global)
          f_roughPluginsList.set(global, roughPluginsList :+ scalahostPlugin)
          f_plugins.set(global, plugins :+ scalahostPlugin)
        }
        */
        // NOTE: This is mandatory, because this is going to: a) finish necessary initialization of the compiler,
        // b) force computation of the classpath that we have just set.
        Debug.logScalahost(println(s"starting the compilation pipeline from pickler..."))
        val run = new global.Run
        global.phase = run.picklerPhase
        global.globalPhase = run.picklerPhase
      } else {
        ???
      }
    } catch {
      case ex: InfrastructureException =>
        throw ex
      case ex: Exception =>
        var message = ex.getMessage
        if (ex.isInstanceOf[MissingRequirementError]) {
          message = message.stripSuffix(".")
          message += " (have you forgotten to reference the standard library"
          message += " when creating a scala.meta context?)"
        }
        fail(message, Some(ex))
    }

    Debug.logScalahost(println(s"indexing ${global.currentRun.units.toList.length} global sources"))
    val globalSources = Nil // TODO

    // TODO: Do something smarter when assigning the initial domain, e.g.:
    // 1) Compute dependencies from settings.classpath
    // 2) Figure out resources somehow
    val globalArtifacts = List(Artifact(globalSources, Nil, Nil))
    currentDomain = Domain(globalArtifacts: _*)

    Debug.logScalahost(println(s"indexing ${initialDomain.artifacts.length} domain artifacts"))
    load(initialDomain.artifacts.toList)

    Debug.logScalahost(println(s"initialized semantic Adapter from $global and $initialDomain"))
  }
}