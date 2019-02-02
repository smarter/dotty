package dotty.tools.sbtplugin

import sbt._
import sbt.Keys._
import sbt.librarymanagement.{
  ivy, DependencyResolution, ScalaModuleInfo, SemanticSelector, UpdateConfiguration, UnresolvedWarningConfiguration,
  VersionNumber
}
import sbt.internal.inc.ScalaInstance
import xsbti.compile._
import java.net.URLClassLoader
import java.util.Optional
import scala.util.Properties.isJavaAtLeast

object DottyPlugin extends AutoPlugin {
  object autoImport {
    val isDotty = settingKey[Boolean]("Is this project compiled with Dotty?")

    // NOTE:
    // - this is a def to support `scalaVersion := dottyLatestNightlyBuild`
    // - if this was a taskKey, then you couldn't do `scalaVersion := dottyLatestNightlyBuild`
    // - if this was a settingKey, then this would evaluate even if you don't use it.
    def dottyLatestNightlyBuild(): Option[String] = {
      import scala.io.Source

      println("Fetching latest Dotty nightly version...")

      val nightly = try {
        // get majorVersion from dotty.epfl.ch
        val source0 = Source.fromURL("http://dotty.epfl.ch/versions/latest-nightly-base")
        val majorVersion = source0.getLines().toSeq.head
        source0.close()

        // get latest nightly version from maven
        val source1 =
          Source.fromURL(s"http://repo1.maven.org/maven2/ch/epfl/lamp/dotty_$majorVersion/maven-metadata.xml")
        val Version = s"      <version>($majorVersion\\..*-bin.*)</version>".r
        val nightly = source1
          .getLines()
          .collect { case Version(version) => version }
          .toSeq
          .lastOption
        source1.close()
        nightly
      } catch {
        case _:java.net.UnknownHostException =>
          None
      }

      nightly match {
        case Some(version) =>
          println(s"Latest Dotty nightly build version: $version")
        case None =>
          println(s"Unable to get Dotty latest nightly build version. Make sure you are connected to internet")
      }

      nightly
    }

    implicit class DottyCompatModuleID(moduleID: ModuleID) {
      /** If this ModuleID cross-version is a Dotty version, replace it
       *  by the Scala 2.x version that the Dotty version is retro-compatible with,
       *  otherwise do nothing.
       *
       *  This setting is useful when your build contains dependencies that have only
       *  been published with Scala 2.x, if you have:
       *  {{{
       *  libraryDependencies += "a" %% "b" % "c"
       *  }}}
       *  you can replace it by:
       *  {{{
       *  libraryDependencies += ("a" %% "b" % "c").withDottyCompat(scalaVersion.value)
       *  }}}
       *  This will have no effect when compiling with Scala 2.x, but when compiling
       *  with Dotty this will change the cross-version to a Scala 2.x one. This
       *  works because Dotty is currently retro-compatible with Scala 2.x.
       *
       *  NOTE: Dotty's retro-compatibility with Scala 2.x will be dropped before
       *  Dotty is released, you should not rely on it.
       */
      def withDottyCompat(scalaVersion: String): ModuleID =
      moduleID.crossVersion match {
          case _: librarymanagement.Binary if scalaVersion.startsWith("0.") =>
            moduleID.cross(CrossVersion.constant("2.12"))
          case _ =>
            moduleID
        }
    }
  }

  import autoImport._

  override def requires: Plugins = plugins.JvmPlugin
  override def trigger = allRequirements

  /** Patches the IncOptions so that .tasty and .hasTasty files are pruned as needed.
   *
   *  This code is adapted from `scalaJSPatchIncOptions` in Scala.js, which needs
   *  to do the exact same thing but for classfiles.
   *
   *  This complicated logic patches the ClassfileManager factory of the given
   *  IncOptions with one that is aware of .tasty and .hasTasty files emitted by the Dotty
   *  compiler. This makes sure that, when a .class file must be deleted, the
   *  corresponding .tasty or .hasTasty file is also deleted.
   */
  def dottyPatchIncOptions(incOptions: IncOptions): IncOptions = {
    val tastyFileManager = new TastyFileManager

    // Once sbt/zinc#562 is fixed, can be:
    // val newExternalHooks =
    //   incOptions.externalHooks.withExternalClassFileManager(tastyFileManager)
    val inheritedHooks = incOptions.externalHooks
    val external = Optional.of(tastyFileManager: ClassFileManager)
    val prevManager = inheritedHooks.getExternalClassFileManager
    val fileManager: Optional[ClassFileManager] =
      if (prevManager.isPresent) Optional.of(WrappedClassFileManager.of(prevManager.get, external))
      else external
    val newExternalHooks = new DefaultExternalHooks(inheritedHooks.getExternalLookup, fileManager)

    incOptions.withExternalHooks(newExternalHooks)
  }

  override val globalSettings: Seq[Def.Setting[_]] = Seq(
    onLoad in Global := onLoad.in(Global).value.andThen { state =>

      val requirement = ">= 1.2.7"

      val sbtV = sbtVersion.value
      if (!VersionNumber(sbtV).matchesSemVer(SemanticSelector(requirement)))
        sys.error(s"The sbt-dotty plugin cannot work with this version of sbt ($sbtV), sbt $requirement is required.")

      state
    }
  )

  override def projectSettings: Seq[Setting[_]] = {
    Seq(
      isDotty := scalaVersion.value.startsWith("0."),

      scalaOrganization := {
        if (isDotty.value)
          "ch.epfl.lamp"
        else
          scalaOrganization.value
      },

      scalacOptions in (Compile, doc) ++= Seq("-project", name.value),

      incOptions in Compile := {
        val inc = (incOptions in Compile).value
        if (isDotty.value)
          dottyPatchIncOptions(inc)
        else
          inc
      },

      scalaCompilerBridgeBinaryJar := Def.taskDyn {
        if (isDotty.value) Def.task {
          val dottyBridgeArtifacts = fetchArtifactsOf(
            dependencyResolution.value,
            scalaModuleInfo.value,
            updateConfiguration.value,
            (unresolvedWarningConfiguration in update).value,
            streams.value.log,
            scalaOrganization.value % "dotty-sbt-bridge" % scalaVersion.value).allFiles
          val jars = dottyBridgeArtifacts.filter(art => art.getName.startsWith("dotty-sbt-bridge") && art.getName.endsWith(".jar")).toArray
          if (jars.size == 0)
            throw new MessageOnlyException("No jar found for dotty-sbt-bridge")
          else if (jars.size > 1)
            throw new MessageOnlyException(s"Multiple jars found for dotty-sbt-bridge: ${jars.toList}")
          else
            jars.headOption
        }
        else Def.task {
          None: Option[File]
        }
      }.value,

      // Needed for RCs publishing
      scalaBinaryVersion := {
        if (isDotty.value)
          scalaVersion.value.split("\\.").take(2).mkString(".")
        else
          scalaBinaryVersion.value
      },

      // Ideally, we should have:
      //
      // 1. Nothing but the Java standard library on the _JVM_ bootclasspath
      //    (starting with Java 9 we cannot inspect it so we don't have a choice)
      //
      // 2. scala-library, dotty-library, dotty-compiler, dotty-doc on the _JVM_
      //    classpath, because we need all of those to actually run the compiler
      //    and the doc tool.
      //    NOTE: All of those should have the *same version* (equal to scalaVersion
      //    for everything but scala-library).
      //
      // 3. scala-library, dotty-library on the _compiler_ bootclasspath because
      //    user code should always have access to the symbols from these jars but
      //    should not be able to shadow them (the compiler bootclasspath has
      //    higher priority than the compiler classpath).
      //    NOTE: the versions of {scala,dotty}-library used here do not necessarily
      //    match the one used in 2. because a dependency of the current project might
      //    require more recent versions, this is OK.
      //
      // 4. every other dependency of the user project on the _compiler_
      //    classpath.
      //
      // Unfortunately, zinc assumes that the compiler bootclasspath is only
      // made of one jar (scala-library), so to make this work we'll need to
      // either change sbt's bootclasspath handling or wait until the
      // dotty-library jar and scala-library jars are merged into one jar.
      // Meanwhile, let's just put nothing at all on the compiler
      // bootclasspath, and instead put scala-library and dotty-library on the
      // compiler classpath. This means that user code could shadow symbols
      // from these jars but we can live with that for now.
      classpathOptions := {
        val old = classpathOptions.value
        if (isDotty.value)
          old
            .withAutoBoot(false)      // we don't put the library on the compiler bootclasspath (as explained above)
            .withFilterLibrary(false) // ...instead, we put it on the compiler classpath
        else
          old
      },
      // ... but when running under Java 8, we still need a compiler bootclasspath
      // that contains the JVM bootclasspath, otherwise sbt incremental
      // compilation breaks.
      scalacOptions ++= {
        if (isDotty.value && !isJavaAtLeast("9"))
          Seq("-bootclasspath", sys.props("sun.boot.class.path"))
        else
          Seq()
      },
      managedScalaInstance := {
        val old = managedScalaInstance.value
        if (isDotty.value)
          false
        else
          old
      },
      scalaInstance := Def.taskDyn {
        if (isDotty.value) Def.task {
          val updateReport =
            fetchArtifactsOf(
              dependencyResolution.value,
              scalaModuleInfo.value,
              updateConfiguration.value,
              (unresolvedWarningConfiguration in update).value,
              streams.value.log,
              scalaOrganization.value %% "dotty-doc" % scalaVersion.value)
          val scalaLibraryJar = getJar(updateReport,
            "org.scala-lang", "scala-library", revision = AllPassFilter)
          val dottyLibraryJar = getJar(updateReport,
            scalaOrganization.value, s"dotty-library_${scalaBinaryVersion.value}", scalaVersion.value)
          val compilerJar = getJar(updateReport,
            scalaOrganization.value, s"dotty-compiler_${scalaBinaryVersion.value}", scalaVersion.value)
          val allJars =
            getJars(updateReport, AllPassFilter, AllPassFilter, AllPassFilter)

          makeScalaInstance(
            state.value,
            scalaVersion.value,
            scalaLibraryJar,
            dottyLibraryJar,
            compilerJar,
            allJars
          )
        }
        else Def.task {
          // This should really be `old` with `val old = scalaInstance.value`
          // above, except that this would force the original definition of the
          // `scalaInstance` task to be computed when `isDotty` is true, which
          // would fail because `managedScalaInstance` is false.
          Defaults.scalaInstanceTask.value
        }
      }.value,

      scalaModuleInfo := {
        val old = scalaModuleInfo.value
        if (isDotty.value) {
          // Turns off the warning:
          // [warn] Binary version (0.9.0-RC1) for dependency ...;0.9.0-RC1
          // [warn]  in ... differs from Scala binary version in project (0.9).
          old.map(_.withCheckExplicit(false))
        } else old
      },

      updateOptions := {
        val old = updateOptions.value
        if (isDotty.value) {
          // Turn off the warning:
          //  circular dependency found:
          //    ch.epfl.lamp#scala-library;0.9.0-RC1->ch.epfl.lamp#dotty-library_0.9;0.9.0-RC1->...
          // (This should go away once we merge dotty-library and scala-library in one artefact)
          old.withCircularDependencyLevel(ivy.CircularDependencyLevel.Ignore)
        } else old
      }
    ) ++ inConfig(Compile)(docSettings) ++ inConfig(Test)(docSettings)
  }

  private val docSettings = inTask(doc)(Seq(
    sources := {
      val _ = compile.value // Ensure that everything is compiled, so TASTy is available.
      val prev = sources.value
      val tastyFiles = (classDirectory.value ** "*.tasty").get.map(_.getAbsoluteFile)
      prev ++ tastyFiles
    },
    scalacOptions += "-from-tasty"
  ))

  /** Fetch artifacts for moduleID */
  def fetchArtifactsOf(
    dependencyRes: DependencyResolution,
    scalaInfo: Option[ScalaModuleInfo],
    updateConfig: UpdateConfiguration,
    warningConfig: UnresolvedWarningConfiguration,
    log: Logger,
    moduleID: ModuleID): UpdateReport = {
    val descriptor = dependencyRes.wrapDependencyInModule(moduleID, scalaInfo)

    dependencyRes.update(descriptor, updateConfig, warningConfig, log) match {
      case Right(report) =>
        report
      case _ =>
        throw new MessageOnlyException(
          s"Couldn't retrieve `$moduleID`.")
    }
  }

  /** Get all jars in updateReport that match the given filter. */
  def getJars(updateReport: UpdateReport, organization: NameFilter, name: NameFilter, revision: NameFilter): Seq[File] = {
    updateReport.select(
      configurationFilter(Runtime.name),
      moduleFilter(organization, name, revision),
      artifactFilter(extension = "jar")
    )
  }

  /** Get the single jar in updateReport that match the given filter.
    * If zero or more than one jar match, an exception will be thrown. */
  def getJar(updateReport: UpdateReport, organization: NameFilter, name: NameFilter, revision: NameFilter): File = {
    val jars = getJars(updateReport, organization, name, revision)
    assert(jars.size == 1, s"There should only be one $name jar but found: $jars")
    jars.head
  }

  def makeScalaInstance(
    state: State, dottyVersion: String, scalaLibrary: File, dottyLibrary: File, compiler: File, all: Seq[File]
  ): ScalaInstance = {
    val loader = state.classLoaderCache(all.toList)
    val loaderLibraryOnly = state.classLoaderCache(List(dottyLibrary, scalaLibrary))
    new ScalaInstance(
      dottyVersion,
      loader,
      loaderLibraryOnly,
      scalaLibrary, // Should be a Seq also containing dottyLibrary but zinc
                    // doesn't support this, see comment above our redefinition
                    // of `classpathOption`
      compiler,
      all.toArray,
      None)

  }
}
