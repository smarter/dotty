package dotty

/** Jars used when compiling test, normally set from the sbt build */
object Jars {
  /** Dotty library Jar */
  val dottyLib: String = sys.env.get("DOTTY_LIB")
    .getOrElse(Properties.dottyLib)

  /** Dotty Compiler Jar */
  val dottyCompiler: String = sys.env.get("DOTTY_COMPILER")
    .getOrElse(Properties.dottyCompiler)

  /** Dotty Interfaces Jar */
  val dottyInterfaces: String = sys.env.get("DOTTY_INTERFACE")
    .getOrElse(Properties.dottyInterfaces)

  /** Scala asm Jar */
  lazy val scalaAsm: String =
    findJarFromRuntime("scala-asm-6.0.0-scala-1")

  /** JLine Jar */
  lazy val jline: String =
    findJarFromRuntime("jline-3.7.0")

  /** sbt compiler-interface jar */
  lazy val sbtCompilerInterface: String =
    findJarFromRuntime("compiler-interface")

  /** Dotty extras classpath from env or properties */
  val dottyExtras: List[String] = sys.env.get("DOTTY_EXTRAS")
    .map(_.split(":").toList).getOrElse(Properties.dottyExtras)

  /** Dotty test dependencies */
  val dottyTestDeps: List[String] =
    dottyLib :: dottyCompiler :: dottyInterfaces :: jline :: dottyExtras

  /** Dotty runtime with compiler dependencies, used for quoted.Expr.run */
  lazy val dottyRunWithCompiler: List[String] =
    dottyLib :: dottyCompiler :: dottyInterfaces :: sbtCompilerInterface :: scalaAsm :: Nil

  def scalaLibrary: String = sys.env.get("DOTTY_SCALA_LIBRARY")
    .getOrElse(findJarFromRuntime("scala-library"))

  /** Gets the scala 2.* library at runtime, note that doing this is unsafe
   *  unless you know that the library will be on the classpath of the running
   *  application. It is currently safe to call this function if the tests are
   *  run by sbt.
   */
  private def findJarFromRuntime(partialName: String) = {
    val urls = ClassLoader.getSystemClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(_.getFile.toString)
    urls.find(_.contains(partialName)).getOrElse {
      throw new java.io.FileNotFoundException(
        s"""Unable to locate $partialName on classpath:\n${urls.toList.mkString("\n")}"""
      )
    }
  }
}
