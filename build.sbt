val dotty = Build.dotty
val `dotty-bootstrapped` = Build.`dotty-bootstrapped`
val `dotty-interfaces` = Build.`dotty-interfaces`
val `dotty-doc` = Build.`dotty-doc`
val `dotty-doc-bootstrapped` = Build.`dotty-doc-bootstrapped`
val `dotty-compiler` = Build.`dotty-compiler`
val `dotty-compiler-bootstrapped` = Build.`dotty-compiler-bootstrapped`
val `dotty-library` = Build.`dotty-library`
val `dotty-library-bootstrapped` = Build.`dotty-library-bootstrapped`
val `dotty-sbt-bridge` = Build.`dotty-sbt-bridge`
val `dotty-sbt-bridge-bootstrapped` = Build.`dotty-sbt-bridge-bootstrapped`
val `dotty-language-server` = Build.`dotty-language-server`
val `dotty-bench` = Build.`dotty-bench`
val `dotty-bench-bootstrapped` = Build.`dotty-bench-bootstrapped`
val `scala-library` = Build.`scala-library`
val `scala-compiler` = Build.`scala-compiler`
val `scala-reflect` = Build.`scala-reflect`
val scalap = Build.scalap
val dist = Build.dist
val `dist-bootstrapped` = Build.`dist-bootstrapped`

val `sbt-dotty` = Build.`sbt-dotty`
val `vscode-dotty` = Build.`vscode-dotty`

val scalaInstanceProject = Build.scalaInstanceProject

inThisBuild(Build.thisBuildSettings)
inScope(Global)(Build.globalSettings)
