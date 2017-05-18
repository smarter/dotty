// Include the sources of the sbt-dotty plugin in the project build,
// so that we can use the current in-development version of the plugin
// in our build instead of a released version.

sources in Compile ++= (baseDirectory.value / "../sbt-dotty/src" ** ("*.scala" | "*.java")).get

libraryDependencies += Dependencies.`jackson-databind`
