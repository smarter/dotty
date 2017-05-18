# IDE support for Dotty, the experimental Scala compiler

This is still very experimental. You can try it using the following steps.

## Build the `ide` branch of Dotty

```shell
git clone --recursive http://github.com/lampepfl/dotty
git remote add staging http://github.com/dotty-staging/dotty
git fetch staging
git checkout ide
sbt ";sbt-dotty/publishLocal;dotty-bootstrapped/publishLocal"
```

## Use Dotty in your own project

Follow https://github.com/lampepfl/dotty-example-project, but use the version of Dotty you built:
```scala
scalaVersion := "0.1.1-bin-SNAPSHOT"
```
and the version of sbt-dotty you built:

```scala
addSbtPlugin("ch.epfl.lamp" % "sbt-dotty" % "0.1.0-RC4-SNAPSHOT")
```


## Start the IDE from sbt
In your own project:

```shell
sbt launchIDE
```

Note: you should not launch Visual Studio Code manually when working on a Dotty
project, always use the command above instead.
