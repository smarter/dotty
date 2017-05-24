# IDE support for Dotty, the experimental Scala compiler

## Prerequistes
To use this in your own Scala project, you must first get it to compile with
Dotty, please follow the instructions at https://github.com/lampepfl/dotty-example-project

## Starting Visual Studio Code from sbt
Once your project succesfully compiles with dotty, you can simply use the
`launchIDE` command provided by the sbt-dotty plugin:

```shell
sbt launchIDE
```

## More information

See http://dotty.epfl.ch/docs/usage/ide-support.html
