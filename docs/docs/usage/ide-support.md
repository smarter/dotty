---
layout: doc-page
title: "IDE support for Dotty"
---

Dotty comes built-in with the Dotty Language Server, an implementation of the
[Language Server Protocol](https://github.com/Microsoft/language-server-protocol),
which means that any editor that implements support for the LSP can get a great
Dotty integration. Currently, the only IDE we officially support
is [Visual Studio Code](https://code.visualstudio.com/).

Prerequisites
============
To use this in your own Scala project, you must first get it to compile with
Dotty, please follow the instructions at https://github.com/lampepfl/dotty-example-project

Usage
=====
1. Install [Visual Studio Code](https://code.visualstudio.com/).
2. In your project, run:
```shell
sbt launchIDE
```

Status
======

## Fully supported features:
- Typechecking as you type to show compiler errors/warnings
- Type information on hover
- Go to definition (in the current project)
- Find all references

## Partially working features:
- Completion
- Renaming
- Go to definition in external projects

## Unimplemented features:
- Documentation on hover
- Formatting code (requires integrating with scalafmt)
- Quick fixes (probably by integrating with scalafix)

## Current limitations, to be fixed:
- Projects should be compiled with sbt before starting the IDE, this is
  automatically done for you if you run `sbt launchIDE`.
- Once the IDE is started, source files that are not opened in the IDE
  should not be modified in some other editor, the IDE won't pick up
  these changes.
- Not all compiler errors/warnings are displayed, just those occuring
  during typechecking.


Feedback
========
Please report issues on https://github.com/lampepfl/dotty/issues,
you can also come chat with use on the
[Dotty gitter channel](https://gitter.im/lampepfl/dotty)!
