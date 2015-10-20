package scala.meta

import scala.meta.internal.hosts.dotty.contexts.{Compiler => Compiler}
import scala.meta.internal.hosts.dotty.contexts.{Adapter => AdapterImpl}

import dotty.tools.dotc.core.Contexts.{Context => DottyContext}

trait Context extends SemanticContext with InteractiveContext

object Context {
  def apply(artifacts: Artifact*)(implicit resolver: Resolver): Context = {
    implicit val ctx: DottyContext = null //TODO
    new AdapterImpl(Compiler(), Domain(artifacts: _*)) {
      override def toString = s"""Context(${artifacts.mkString(", ")})"""
    }
  }
}
