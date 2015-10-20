package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.{ast => m}
import scala.meta.internal.hosts.dotty.reflect._
import scala.meta.internal.flags._

import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.core.{Types => dty}

// This module exposes a method that can convert scala.reflect annotations into equivalent scala.meta mods.
// There's not much to say about this conversion except that it's a really lossy one:
// not only we have to deal with desugared trees in annotation arguments,
// but we also have to tolerate the loss of the constructor symbol (because g.AnnotationInfos only have a type).
// See comments to ToMtree to learn more about preserving original syntax.
trait ToMannot[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  // TODO
}