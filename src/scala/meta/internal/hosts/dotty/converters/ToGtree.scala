package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.tools.nsc.reporters.StoreReporter
import scala.meta.dialects.Scala211
import scala.meta.internal.{ast => m}
import scala.meta.internal.prettyprinters._
import scala.meta.internal.hosts.dotty.reflect._

import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.core.{Names => dna}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

// This module exposes a method to convert from scala.meta trees to scala.reflect trees.
// Nothing is implemented yet, but we'll have to at least take a stab at it to enable scala.meta macros.
trait ToGtree[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected implicit class XtensionMtreeToGtree(mtree: m.Tree) {
    def toGtree: g.Tree = {
      ???
    }
  }
}