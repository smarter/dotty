package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.{ast => m}
import scala.meta.internal.hosts.dotty.reflect._

import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.core.{Types => dty}

// This module exposes methods to convert from scala.meta members to scala.reflect symbols.
// To be more precise, it returns logical symbols instead of scala.reflect ones, but that's almost the same.
trait ToGsymbol[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected implicit class XtensionMnameToLsymbols(mname: m.Name) {
    def toLsymbols: Seq[l.Symbol] = {
      mname.denot.symbols.map(symbolTable.convert)
    }
  }

  protected implicit class XtensionMmemberToLsymbols(mmember: m.Member) {
    def toLsymbols: Seq[l.Symbol] = mmember.name.require[m.Name].toLsymbols
  }
}