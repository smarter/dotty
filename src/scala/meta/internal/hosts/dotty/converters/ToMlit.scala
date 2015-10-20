package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.hosts.dotty.reflect._

import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.{TypeErasure => dte}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.core.{Names => dna}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

trait ToMlit[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected implicit class XtensionGconstToMlit(gconst: dco.Constants.Constant) {
    def toMlit: m.Lit = {
      require(!gconst.value.isInstanceOf[dty.Type] && !gconst.value.isInstanceOf[dsy.Symbol])
      val msytree = m.Lit(gconst.value)
      msytree.withMattrs(gconst.tpe).forceTypechecked
    }
  }
}