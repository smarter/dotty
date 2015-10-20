package scala.meta.internal.hosts.dotty
package reflect

import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable
import org.scalameta.adt._

import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

import dotty.tools.dotc.core.Flags._


trait SymbolHelpers[A >: dtr.Untyped <: dty.Type] {
  self: ReflectToolkit[A] =>

  implicit class RichHelperSymbol(sym: dco.Symbols.Symbol)(implicit val ctx: Context) {
    def paramss: List[List[dsy.Symbol]] = sym.info.paramss

    def isAnonymous: Boolean = sym.name.isAnonymous

    def isIntrinsic: Boolean = {
      false
    }

    def prefix: dty.Type = {
      if (sym.is(Param)) dty.NoPrefix
      else if (sym.owner.isTerm) dty.NoPrefix
      else if (sym.isConstructor) sym.owner.prefix
      else sym.owner.thisType
    }
  }
}
