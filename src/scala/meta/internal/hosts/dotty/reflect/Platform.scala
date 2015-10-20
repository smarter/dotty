package scala.meta.internal.hosts.dotty
package reflect

import org.scalameta.invariants._

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

import dotty.tools.dotc.core.Flags._

trait Platform[A >: dtr.Untyped <: dty.Type] {
  self: ReflectToolkit[A] =>

  implicit class RichPlatformType(tpe: dty.Type) {
    val defn = ctx.definitions

    // TODO: I don't like having to reproduce this logic by hand
    // but I have no idea what compiler API to use here
    private def jvmname(sym0: dsy.Symbol): String = {
      def loop(sym: dsy.Symbol): String = {
        val prefix = {
          if (sym.owner == defn.RootClass || sym.owner == defn.EmptyPackageClass) ""
          else {
            val ownerName = loop(sym.owner)
            val separator = if (sym.owner.is(PackageClass)) "/" else if (sym.owner.is(ModuleClass)) "" else "$"
            ownerName + separator
          }
        }
        val suffix = if (sym.is(Module) && !sym.is(Package)) "$" else ""
        prefix + sym.name.toString + suffix
      }
      loop(sym0)
    }
    def jvmsig: String = {
      tpe match {
        case tpe: dty.TypeRef =>
          val sym = tpe.symbol
          if (sym == defn.UnitClass || sym == defn.BoxedUnitClass) "V"
          else if (sym == defn.BooleanClass) "Z"
          else if (sym == defn.CharClass) "C"
          else if (sym == defn.ByteClass) "B"
          else if (sym == defn.ShortClass) "S"
          else if (sym == defn.IntClass) "I"
          else if (sym == defn.FloatClass) "F"
          else if (sym == defn.LongClass) "J"
          else if (sym == defn.DoubleClass) "D"
          else if (sym == defn.ArrayClass) "["
          else "L" + jvmname(sym) + ";"
        case tpe @ dty.MethodType(_, paramTypes) =>
          s"(" + paramTypes.map(_.jvmsig).mkString("") + ")" + tpe.resultType.jvmsig
        case dty.ConstantType(value) =>
          value.tpe.widen.jvmsig
      }
    }
  }

  implicit class RichPlatformSymbol(sym: dsy.Symbol) {
    def jvmsig: String = {
      val erasedInfo = dte.transformInfo(sym, sym.info)
      val sigInfo =
        if (sym.isConstructor) {
          // TODO(@smarter): We do something similar in Erasure#DefDef,
          // maybe transformInfo should directly return the correct signature?
          val dty.MethodType(pnames, ptypes) = erasedInfo
          dty.MethodType(pnames, ptypes, ctx.definitions.UnitType)
        } else {
          erasedInfo
        }
      //dte.erasure(sym.info).jvmsig
      sigInfo.jvmsig
    }
  }
}