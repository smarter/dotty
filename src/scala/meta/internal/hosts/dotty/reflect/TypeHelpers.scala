package scala.meta.internal.hosts.dotty
package reflect

import scala.collection.mutable
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

trait TypeHelpers[A >: dtr.Untyped <: dty.Type] {
  self: ReflectToolkit[A] =>


  implicit class RichHelperType(tpe: dty.Type) {
    def paramss: List[List[dsy.Symbol]] = tpe.paramTypess.map(_.map(_.typeSymbol))
    /*
    def etaReduce: Type = EtaReduce.unapply(tpe).getOrElse(tpe)
    def directBaseTypes: List[Type] = ???
    */
  }
  implicit class RichHelperClassInfoType(tpe: dty.ClassInfo) {
    def realParents: List[dty.Type] = {
      tpe.parents match {
        case classTpe :: traitTpe :: rest if traitTpe <:< classTpe =>
          // NOTE: this is obviously imprecise, but at least it captures the common case
          traitTpe :: rest
        case other =>
          other
      }
    }
  }

  /*
  object EtaReduce {
    def unapply(tpe: Type): Option[Type] = tpe match {
      case PolyType(tparams, TypeRef(pre, sym, targs)) =>
        val canReduce = tparams.zip(targs).forall({
          case (tparam, TypeRef(_, targsym, Nil)) => tparam == targsym
          case _ => false
        })
        if (canReduce) Some(TypeRef(pre, sym, Nil))
        else None
      case _ =>
        None
    }
  }
  */
}
