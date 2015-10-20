package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.dotty.reflect._
import scala.meta.internal.flags._

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

// This a random grab bag of utilities.
// Ideally, we'd find better places for them than this file.
trait ToM[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected def mfakector(gtpe: dty.Type): m.Ctor.Primary = {
    val ctorTpe = dty.MethodType(Nil, gtpe)
    val ctorOwner = symbolTable.convert(gtpe.typeSymbol.toLogical)
    val ctorSym = s.Symbol.Global(ctorOwner, "<init>", s.Signature.Method(ctorTpe.jvmsig))
    val ctorDenot = s.Denotation.Single(s.Prefix.Type(gtpe.toMtype), ctorSym)
    val ctorName = m.Ctor.Name(gtpe.typeSymbol.displayName).withMattrs(ctorDenot, ctorTpe)
    m.Ctor.Primary(Nil, ctorName, List(List()))
  }
}