package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.language.implicitConversions
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

import dotty.tools.dotc.core.Flags._

trait ToMname[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected implicit class XtensionGsymbolToMname[T <: dsy.Symbol, U <: m.Name](gsym: T)
    (implicit ev: GsymbolToMname[T, U], ctx: Context) {
    def toMname(gpre0: dty.Type, value: String = gsym.displayName): U = {
      // TODO: account for ctornames?
      // TODO: what about anonymous names?
      val gpre = if (gpre0 == DefaultPrefix) gsym.prefix else gpre0
      val mname = {
        if (gsym.isTerm) m.Term.Name(value).withMattrs(gpre, gsym).asInstanceOf[U]
        else if (gsym.isType) m.Type.Name(value).withMattrs(gpre, gsym).asInstanceOf[U]
        else abort(gsym)
      }
      mname.forceTypechecked
    }
  }

  val z: AnyRef = null

  protected var DefaultPrefix: dty.Type = _
  protected implicit class XtensionDefaultPrefix(_z: z.type) {
    def DefaultPrefix = {
      if (self.DefaultPrefix eq null) {
        val dummySymbol = ctx.newSymbol(dsy.NoSymbol, "<defaultPrefix>".toTypeName, EmptyFlags, dty.NoType)
        self.DefaultPrefix = dty.TypeRef(dty.NoPrefix, dummySymbol)
      }
      self.DefaultPrefix
    }
    def showRaw(tree: g.Tree, printIds: Boolean = false, printTypes: Boolean = false): String = tree.show
  }

  protected trait GsymbolToMname[T, U]
  object GsymbolToMname {
    implicit def gsymbolToMname: GsymbolToMname[dsy.Symbol, m.Name] = null
    implicit def gtermSymbolToMtermName[T <: dsy.TermSymbol]: GsymbolToMname[T, m.Term.Name] = null
    implicit def gtypeSymbolToMtypeName[T <: dsy.TypeSymbol]: GsymbolToMname[T, m.Type.Name] = null
  }
}