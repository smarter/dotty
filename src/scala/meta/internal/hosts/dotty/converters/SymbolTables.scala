package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.collections._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.dotty.reflect._
import java.util.UUID.randomUUID

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

// This module tracks the correspondence between scala.reflect and scala.meta symbols.
// It can also do automatic conversion between globally visible symbols,
// but it needs help when dealing with local symbols.
trait SymbolTables[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected class SymbolTable {
    private val symCache = TwoWayCache[l.Symbol, s.Symbol]()

    // TODO: `convert` is somewhat copy/pasted from core/quasiquotes/Macros.scala
    // however, there's no way for us to share those implementations until we bootstrap
    def convert(lsym: l.Symbol): s.Symbol = symCache.getOrElseUpdate(lsym, lsym match {
      case l.Self(gowner) =>
        val sowner = convert(gowner.toLogical)
        sowner match {
          case sowner: s.Symbol.Global => s.Symbol.Global(sowner, "this", s.Signature.Self)
          case _ => s.Symbol.Local(randomUUID().toString)
        }
      case lsym =>
        def isGlobal(gsym: dsy.Symbol): Boolean = {
          def definitelyLocal = gsym == dsy.NoSymbol || gsym.name.toString.startsWith("<local ") || (gsym.owner.is(Method) && !gsym.is(Param))
          def isParentGlobal = gsym.is(Package) || isGlobal(gsym.owner)
          !definitelyLocal && isParentGlobal
        }
        def signature(gsym: dsy.Symbol): s.Signature = {
          if (gsym.is(Method) && !gsym.isGetter) s.Signature.Method(gsym.jvmsig)
          else if (gsym.isTerm || (gsym.is(Deferred) && gsym.name.endsWith(nme.SINGLETON_SUFFIX))) s.Signature.Term
          else if (gsym.isType) s.Signature.Type
          else unreachable(debug(gsym, gsym.flags, gsym.getClass, gsym.owner))
        }
        val gsym = lsym.gsymbol
        require(!gsym.is(ModuleClass) && !gsym.is(PackageClass))
        if (gsym == dsy.NoSymbol) s.Symbol.Zero
        else if (gsym == ctx.definitions.RootClass.companionModule) s.Symbol.RootPackage
        else if (gsym == ctx.definitions.EmptyPackageVal) s.Symbol.EmptyPackage
        else if (isGlobal(gsym)) s.Symbol.Global(convert(gsym.owner.toLogical), gsym.name.decode.toString, signature(gsym))
        else s.Symbol.Local(randomUUID().toString)
    })

    def lookupOrElseUpdate(lsym: l.Symbol, ssym: => s.Symbol): s.Symbol = symCache.getOrElseUpdate(lsym, ssym)

    def convert(ssym: s.Symbol): l.Symbol = symCache.getOrElseUpdate(ssym, {
      def resolve(lsym: l.Symbol, name: String, hsig: s.Signature): l.Symbol = hsig match {
        case s.Signature.Type => lsym.gsymbol.info.decl(name.toTypeName).symbol.toLogical
        case s.Signature.Term => lsym.gsymbol.info.decl(name.toTermName).suchThat(galt => galt.isGetter || !galt.is(Method)).symbol.toLogical
        case s.Signature.Method(jvmsig) => lsym.gsymbol.info.decl(name.toTermName).suchThat(galt => galt.is(Method) && galt.jvmsig == jvmsig).symbol.toLogical
        case s.Signature.TypeParameter => lsym.gsymbol.typeParams.filter(_.name.toString == name).head.toLogical
        case s.Signature.TermParameter => ??? //lsym.gsymbol.paramss.flatten.filter(_.name.toString == name).head.toLogical
        case s.Signature.Self => l.Self(lsym.gsymbol)
      }
      ssym match {
        case s.Symbol.Zero => l.Zero
        case s.Symbol.RootPackage => l.Package(ctx.definitions.RootPackage, ctx.definitions.RootClass)
        case s.Symbol.EmptyPackage => l.Package(ctx.definitions.EmptyPackageVal, ctx.definitions.EmptyPackageClass)
        case s.Symbol.Global(howner, name, hsig) => resolve(convert(howner), name, hsig)
        case s.Symbol.Local(id) => throw new ConvertException(ssym, s"implementation restriction: internal cache has no symbol associated with $ssym")
      }
    })

    def lookupOrElseUpdate(ssym: s.Symbol, lsym: => l.Symbol): l.Symbol = symCache.getOrElseUpdate(ssym, lsym)
  }
}