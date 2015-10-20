package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.ast.Helpers.XtensionTermOps
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

import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.Flags._

// This module exposes a method to convert from scala.meta types to scala.reflect types.
// The logic is mostly straightforward except for when we need to create symbols for compound and existential types.
trait ToGtype[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] {
  self: Api[A] =>

  protected implicit class XtensionMtypeToGtype(mtpe: m.Type.Arg) {
    private def gtypeBounds(mbounds: m.Type.Bounds): dty.TypeBounds = {
      val glo = mbounds.lo.map(_.toGtype).getOrElse(ctx.definitions.NothingType)
      val ghi = mbounds.hi.map(_.toGtype).getOrElse(ctx.definitions.AnyType)
      dty.TypeBounds(glo, ghi)
    }
    private implicit class RichGlobalSymbol(gsym: dsy.Symbol) {
      private def mkGterm(name: String, flags: FlagSet) = ctx.newSymbol(gsym, name.toTermName, flags, dty.NoType)
      private def mkGtype(name: String, flags: FlagSet) = ctx.newSymbol(gsym, name.toTypeName, flags, dty.NoType)
      def mkLabstractVal(name: String) = l.AbstractVal(mkGterm(name, Deferred | Method | Stable | Accessor))
      def mkLabstractVar(name: String) = l.AbstractVar(mkGterm(name, Deferred | Method | Accessor), mkGterm(name + nme.SETTER_SUFFIX, Deferred | Method | Accessor))
      def mkLabstractDef(name: String) = l.AbstractDef(mkGterm(name, Method))
      def mkLabstractType(name: String) = l.AbstractType(mkGtype(name, Deferred))
      def mkLtype(name: String) = l.Type(mkGtype(name, EmptyFlags))
      def mkLtermParameter(name: String) = l.TermParameter(mkGterm(name, Param))
      def mkLtypeParameter(name: String) = l.TypeParameter(mkGtype(name, Param | Deferred))
    }
    private implicit class RichLogicalSymbol(lsym: l.Symbol) {
      private def mimicMods(mmods: Seq[m.Mod], mtree: m.Tree): Unit = {
        val gsym = lsym.gsymbol // TODO: check that this is correct
        mmods.foreach({
          case mmod: m.Mod.Annot => ???
          case m.Mod.Private(m.Name.Anonymous()) => gsym.setFlag(Private)
          case m.Mod.Private(m.Term.This(_)) => gsym.setFlag(Private | Local)
          case m.Mod.Private(_) => ???
          case m.Mod.Protected(m.Name.Anonymous()) => gsym.setFlag(Protected)
          case m.Mod.Protected(m.Term.This(_)) => gsym.setFlag(Protected | Local)
          case m.Mod.Protected(_) => ???
          case mmod: m.Mod.Implicit => gsym.setFlag(Implicit)
          case mmod: m.Mod.Final => gsym.setFlag(Final)
          case mmod: m.Mod.Sealed => gsym.setFlag(Sealed)
          case mmod: m.Mod.Override => gsym.setFlag(Override)
          case mmod: m.Mod.Case => gsym.setFlag(Case)
          case mmod: m.Mod.Abstract => gsym.setFlag(Abstract)
          case mmod: m.Mod.Covariant => gsym.setFlag(Covariant)
          case mmod: m.Mod.Contravariant => gsym.setFlag(Contravariant)
          case mmod: m.Mod.Lazy => gsym.setFlag(Lazy)
          case mmod: m.Mod.ValParam => // do nothing
          case mmod: m.Mod.VarParam => // do nothing
        })
        // TODO: INTERFACE, MUTABLE, STATIC, PRESUPER, INCONSTRUCTOR, STABLE, *ACCESSOR, EXISTENTIAL
        if (gsym.is(Abstract) && gsym.is(Override)) { gsym.resetFlag(Abstract | Override); gsym.setFlag(AbsOverride) }
        if (mtree.isInstanceOf[m.Defn.Trait]) gsym.setFlag(Trait)
        mtree match { case mtree: m.Term.Param if mtree.default.nonEmpty => gsym.setFlag(DefaultParameterized); case _ => }
        //mtree match { case mtree: m.Defn.Var if mtree.rhs.isEmpty => gsym.setFlag(DefaultInit); case _ => }
      }
      private def gtparams(mtparams: Seq[m.Type.Param]): List[dsy.Symbol] = {
        mtparams.map(mtparam => {
          val htparam = mtparam.name.require[m.Name].denot.requireSymbol
          val gowner = { require(lsym.gsymbols.length == 1); lsym.gsymbol }
          val ltparam = symbolTable.lookupOrElseUpdate(htparam, gowner.mkLtypeParameter(mtparam.name.toString))
          ltparam.mimic(mtparam).gsymbol
        }).toList
      }
      private def gparams(mparams: Seq[m.Term.Param]): List[dsy.Symbol] = {
        mparams.map(mparam => {
          val hparam = mparam.name.require[m.Name].denot.requireSymbol
          val gowner = { require(lsym.gsymbols.length == 1); lsym.gsymbol }
          val lparam = symbolTable.lookupOrElseUpdate(hparam, gowner.mkLtermParameter(mparam.name.toString))
          lparam.mimic(mparam).gsymbol
        }).toList
      }
      private def mimicInfo(mtree: m.Tree): Unit = ???
      def mimic(mtree: m.Tree): l.Symbol = {
        //if (!lsym.gsymbol.hasRawInfo) {
        if (false) {
          import scala.language.reflectiveCalls
          mimicMods(mtree.require[{ def mods: Seq[m.Mod] }].mods, mtree)
          mimicInfo(mtree)
        }
        lsym
      }
    }
    private def gowner(mtree: m.Tree): dsy.Symbol = {
      // TODO: we probably need something other than NoSymbol for RefinedType.decls and ExistentialType.quants
      // I always had no idea about how this works in scala. I guess, it's time for find out :)
      dsy.NoSymbol
    }
    private def gprefix(hprefix: s.Prefix): dty.Type = {
      hprefix match {
        case s.Prefix.Zero => dty.NoPrefix
        case s.Prefix.Type(mtpe) => mtpe.require[m.Type].toGtype
      }
    }
    def toGtype: dty.Type = tpeCache.getOrElseUpdate(mtpe, {
      def loop(mtpe: m.Type.Arg): dty.Type = mtpe match {
        case mname: m.Type.Name =>
          dty.TypeRef(gprefix(mname.denot.prefix), symbolTable.convert(mname.denot.requireSymbol).gsymbol.asType)
        case m.Type.Select(mqual, mname) =>
          dty.TypeRef(loop(m.Type.Singleton(mqual)), symbolTable.convert(mname.denot.requireSymbol).gsymbol.asType)
        case m.Type.Project(mqual, mname) =>
          dty.TypeRef(loop(mqual), symbolTable.convert(mname.denot.requireSymbol).gsymbol.asType)
        case m.Type.Singleton(mref) =>
          def singleType(mname: m.Term.Name): dty.Type = {
            val gsym = symbolTable.convert(mname.denot.requireSymbol).gsymbol
            if (gsym.is(ModuleClass)) gsym.asClass.thisType
            else dty.TermRef(gprefix(mname.denot.prefix), gsym.asTerm)
          }
          def superType(msuper: m.Term.Super): dty.Type = {
            val gpre = gprefix(msuper.thisp.require[m.Name].denot.prefix)
            val gmixsym = msuper.superp.require[m.Name].denot.requireSymbol match {
              case s.Symbol.Zero => gpre.classSymbol.asClass.classInfo.instantiatedParents.reduceLeft(ctx.typeComparer.andType(_, _))
              case ssym => gpre.typeSymbol.info.baseTypeWithArgs(symbolTable.convert(ssym).gsymbol)
            }
            dty.SuperType(gpre, gmixsym)
          }
          mref match {
            case mname: m.Term.Name => singleType(mname)
            case m.Term.Select(_, mname) => singleType(mname)
            case mref: m.Term.This => symbolTable.convert(mref.qual.require[m.Name].denot.requireSymbol).gsymbol.asClass.thisType
            case mref: m.Term.Super => superType(mref)
          }
        case m.Type.Apply(mtpe, margs) =>
          loop(mtpe).appliedTo(margs.map(loop).toList)
        case m.Type.ApplyInfix(mlhs, mop, mrhs) =>
          loop(mtpe).appliedTo(List(loop(mlhs), loop(mrhs)))
        case m.Type.Function(mparams, mres) =>
          ???
        case m.Type.Tuple(melements) =>
          ???
        case m.Type.Compound(mtpes, mrefinement) =>
          ???
        case m.Type.Annotate(mtpe, mannots) =>
          ???
        case m.Type.Placeholder(mbounds) =>
          ???
        case m.Type.Arg.ByName(mtpe) =>
          ???
        case m.Type.Arg.Repeated(mtpe) =>
          ctx.definitions.RepeatedParamType.appliedTo(loop(mtpe))
        case mlit: m.Lit =>
          require(!mlit.value.isInstanceOf[scala.Symbol])
          dty.ConstantType(Constant(mlit.value))
      }
      loop(mtpe)
    })
  }
}