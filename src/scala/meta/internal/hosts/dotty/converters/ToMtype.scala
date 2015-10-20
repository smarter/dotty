package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.dotty.reflect._
import scala.meta.internal.flags._
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

// This module exposes a method that can convert scala.reflect types into equivalent scala.meta types.
// See comments to ToMtree to learn more about how this conversion preserves the original syntax of those types.
trait ToMtype[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected implicit class XtensionGtypeToMtype(gtpe: dty.Type) {
    def toMtype: m.Type = gtpe.toMtypeArg.require[m.Type]
    def toMtypeArg: m.Type.Arg = tpeCache.getOrElseUpdate(gtpe, {
      val mtpe = gtpe match {
        case dty.NoPrefix =>
          unreachable
        case dty.NoType =>
          unreachable
        case dty.SuperType(thistpe, supertpe) => 
         require(thistpe.isInstanceOf[dty.ThisType] && thistpe.typeSymbol.isType && supertpe.typeSymbol.isType)
          val supersym = if (supertpe.isInstanceOf[dty.RefinedType]) dsy.NoSymbol else supertpe.typeSymbol
          val superqual = m.Name.Indeterminate(thistpe.typeSymbol.displayName).withMattrs(z.DefaultPrefix, thistpe.typeSymbol)
          val supermix = ({
            if (supersym == dsy.NoSymbol) m.Name.Anonymous()
            else m.Name.Indeterminate(supertpe.typeSymbol.displayName)
          }).withMattrs(thistpe, supersym)
          m.Type.Singleton(m.Term.Super(superqual, supermix))
        case tpe: dty.ThisType =>
          val sym = tpe.cls
          val ref = {
            if (sym.is(ModuleClass)) sym.companionModule.asTerm.toMname(z.DefaultPrefix)
            // TODO: should we really use z.DefaultPrefix here?
            else m.Term.This(m.Name.Indeterminate(sym.displayName).withMattrs(z.DefaultPrefix, sym)).withMattrs(gtpe.widen)
          }
          m.Type.Singleton(ref)
        case tpe @ dty.TermRef(pre, _) =>
          val sym = tpe.symbol
          val name = sym.asTerm.toMname(pre)
          val ref = pre match {
            case dty.NoPrefix =>
              name
            case pre if pre.typeSymbol.isStaticOwner =>
              name
            case pre: dty.SingletonType =>
              val m.Type.Singleton(preref) = pre.toMtype
              m.Term.Select(preref, name).inheritAttrs(name)
            case pre @ dty.TypeRef(dty.NoPrefix, name) if pre.symbol.is(Deferred) =>
              ???
            case pre: dty.TypeRef =>
              // TODO: wow, so much for the hypothesis that all post-typer types are representable with syntax
              // here's one for you: take a look at `context.unit.synthetics.get` in Typers.scala
              // the prefix of the selection is typed as `Typers.this.global.CompilationUnit#synthetics.type`
              // from what I can see, we should represent this type as an existential, i.e.
              // `_1.synthetics.type forSome { val _1: Typers.this.global.CompilationUnit }`
              // however that representation would require non-trivial effort to pull off
              // (because we'll have to carry around that m.Type.Existential and unwrap it when anyone wants to use it)
              // therefore for now I'm just putting a stub here
              name
            case _ =>
              throw new ConvertException(gtpe, s"unsupported type $gtpe, prefix = ${pre.getClass}, structure = ${gtpe.show}")
          }
          // NOTE: we can't just emit m.Type.Singleton(m.Term.Name(...).withDenot(pre, sym))
          // because in some situations (when the prefix is not stable) that will be a lie
          // because naked names are supposed to be usable without a prefix
          m.Type.Singleton(ref)
        case tpe: dty.TypeRef =>
          val pre = tpe.prefix
          val sym = tpe.symbol
          if (sym == ctx.definitions.RepeatedParamClass) {
            ???
          } else {
            val mname = sym.asType.toMname(pre)
            pre match {
              case dty.NoPrefix =>
                mname
              case pre if pre.typeSymbol.isStaticOwner =>
                mname
              case pre: dty.SingletonType =>
                val m.Type.Singleton(preref) = pre.toMtype
                m.Type.Select(preref, mname)
              case _ =>
                m.Type.Project(pre.toMtype, mname)
            }
          }
        // case tpe @ dty.RefinedType(parent, _) =>
        //   val refSym = tpe.refinedInfo.typeSymbol
        //   m.Type.Compound(Seq(parent.toMtype), refSym.toLogical.toMmember(dty.NoPrefix).stat)
        // case dty.AnnotatedType(annot, underlying) =>
        //   m.Type.Annotate(underlying.toMtype, Seq(annot.toMannot))
        case dty.ConstantType(const @ dco.Constants.Constant(tpe: dty.Type)) =>
          tpe.widen.toMtype
        case dty.ConstantType(const) =>
          const.toMlit
        case tpe @ dty.MethodType(_, params) =>
          val ret = tpe.resultType
          val gparamss = tpe.paramTypess.map(_.map(_.typeSymbol))
          val mparamss = gparamss.toLogical.map(_.map(_.toMmember(dty.NoPrefix).require[m.Term.Param]))
          m.Type.Method(mparamss, ret.toMtype)
        case _ =>
          throw new ConvertException(gtpe, s"unsupported type $gtpe, designation = ${gtpe.getClass}, structure = ${gtpe.show}")
      }
      mtpe.forceTypechecked
    })
  }
}