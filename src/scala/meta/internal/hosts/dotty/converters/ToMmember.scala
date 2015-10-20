package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.collections._
import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.tools.nsc.{Global => ScalaGlobal}
import scala.reflect.internal.Flags._
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.semantic.{Context => ScalametaSemanticContext}
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

import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.NameOps._

// This module exposes a method that can convert scala.reflect symbols into equivalent scala.meta members.
// There are some peculiarities that you'll need to know about it:
//
// 1) The conversion always requires a prefix (i.e. a scala.reflect type), because
// our members track prefixes to avoid accidental mishaps and the inconvenience of typeSignatureIn/asSeenFrom.
// Consequently, the output m.Member might change its structural parts based on the prefix,
// e.g. `t"List".defs("head")` will look like `def head: A = ???`,
// while `t"List[Int]".defs("head")` will look like `def head: Int = ???`.
//
// 2) The conversion actually works not with dsy.Symbol, but with l.Symbol (aka logical symbol).
// That's because scala.reflect symbols and scala.meta members don't have a one-to-one correspondence
// (e.g. field + getter + setter collapse into a single m.Defn.Var and certain dsy.Symbols, e.g. $isInstanceOf,
// don't even have a representation in the scala.meta world).
//
// 3) The conversion not only supports lookup within scala.meta ASTs (i.e. AST persistence),
// but it can also operate in legacy mode, where it rebuilds scala.meta-compliant metadata from dsy.Symbols.
//
// 4) See comments to ToMtree to learn more about how this conversion preserves the original syntax of those types.
trait ToMmember[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] {
  self: Api[A] =>

  protected implicit class XtensionLsymbolToMmember(lsym: l.Symbol) {
    private def mmods(gpre: dty.Type, lsym: l.Symbol): Seq[m.Mod] = {
      def annotationMods(lsym: l.Symbol): Seq[m.Mod] = {
        // TODO
        Seq()
      }
      def accessQualifierMods(lsym: l.Symbol): Seq[m.Mod] = {
        val gsym = lsym.gsymbol
        val gpriv = gsym.privateWithin.orElse(gsym.owner)
        if (gsym.is(Local)) {
          val maccessBoundary = m.Term.This(m.Name.Anonymous().withMattrs(gpre, gpriv))
          if (gsym.is(Protected)) List(m.Mod.Protected(maccessBoundary))
          else if (gsym.is(Private)) List(m.Mod.Private(maccessBoundary))
          else unreachable(debug(gsym, gsym.flags, gsym.getClass, gsym.owner))
        } else if (gsym.privateWithin != null && gsym.privateWithin.exists && gpriv != dsy.NoSymbol) {
          // TODO: `private[pkg] class C` doesn't have Private in its flags
          // so we need to account for that!
          // TODO: we probably need to account for gpre instead of blindly using z.DefaultPrefix
          val maccessBoundary = gpriv.toMname(z.DefaultPrefix).require[m.Name.Qualifier]
          if (gsym.is(Protected)) List(m.Mod.Protected(maccessBoundary))
          else List(m.Mod.Private(maccessBoundary))
        } else {
          val maccessBoundary = m.Name.Anonymous().withMattrs(gpre, gsym.owner)
          if (gsym.is(Protected)) List(m.Mod.Protected(maccessBoundary))
          else if (gsym.is(Private)) List(m.Mod.Private(maccessBoundary))
          else Nil
        }
      }
      def otherMods(lsym: l.Symbol): Seq[m.Mod] = {
        val gsym = lsym.gsymbol
        val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
        if (gsym.is(Implicit)) mmods += m.Mod.Implicit()
        if (gsym.is(Final)) mmods += m.Mod.Final()
        if (gsym.is(Sealed)) mmods += m.Mod.Sealed()
        if (gsym.is(Override)) mmods += m.Mod.Override()
        if (gsym.is(Case)) mmods += m.Mod.Case()
        if (gsym.is(Abstract) && lsym.isInstanceOf[l.Clazz]) mmods += m.Mod.Abstract()
        if (gsym.is(Covariant)) mmods += m.Mod.Covariant()
        if (gsym.is(Contravariant)) mmods += m.Mod.Contravariant()
        if (gsym.is(Lazy)) mmods += m.Mod.Lazy()
        mmods.toList
      }
      def valVarParamMods(lsym: l.Symbol): Seq[m.Mod] = {
        val mmods = scala.collection.mutable.ListBuffer[m.Mod]()
        val ggetter = lsym.gsymbol.owner.filter(_.isPrimaryConstructor).symbol.owner.info.member(lsym.gsymbol.name).symbol
        val gfield = ggetter.owner.info.member(ggetter.symbol.name.asTermName.fieldName).symbol
        val isApplicable = lsym.gsymbol.owner.isPrimaryConstructor && gfield != dsy.NoSymbol
        if (isApplicable && gfield.is(Mutable)) mmods += m.Mod.VarParam()
        if (isApplicable && !gfield.is(Mutable) && !gfield.owner.is(Case)) mmods += m.Mod.ValParam()
        mmods.toList
      }
      val result = annotationMods(lsym) ++ accessQualifierMods(lsym) ++ otherMods(lsym) ++ valVarParamMods(lsym)
      // TODO: we can't discern `class C(x: Int)` and `class C(private[this] val x: Int)`
      // so let's err on the side of the more popular option
      if (lsym.gsymbol.owner.isPrimaryConstructor) result.filter({
        case m.Mod.Private(m.Term.This(_)) => false
        case _ => true
      }) else result
    }
    def toMmember(gpre: dty.Type): m.Member = ldenotToMmemberIndex.getOrElseUpdate(l.Denotation(gpre, lsym), {
      def approximateSymbol(lsym: l.Symbol): m.Member = {
        // NOTE: we don't need to clear the Local_SUFFIX_STRING from the name of `lsym.gsymbol`
        // because it's always guaranteed not to end with Local_SUFFIX_STRING
        // see LogicalSymbols.scala for more information
        lazy val gsym = lsym.gsymbol
        lazy val ginfo = gpre.memberInfo(gsym.moduleClass.orElse(gsym))
        lazy val gtparams = ginfo.typeParams
        lazy val gvparamss = ginfo.paramss
        lazy val gtpe = {
          // NOTE: strips off only those vparams and tparams that are part of the definition
          // we don't want to, for example, damage type lambdas
          def loop(gtpe: dty.Type): dty.Type = gtpe match {
            case gtpe @ dty.MethodType(_, gvparams) =>
              val gret = gtpe.resultType
              if (gvparams.forall(gsym => gvparamss.flatten.exists(_ == gsym))) loop(gret)
              else gtpe
            case gtpe: dty.PolyParam =>
              //if (gtparams.forall(gsym => gtparams.exists(_ == gsym))) loop(gret)
              //else gret
              loop(gtpe.resultType)
            case _ =>
              gtpe
          }
          loop(ginfo)
        }
        lazy val mmods = {
          val mffi: List[m.Mod.Ffi] = {
            def ffiScalaIntrinsic(gmeth: dsy.Symbol) = {
              val className = gmeth.owner.info.jvmsig
              val methodName = gmeth.name.toString
              val methodSig = gmeth.info.jvmsig
              List(m.Mod.Ffi(s"scalaIntrinsic($className, $methodName, $methodSig)"))
            }
            def ffiJvmField(gfield: dsy.Symbol) = {
              val className = gfield.owner.info.jvmsig
              val fieldName = gfield.name.toString
              val fieldSig = gfield.info.jvmsig
              List(m.Mod.Ffi(s"jvmField($className, $fieldName, $fieldSig)"))
            }
            def ffiJvmMethod(gmeth: dsy.Symbol) = {
              val className = gmeth.owner.info.jvmsig
              val methodName = gmeth.name.toString
              val methodSig = gmeth.info.jvmsig
              List(m.Mod.Ffi(s"jvmMethod($className, $methodName, $methodSig)"))
            }
            def ffiJvmErasure(gtpe: dty.Type) = {
              val className = gtpe.jvmsig
              List(m.Mod.Ffi(s"jvmErasure($className)"))
            }
            def ffiJvmPackage(gsym: dsy.Symbol) = {
              val packageName = gsym.fullName
              List(m.Mod.Ffi(s"jvmPackage($packageName)"))
            }
            lsym match {
              case l.AbstractVal(gget) =>
                ffiJvmMethod(gget)
              case l.AbstractVar(gget, gset) =>
                ffiJvmMethod(gget)
              case l.AbstractDef(gsym) =>
                if (gsym.isIntrinsic) ffiScalaIntrinsic(gsym)
                else ffiJvmMethod(gsym)
              case l.AbstractType(gsym) =>
                ffiJvmErasure(gsym.info)
              case l.Val(gfield, gget) =>
                if (gget == dsy.NoSymbol) ffiJvmField(gfield)
                else ffiJvmMethod(gget)
              case l.Var(gfield, gget, _) =>
                if (gget == dsy.NoSymbol) ffiJvmField(gfield)
                else ffiJvmMethod(gget)
              case l.Def(gsym) =>
                if (gsym.isIntrinsic) ffiScalaIntrinsic(gsym)
                else ffiJvmMethod(gsym)
              case l.Type(gsym) =>
                ffiJvmErasure(gsym.info)
              case l.Clazz(gsym) =>
                ffiJvmErasure(gsym.info)
              case l.Trait(gsym) =>
                ffiJvmErasure(gsym.info)
              case l.Object(gmodule, gmoduleClass) =>
                ffiJvmErasure(gmodule.info)
              case l.Package(gmodule, gmoduleClass) =>
                ffiJvmPackage(gmodule)
              case l.PackageObject(gmodule, gmoduleClass) =>
                ffiJvmErasure(gmodule.info)
              case l.PrimaryCtor(gsym) =>
                ffiJvmMethod(gsym)
              case l.SecondaryCtor(gsym) =>
                ffiJvmMethod(gsym)
              case l.TermParameter(gsym) if gsym.is(DefaultParameterized) =>
                val paramPos = gsym.owner.paramss.flatten.indexWhere(_.name == gsym.name)
                require(paramPos != -1)
                val gdefaultGetterName = gsym.owner.name + "$default$" + (paramPos + 1)
                var gdefaultGetterOwner = if (!gsym.owner.isConstructor) gsym.owner.owner else gsym.owner.owner.linkedClass
                val gdefaultGetter = gdefaultGetterOwner.info.decl(gdefaultGetterName.toTermName.name.encode).symbol
                require(gdefaultGetter != dsy.NoSymbol && debug(gsym, gdefaultGetterOwner, gdefaultGetterName))
                ffiJvmMethod(gdefaultGetter)
              // TODO: should we also generate ffi information for type parameters? what should it look like?
              case _ =>
                Nil
            }
          }
          mffi ++ this.mmods(gpre, lsym)
        }
        lazy val mname = {
          def ctorName(gsym: dsy.Symbol) = {
            m.Ctor.Name(gsym.owner.displayName).withMattrs(gpre, gsym)
          }
          def paramName(gsym: dsy.Symbol) = {
            if (gsym.name.isAnonymous) m.Name.Anonymous().withMattrs(z.DefaultPrefix, gsym)
            else gsym.toMname(z.DefaultPrefix)
          }
          lsym match {
            case l.AbstractVal(gsym) =>
              gsym.toMname(gpre)
            case l.PackageObject(gmodule, gmoduleClass) =>
              m.Term.Name(gmodule.owner.displayName).withMattrs(gpre, gmodule)
            case l.PrimaryCtor(gsym) =>
              ctorName(gsym)
            case l.SecondaryCtor(gsym) =>
              ctorName(gsym)
            case l.TermParameter(gsym) =>
              paramName(gsym)
            case l.TypeParameter(gsym) =>
              paramName(gsym)
            case _ =>
              gsym.toMname(gpre)
          }
        }
        lazy val mtparams = gtparams.map(gtparam => l.TypeParameter(gtparam).toMmember(dty.NoPrefix).require[m.Type.Param])
        lazy val mvparamss = gvparamss.map(_.map(gvparam => l.TermParameter(gvparam).toMmember(dty.NoPrefix).require[m.Term.Param]))
        lazy val mtpe = lsym match {
          case l.Val(_, _) =>
            gtpe.underlyingIfRepeated(isJava = false).toMtype
          case l.Var(_, _, _) =>
            gtpe.underlyingIfRepeated(isJava = false).toMtype
          case _ =>
            gtpe.toMtype
        }
        lazy val mtpearg = gtpe.toMtypeArg
        lazy val mtpebounds = gtpe match {
          case gtpe @ dty.TypeBounds(glo, ghi) =>
            val mlo = if (glo =:= ctx.definitions.NothingType) None else Some(glo.toMtype)
            val mhi = if (ghi =:= ctx.definitions.AnyType) None else Some(ghi.toMtype)
            m.Type.Bounds(mlo, mhi)
        }
        lazy val mbody: m.Term = {
          val munknownTerm = m.Term.Name("???").withMattrs(z.DefaultPrefix, ctx.definitions.`Predef???Class`)
          lsym match {
            case l.SecondaryCtor(gsym) =>
              val gctor = gsym.owner.primaryConstructor
              val mctorref = m.Ctor.Name(gsym.owner.displayName).withMattrs(gpre, gctor)
              m.Term.Apply(mctorref, List(munknownTerm))
            case _ =>
              munknownTerm
          }
        }
        lazy val mmaybeBody = Some(mbody)
        lazy val mfakector = self.mfakector(gtpe)
        lazy val mctor = {
          if (lsym.isInstanceOf[l.Clazz] || lsym.isInstanceOf[l.Object]) {
            val gctorsym = lsym.gsymbol.moduleClass.orElse(lsym.gsymbol).primaryConstructor
            if (gctorsym != dsy.NoSymbol) {
              val gctorinfo = gpre.memberInfo(gctorsym)
              val mctorname = m.Ctor.Name(gsym.displayName).withMattrs(gpre, gctorsym)
              var mctorparamss = {
                if (lsym.isInstanceOf[l.Clazz]) gctorinfo.paramss.map(_.map(gvparam => l.TermParameter(gvparam).toMmember(dty.NoPrefix).require[m.Term.Param]))
                else Nil // NOTE: synthetic constructors for modules have a fake List(List()) parameter list
              }
              if (mctorparamss.length == 1 && mctorparamss.flatten.length == 0) mctorparamss = Nil
              m.Ctor.Primary(this.mmods(gpre, l.PrimaryCtor(gctorsym)), mctorname, mctorparamss)
            } else {
              mfakector
            }
          } else {
            mfakector
          }
        }
        lazy val mtemplate = {
          val gparents = ginfo match {
            case gtpe: dty.ClassInfo => gtpe.realParents
            // case g.PolyType(_, gtpe: g.ClassInfoType) => gtpe.realParents
          }
          val mparents = gparents.map(gparent => {
            val mtpe = gparent.toMtype
            var gctor = gparent.typeSymbol.primaryConstructor.orElse(gparent.typeSymbol)
            //if (gctor.name == nme.MIXIN_CONSTRUCTOR) gctor = gparent.typeSymbol
            val mctor = m.Ctor.Name(gparent.typeSymbol.name.toString).withMattrs(gparent, gctor)
            mtpe.ctorRef(mctor).require[m.Ctor.Call]
          })
          // TODO: apply gpre to mselftpe
          val mselftpe = if (gsym.asClass.classInfo.selfType.typeSymbol != gsym) Some(gsym.asClass.classInfo.selfType.toMtype) else None
          val mself = m.Term.Param(Nil, m.Name.Anonymous(), mselftpe, None)
          m.Template(Nil, mparents, mself, Some(mstats))
        }
        lazy val mstats = LazySeq({
          val gstatowner = gsym match { case gclass: dsy.ClassSymbol => gclass; case gmodule if gmodule.is(ModuleVal) => gmodule.moduleClass.asClass }
          //val gstatpre = gpre.memberType(gstatowner)
          val gstatpre = gpre.memberInfo(gstatowner)
          val ldecls = gstatpre.decls.toLogical
          val lcensoredDecls = ldecls.filter(!_.isInstanceOf[l.PrimaryCtor])
          lcensoredDecls.map(_.toMmember(gstatpre)).map(_.stat)
        })
        lazy val mmaybeDefault = if (gsym.is(DefaultParameterized)) Some(mbody) else None
        lazy val mcontextbounds = Nil // TODO
        lsym match {
          case l.Zero => unreachable(debug(lsym.gsymbol, lsym.gsymbol.flags, lsym.gsymbol.getClass, lsym.gsymbol.owner))
          case _: l.AbstractVal => m.Decl.Val(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), mtpe).member
          case _: l.AbstractVar => m.Decl.Var(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), mtpe).member
          case _: l.AbstractDef if lsym.gsymbol.isIntrinsic => m.Defn.Def(mmods, mname.require[m.Term.Name], mtparams, mvparamss, Some(mtpe), mbody)
          case _: l.AbstractDef => m.Decl.Def(mmods, mname.require[m.Term.Name], mtparams, mvparamss, mtpe)
          case _: l.AbstractType => m.Decl.Type(mmods, mname.require[m.Type.Name], mtparams, mtpebounds)
          case _: l.Val => m.Defn.Val(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), Some(mtpe), mbody).member
          case _: l.Var => m.Defn.Var(mmods, List(m.Pat.Var.Term(mname.require[m.Term.Name])), Some(mtpe), mmaybeBody).member
          case _: l.Def => m.Defn.Def(mmods, mname.require[m.Term.Name], mtparams, mvparamss, Some(mtpe), mbody)
          case _: l.Type => m.Defn.Type(mmods, mname.require[m.Type.Name], mtparams, mtpe)
          case _: l.Clazz => m.Defn.Class(mmods, mname.require[m.Type.Name], mtparams, mctor, mtemplate)
          case _: l.Trait => m.Defn.Trait(mmods, mname.require[m.Type.Name], mtparams, mctor, mtemplate)
          case _: l.Object => m.Defn.Object(mmods, mname.require[m.Term.Name], mctor, mtemplate)
          case _: l.Package => m.Pkg(mname.require[m.Term.Name], mstats)
          case _: l.PackageObject => m.Pkg.Object(mmods, mname.require[m.Term.Name], mctor, mtemplate)
          case _: l.PrimaryCtor => m.Ctor.Primary(mmods, mname.require[m.Ctor.Name], mvparamss)
          case _: l.SecondaryCtor => m.Ctor.Secondary(mmods, mname.require[m.Ctor.Name], mvparamss, mbody)
          case _: l.TermBind => m.Pat.Var.Term(mname.require[m.Term.Name])
          case _: l.TypeBind => m.Pat.Var.Type(mname.require[m.Type.Name])
          case _: l.TermParameter => m.Term.Param(mmods, mname.require[m.Term.Param.Name], Some(mtpearg), mmaybeDefault).withAttrs(mtpearg)
          case _: l.TypeParameter => m.Type.Param(mmods, mname.require[m.Type.Param.Name], mtparams, mtpebounds, Nil, mcontextbounds)
          case _ => throw new ConvertException(lsym, s"unsupported symbol $lsym, designation = ${gsym.getClass}, flags = ${gsym.flags}")
        }
      }
      def applyPrefix(gpre: dty.Type, mmem: m.Member): m.Member = {
        if (gpre == dty.NoPrefix) mmem
        else {
          // TODO: implement me! it might not be that hard, to be honest
          // 1) Replace Type.Name(tparam) in mmem and its denotations with values obtained from gpre
          // 2) Replace Term.This(mmem.owner) in mmem and its denotations with apply(gpre)
          mmem
        }
      }
      val ssym = symbolTable.convert(lsym)
      val maybeCachedMmember = ssymToMmemberIndex.get(ssym)
      val maybePrefixedCachedMmember = maybeCachedMmember.map(applyPrefix(gpre, _))
      maybePrefixedCachedMmember.getOrElse(approximateSymbol(lsym)).forceTypechecked
    })
  }

  protected implicit class XtensionLdenotationToMmember(ldenot: l.Denotation) {
    def toMmember: m.Member = ldenot.sym.toMmember(ldenot.pre)
  }
}