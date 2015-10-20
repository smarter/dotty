package scala.meta.internal.hosts.dotty
package reflect

import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import org.scalameta.invariants._
import org.scalameta.unreachable

import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

import dotty.tools.dotc.core.Flags._

import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.transform.SymUtils._

import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.core.{Types => dty}

trait LogicalSymbols[A >: dtr.Untyped <: dty.Type] {
  self: ReflectToolkit[A] =>

  implicit class RichLogicalSymbol(gsym: dsy.Symbol) {
    def toLogical: l.Symbol = {
      val results = logicalSymbols(List(gsym))
      require(results.length == 1 && debug(gsym, results))
      results.head
    }
  }

  implicit class RichLogicalScope(gscope: dco.Scopes.Scope) {
    def toLogical: Seq[l.Symbol] =
      gscope.toList.toLogical
  }

  implicit class RichLogicalSymbols(gsyms: Seq[dsy.Symbol]) {
    def toLogical: Seq[l.Symbol] = logicalSymbols(gsyms)
  }

  implicit class RichLogicalSymbolss(gsymss: Seq[Seq[dsy.Symbol]]) {
    def toLogical: Seq[Seq[l.Symbol]] = gsymss.map(_.toLogical)
  }

  sealed trait LogicalSymbol extends Product {
    def name: String
    def symbol: dsy.Symbol
    def symbols: Seq[dsy.Symbol] = this.productIterator.collect({case sym: dsy.Symbol => sym}).filter(_ != dsy.NoSymbol).toList
    def gsymbol: dsy.Symbol = symbol
    def gsymbols: Seq[dsy.Symbol] = symbols
    def isComplete: Boolean = !isIncomplete
    def isIncomplete: Boolean = symbol == dsy.NoSymbol
  }

  trait LogicalSymbols { l: self.l.type =>
    type Symbol = LogicalSymbol

    // not representable with syntax
    // is here simply to provide an encoding for compiler's NoSymbol
    case object Zero extends LogicalSymbol {
      def name = nme.NO_NAME.toString
      def symbol = dsy.NoSymbol
      override def isComplete = true
      override def isIncomplete = false
    }

    // can be a synthetic symbol called "this" if it was specified by the user (the name gets lost during typechecking)
    // can be NoSymbol if it wasn't explicitly specified by the user (i.e. when we have g.noSelfType)
    case class Self(owner: dsy.Symbol) extends LogicalSymbol {
      def name = "this"
      def symbol = throw new UnsupportedOperationException("l.Self symbols don't have adequate equivalents in scala.reflect")
      override def isComplete = true
      override def isIncomplete = false
    }

    // > val x: Int
    // value x, 'x', class MethodSymbol, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR)
    // > type T = X { val x: Int }
    // value x, 'x', class MethodSymbol, flags = 138412112 (DEFERRED | METHOD | STABLE | ACCESSOR)
    // > type T = X forSome { val x: Int }
    // type x.type, 'x.type', class AbstractTypeSymbol, flags = 34359738384 (DEFERRED | EXISTENTIAL)
    case class AbstractVal(getter: dsy.Symbol) extends LogicalSymbol {
      def name = getter.name.toString
      def symbol = getter
    }

    // > var x: Int
    // method x, 'x', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // method x_=, 'x_$eq', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // > type T = X { var x: Int }
    // method x, 'x', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    // method x_=, 'x_$eq', class MethodSymbol, flags = 134217808 (DEFERRED | METHOD | ACCESSOR)
    case class AbstractVar(getter: dsy.Symbol, setter: dsy.Symbol) extends LogicalSymbol {
      def name = getter.orElse(setter).name.asTermName.getterName.toString
      def symbol = getter
    }

    // > def x: Int
    // method x, 'x', class MethodSymbol, flags = 80 (DEFERRED | METHOD)
    // > type T = X { def x: Int }
    // method x, 'x', class MethodSymbol, flags = 80 (DEFERRED | METHOD)
    case class AbstractDef(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > type T
    // type T, 'T', class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // > type T = X { type T <: Int }
    // type T, 'T', class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // > type T = X forSome { type T <: Int }
    // type T, 'T', class AbstractTypeSymbol, flags = 34359738384 (DEFERRED | EXISTENTIAL)
    case class AbstractType(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > val x = 2
    // value x, 'x', class MethodSymbol, flags = 138412096 (METHOD | STABLE | ACCESSOR) + maybe PARAMACCESSOR
    // value x, 'x ', class TermSymbol, flags = 17592186568708 (PRIVATE | LOCAL | TRIEDCOOKING) + maybe PARAMACCESSOR
    // > private[this] val x = 2
    // value x, 'x', class TermSymbol, flags = 524292 (PRIVATE | LOCAL) + maybe PARAMACCESSOR
    // > locally { val x = 2 }
    // value x, 'x', class TermSymbol, flags = 0
    case class Val(field: dsy.Symbol, getter: dsy.Symbol) extends LogicalSymbol {
      def name = field.orElse(getter).name.asTermName.getterName.toString
      def symbol = getter.orElse(field)
      override def isComplete = symbol != dsy.NoSymbol
    }

    // > var x = 2
    // method x, 'x', class MethodSymbol, flags = 134217792 (METHOD | ACCESSOR) + maybe PARAMACCESSOR + maybe DEFAULTINIT
    // method x_=, 'x_$eq', class MethodSymbol, flags = 134217792 (METHOD | ACCESSOR) + maybe PARAMACCESSOR + maybe DEFAULTINIT
    // variable x, 'x ', class TermSymbol, flags = 17592186572804 (PRIVATE | MUTABLE | LOCAL | TRIEDCOOKING) + maybe PARAMACCESSOR
    // > private[this] var x = 2
    // variable x, 'x', class TermSymbol, flags = 528388 (PRIVATE | MUTABLE | LOCAL) + maybe PARAMACCESSOR + maybe DEFAULTINIT
    // > locally { var x = 2 }
    // variable x, 'x', class TermSymbol, flags = 4096 (MUTABLE)
    case class Var(field: dsy.Symbol, getter: dsy.Symbol, setter: dsy.Symbol) extends LogicalSymbol {
      def name = field.orElse(getter).orElse(setter).name.asTermName.getterName.toString
      def symbol = getter.orElse(field)
      override def isComplete = symbol != dsy.NoSymbol
    }

    // > def x = 2
    // method x, 'x', class MethodSymbol, flags = 64 (METHOD)
    case class Def(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > def x: Int = macro ???
    // macro method x, 'x', class MethodSymbol, flags = 32832 (METHOD | MACRO)
    case class Macro(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > type T = Int
    // type T, 'T', class AliasTypeSymbol, flags = 0 ()
    // > type T = X { type T = Int }
    // type T, 'T', class AliasTypeSymbol, flags = 0 ()
    case class Type(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > class C
    // class C, 'C', class ClassSymbol, flags = 0 ()
    // NOTE: [warn] LogicalSymbols.scala:108:
    // Class org.scalameta.reflection.LogicalSymbols$LogicalSymbol$Class
    // differs only in case from org.scalameta.reflection.LogicalSymbols$LogicalSymbol$class.
    // Such classes will overwrite one another on case-insensitive filesystems.
    case class Clazz(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > trait T
    // trait T, 'T', class ClassSymbol, flags = 33554568 (ABSTRACT | INTERFACE | TRAIT)
    // > trait T { def x = 2 }
    // trait T, 'T', class ClassSymbol, flags = 33554440 (ABSTRACT | TRAIT)
    case class Trait(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > object M
    // object M, 'M', class ModuleSymbol, flags = 256 (MODULE)
    case class Object(module: dsy.Symbol, moduleClass: dsy.Symbol) extends LogicalSymbol {
      def name = module.name.toString
      def symbol = module
    }

    // > package scala
    // package scala, 'scala', class ModuleSymbol, flags = 17592187109664 (FINAL | MODULE | PACKAGE | JAVA | TRIEDCOOKING)
    case class Package(module: dsy.Symbol, moduleClass: dsy.Symbol) extends LogicalSymbol {
      def name = module.name.toString
      def symbol = module
    }

    // > package object scala
    // package object scala, 'package', class ModuleSymbol, flags = 256 (MODULE)
    case class PackageObject(module: dsy.Symbol, moduleClass: dsy.Symbol) extends LogicalSymbol {
      def name = module.name.toString
      def symbol = module
    }

    // > class C(x: Int)
    // constructor C, '<init>', class MethodSymbol, flags = 64 (METHOD)
    case class PrimaryCtor(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > def this() = this(2)
    // constructor C, '<init>', class MethodSymbol, flags = 64 (METHOD)
    case class SecondaryCtor(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > Nil match { case x => ??? }
    // value x, 'x', class TermSymbol, flags = 0
    // NOTE: this symbol can't be distinguished from a local val
    // so I've patched the analyzer to attach metadata that would allow that
    case class TermBind(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > Nil match { case _: List[t] => ??? }
    // type t, 't', class AbstractTypeSymbol, flags = 16 (DEFERRED)
    // NOTE: this symbol can't be distinguished from a local type
    // so I've patched the analyzer to attach metadata that would allow that
    case class TypeBind(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > class C(x: Int)
    // value x, 'x', class TermSymbol, flags = 537395204 (PRIVATE | LOCAL | PARAMACCESSOR)
    // constructor C, class MethodSymbol, flags = 64 (METHOD)
    // value x, 'x', class TermSymbol, flags = 8192 (PARAM)
    // > def foo(x: Int) = ???
    // value x, 'x', class TermSymbol, flags = 8192 (PARAM)
    case class TermParameter(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }

    // > class C[T]
    // type T, 'T', class AbstractTypeSymbol, flags = 8208 (DEFERRED | PARAM)
    // > def foo[T] = ???
    // type T, 'T', class TypeSkolem, flags = 8208 (DEFERRED | PARAM)
    case class TypeParameter(symbol: dsy.Symbol) extends LogicalSymbol {
      def name = symbol.name.toString
    }
  }

  private def allowSymbol(gsym: dsy.Symbol): Boolean = {
    // TODO
    true
  }

  private def logicalSymbols(gsyms: Seq[dsy.Symbol]): Seq[l.Symbol] = {
    val rawResult = mutable.ListBuffer[l.Symbol]()
    val iterator = gsyms.iterator
    while (iterator.hasNext) {
      val gsym = {
        var result = iterator.next()
        if (result.is(ModuleClass)) result = result.companionModule
        result
      }
      if (allowSymbol(gsym)) {
        val lsym = {
          if (gsym == dsy.NoSymbol) {
            l.Zero
          } else if (gsym.isTerm && !gsym.is(Method) && !gsym.is(Module)) {
            require(!gsym.owner.isRefinementClass)
            if (gsym.is(TermParam)) l.TermParameter(gsym)
            else {
              if (gsym.is(Mutable)) l.Var(gsym, gsym.getter, gsym.setter)
              else {
                // if (gsym.metadata.get("isPatternVariable").map(_.require[Boolean]).getOrElse(false)) l.TermBind(gsym) 
                // else
                l.Val(gsym, gsym.getter)
              }
            }
          } else if (gsym.is(Method)) {
            require(gsym.is(Method))
            if (gsym.is(Accessor)) {
              if (gsym.is(Stable)) {
                if (gsym.is(Deferred) || gsym.owner.isRefinementClass) l.AbstractVal(gsym)
                else l.Val(gsym.accessedFieldOrGetter, gsym)
              } else {
                if (!gsym.isSetter) {
                  if (gsym.is(Deferred) || gsym.owner.isRefinementClass) l.AbstractVar(gsym, gsym.setter)
                  else l.Var(gsym.accessedFieldOrGetter, gsym, gsym.setter)
                } else {
                  if (gsym.is(Deferred) || gsym.owner.isRefinementClass) l.AbstractVar(gsym.getter, gsym)
                  else l.Var(gsym.accessedFieldOrGetter, gsym.getter, gsym)
                }
              }
            } else {
              if (gsym.is(Macro)) l.Macro(gsym)
              else if (gsym.isPrimaryConstructor) l.PrimaryCtor(gsym)
              else if (gsym.isConstructor) l.SecondaryCtor(gsym)
              else if (gsym.is(Deferred) || gsym.owner.isRefinementClass) l.AbstractDef(gsym)
              else l.Def(gsym)
            }
          } else if (gsym.is(Module)) {
            if (gsym.is(Package)) {
              l.Package(gsym, gsym.moduleClass)
            } else {
              if (gsym.isPackageObject) l.PackageObject(gsym, gsym.moduleClass)
              else l.Object(gsym, gsym.moduleClass)
            }
          } else if (gsym.isType && !gsym.isClass) {
            if (gsym.is(TypeParam)) {
              l.TypeParameter(gsym)
            } else if (gsym.is(Deferred)) {
              //if (gsym.metadata.get("isPatternVariable").map(_.require[Boolean]).getOrElse(false)) l.TypeBind(gsym)
              //else
              l.AbstractType(gsym)
            } else {
              l.Type(gsym)
            }
          } else if (gsym.isClass) {
            if (gsym.is(Trait)) l.Trait(gsym)
            else l.Clazz(gsym)
          } else {
            sys.error(s"unsupported symbol ${gsym}, designation = ${gsym.getClass}, info = ${gsym.info.show}")
          }
        }
        rawResult += lsym
      }
    }
    val result = rawResult.toList.distinct
    if (result.exists(_.isIncomplete)) {
      // this situation can probably occur when we are converting a scope with a getter and a setter
      // and their owners are dummy symbols that don't track their children
      val completeResult = mutable.ListBuffer[l.Symbol]()
      var i = 0
      while (i < result.length) {
        def merge(lsym1: LogicalSymbol, lsym2: LogicalSymbol) = (lsym1, lsym2) match {
          case (l.AbstractVar(g1, s1), l.AbstractVar(g2, s2)) => l.AbstractVar(g1.orElse(g2), s1.orElse(s2))
          case (l.Val(f1, g1), l.Val(f2, g2)) => l.Val(f1.orElse(f2), g1.orElse(g2))
          case (l.Var(f1, g1, s1), l.Var(f2, g2, s2)) => l.Var(f1.orElse(f2), g1.orElse(g2), s1.orElse(s2))
          case _ => unreachable(debug(lsym1, lsym2))
        }
        val partial = result(i)
        val found = completeResult.indexWhere(accum => accum.productPrefix == partial.productPrefix && accum.name == partial.name)
        if (found != -1) completeResult(found) = merge(completeResult(found), partial)
        else completeResult += result(i)
        i += 1
      }
      require(completeResult.forall(_.isComplete))
      completeResult.toList
    } else {
      result
    }
  }

  implicit class RichHelperLogicalSymbol(lsym: l.Symbol) {
    def supermembers: List[l.Symbol] = {
      def overridees = {
        lsym.symbol.allOverriddenSymbols.take(1).map(_.toLogical).toList
      }
      def superclasses = {
        val relevantInfo = lsym.symbol.info.typeSymbol.info
        val parentTypes = relevantInfo.require[dty.ClassInfo].realParents
        parentTypes.map(_.typeSymbol.toLogical)
      }
      lsym match {
        case l.AbstractVal(gget) => overridees
        case l.AbstractVar(gget, gset) => overridees
        case l.AbstractDef(gsym) => overridees
        case l.AbstractType(gsym) => overridees
        case l.Val(gfield, gget) => overridees
        case l.Var(gfield, gget, gset) => overridees
        case l.Def(gsym) => overridees
        case l.Macro(gsym) => overridees
        case l.Type(gsym) => overridees
        case l.Clazz(gsym) => superclasses
        case l.Trait(gsym) => superclasses
        case l.Object(gmodule, gmoduleclass) => superclasses
        case l.Package(gmodule, gmoduleclass) => Nil
        case l.PackageObject(gmodule, gmoduleclass) => superclasses
        case l.PrimaryCtor(gsym) => Nil
        case l.SecondaryCtor(gsym) => Nil
        case l.TermBind(gsym) => Nil
        case l.TypeBind(gsym) => Nil
        case l.TermParameter(gsym) => Nil
        case l.TypeParameter(gsym) => Nil
        case _ => unreachable(debug(lsym))
      }
    }

    def submembers: List[l.Symbol] = {
      def overriders = {
        // TODO: look up children of the owner and then search in their decls
        ???
      }
      def subclasses = {
        // TODO
        val unordered = Set.empty[dsy.Symbol]
        unordered.toList.sortBy(_.name.decode).map(_.toLogical)
      }
      lsym match {
        case l.AbstractVal(gget) => overriders
        case l.AbstractVar(gget, gset) => overriders
        case l.AbstractDef(gsym) => overriders
        case l.AbstractType(gsym) => overriders
        case l.Val(gfield, gget) => overriders
        case l.Var(gfield, gget, gset) => overriders
        case l.Def(gsym) => overriders
        case l.Macro(gsym) => overriders
        case l.Type(gsym) => overriders
        case l.Clazz(gsym) => subclasses
        case l.Trait(gsym) => subclasses
        case l.Object(gmodule, gmoduleclass) => Nil
        case l.Package(gmodule, gmoduleclass) => Nil
        case l.PackageObject(gmodule, gmoduleclass) => Nil
        case l.PrimaryCtor(gsym) => Nil
        case l.SecondaryCtor(gsym) => Nil
        case l.TermBind(gsym) => Nil
        case l.TypeBind(gsym) => Nil
        case l.TermParameter(gsym) => Nil
        case l.TypeParameter(gsym) => Nil
        case _ => unreachable(debug(lsym))
      }
    }
  }
}