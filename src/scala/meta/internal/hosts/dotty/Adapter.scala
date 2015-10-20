package scala.meta
package internal.hosts.dotty

import scala.reflect.{classTag, ClassTag}
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.meta.internal.hosts.dotty.contexts.{Compiler => Compiler}
import scala.meta.internal.hosts.dotty.contexts.{Adapter => AdapterImpl}
import scala.meta.{Context => ContextApi}
import scala.tools.nsc.Global

import dotty.tools.dotc.{util => dut}
import dotty.tools.dotc.{core => dco}
import dotty.tools.dotc.core.{Symbols => dsy}
import dotty.tools.dotc.core.{TypeErasure => dte}
import dotty.tools.dotc.core.Contexts.{Context => DottyContext}
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.core.{Names => dna}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.StdNames.{nme, tpnme}

object Adapter {
  def apply[G <: Global, T >: dtr.Untyped <: dty.Type](global: G)(implicit ctx: DottyContext): Adapter[G, T] = {
    new AdapterImpl[G, T](global) {
      override def toString = s"Adapter($global)"
    }
  }
}

// TODO: We can probably expand this to interface with any Universe
// not just with scala.tools.nsc.Global. How necessary is this, though?
trait Adapter[G <: Global, T >: dtr.Untyped <: dty.Type] extends ContextApi {
  self: AdapterImpl[G, T] =>

  val g: dtr.Instance[T] = this

  object conversions {
    implicit class ScalahostAdapterReflectTree(gtree: g.Tree) {
      // TODO: make this work
      // def toMeta[T <: m.Tree : ClassTag](implicit ev: T OrElse m.Tree): ev.T = {
      //   implicit val tag: ClassTag[ev.T] = ev.tag
      //   self.toMtree[ev.T](gtree)
      // }
      // NOTE: can't forceTypechecked here, because gtree may be unattributed
      // and then mtree will end up unattributed as well
      def toMeta: m.Tree = self.toMtree[m.Tree](gtree)
    }
    implicit class ScalahostAdapterReflectType(gtype: dty.Type) {
      // TODO: make this work
      // def toMeta[T <: m.Type.Arg : ClassTag](implicit ev: T OrElse m.Type.Arg): ev.T = {
      //   implicit val tag: ClassTag[ev.T] = ev.tag
      //   if (classOf[m.Type].isAssignableFrom(tag.runtimeClass)) self.toMtype(gtype).require[ev.T]
      //   else self.toMtypeArg(gtype).require[ev.T]
      // }
      def toMeta: m.Type.Arg = self.toMtypeArg(gtype).forceTypechecked
    }
    implicit class ScalahostAdapterReflectSymbol(gsym: dsy.Symbol) {
      // TODO: make this work
      // def toMeta[T <: m.Member](gpre: dty.Type)(implicit ev: T OrElse m.Member): ev.T = {
      //   implicit val tag: ClassTag[ev.T] = ev.tag
      //   self.toMmember(gsym, gpre).require[ev.T]
      // }
      def toMeta(gpre: dty.Type): m.Member = self.toMmember(gsym, gpre).forceTypechecked
    }
    // implicit class ScalahostAdapterReflectAnnotation(gannot: g.AnnotationInfo) {
    //   def toMeta: m.Mod.Annot = ???
    // }
    implicit class ScalahostAdapterMetaTree(mtree: m.Tree) {
      def toReflect: g.Tree = self.toGtree(mtree).asInstanceOf[g.Tree]
    }
    implicit class ScalahostAdapterMetaType(mtpe: m.Type.Arg) {
      def toReflect: dty.Type = self.toGtype(mtpe).asInstanceOf[dty.Type]
    }
    implicit class ScalahostAdapterMetaName(mname: m.Name) {
      def toReflect: Seq[dsy.Symbol] = self.toGsymbols(mname).asInstanceOf[Seq[dsy.Symbol]]
    }
    implicit class ScalahostAdapterMetaMember(mmember: m.Member) {
      def toReflect: Seq[dsy.Symbol] = self.toGsymbols(mmember).asInstanceOf[Seq[dsy.Symbol]]
    }
  }

  private[meta] def toMtree[T <: m.Tree : ClassTag](gtree: g.Tree): T
  private[meta] def toMtype(gtpe: dty.Type): m.Type
  private[meta] def toMtypeArg(gtpe: dty.Type): m.Type.Arg
  private[meta] def toMmember(gsym: dsy.Symbol, gpre: dty.Type): m.Member
  // private[meta] def toMannot(gannot: g.AnnotationInfo): m.Mod.Annot
  private[meta] def toGtree(mtree: m.Tree): g.Tree
  private[meta] def toGtype(mtpe: m.Type.Arg): dty.Type
  private[meta] def toGsymbols(mname: m.Name): Seq[dsy.Symbol]
  private[meta] def toGsymbols(mmember: m.Member): Seq[dsy.Symbol]
}
