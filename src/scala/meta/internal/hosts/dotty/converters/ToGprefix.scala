package scala.meta
package internal.hosts.dotty
package converters

import org.scalameta.invariants._
import org.scalameta.unreachable
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => mapi}
import scala.meta.internal.{ast => m}
import scala.meta.internal.{semantic => s}
import scala.meta.internal.hosts.dotty.reflect._

import dotty.tools.dotc.core.{Types => dty}
import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.core.Contexts.Context

trait ToGprefix[A >: dtr.Untyped <: dty.Type] extends ReflectToolkit[A] with MetaToolkit {
  self: Api[A] =>

  protected implicit class XtensionNamePrefix(mname: m.Name) {
    def toGprefix = mname.denot.prefix match {
      case s.Prefix.Zero => dty.NoPrefix
      case s.Prefix.Type(mtpe) => mtpe.require[m.Type].toGtype
    }
  }

  protected implicit class XtensionMemberPrefix(mmember: m.Member) {
    def toGprefix = mmember.name.require[m.Name].toGprefix
  }
}