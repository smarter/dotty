package scala.meta
package internal.hosts.dotty
package converters

import scala.tools.nsc.{Global => ScalaGlobal}
import scala.meta.semantic.{Context => ScalametaSemanticContext}

import dotty.tools.dotc.core.Contexts.{Context => DottyContext}

import dotty.tools.dotc.core.Types.Type
import dotty.tools.dotc.ast.Trees._

abstract class Api[A >: Untyped <: Type](global: ScalaGlobal)(implicit ctx: DottyContext)
   extends Instance[A]
   with ToM[A]
   with ToMannot[A]
   with ToMattrs[A]
   with ToMlit[A]
   with ToMmember[A]
   with ToMname[A]
   with ToMtree[A]
   with ToMtype[A]
   with ToGprefix[A]
   with ToGsymbol[A]
   with ToGtree[A]
   with ToGtype[A]
   with SymbolTables[A]
   with Caches[A] {
  implicit val c: ScalametaSemanticContext
}