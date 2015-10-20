package scala.meta.internal.hosts.dotty
package reflect

import scala.tools.nsc.Global
import dotty.tools.dotc.core.Contexts.{Context => DottyContext}

import dotty.tools.dotc.ast.{Trees => dtr}
import dotty.tools.dotc.core.{Types => dty}

trait ReflectToolkit[A >: dtr.Untyped <: dty.Type]
                    extends dtr.Instance[A]
                       with TreeHelpers[A]
                       with TypeHelpers[A]
                       with SymbolHelpers[A]
                       with Platform[A]
                       with LogicalSymbols[A]
                       with LogicalTrees[A] {
  val global: Global
  val g: dtr.Instance[A]
  implicit val ctx: DottyContext
  object l extends LogicalSymbols with LogicalTrees
}