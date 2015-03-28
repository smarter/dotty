package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import Contexts._, DenotTransformers._, TreeTransforms._, Phases.Phase
import TreeExtractors._

/** This phase performs safe peephole optimizations. */
class PeepholeOptimize extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "peepholeoptimize"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[ElimErasedValueType])

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree =
    tree match {
      // (new V(e)).u => e
      case ValueClassUnbox(NewWithArgs(_, List(e))) =>
        e
      case _ =>
        tree
    }
}
