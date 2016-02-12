package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._, core.Decorators._
import TreeTransforms._, Phases.Phase
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._

class Tracer extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "tracer"

  /** Trace method calls.
   *
   *  Replace:
   *    fun(args) // Apply(fun, args)
   *  By:
   *  {
   *     Console.print("## Entering fun(args)\n")
   *     fun(args)
   *  }
   */
  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {

    val message = s"## Entering ${tree.show}\n"

    // Console
    val consoleType = ctx.requiredModule("scala.Console")
    val consoleRef = ref(consoleType)

    // Console.print
    val print = consoleRef.select("print".toTermName)

    // Console.print(...)
    val printMessage = print.appliedTo(Literal(Constant(message)))

    // Block(stats, expr) means { stats; expr }
    Block(List(printMessage), tree)
  }

}
