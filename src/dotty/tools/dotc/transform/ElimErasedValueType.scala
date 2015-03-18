package dotty.tools.dotc
package transform

import ast.{Trees, tpd}
import core._
import TreeTransforms._, Phases.Phase
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import TypeErasure.ErasedValueType, ValueClasses._

class ElimErasedValueType extends MiniPhaseTransform with DenotTransformer {

  import tpd._

  override def phaseName: String = "elimErasedValueType"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[Erasure])

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = {
    val info = ref match {
      case ref: ClassDenotation if ref is ModuleClass =>
        ref.linkedClass match {
          case origClass: ClassSymbol if isDerivedValueClass(origClass) =>
            val cinfo = ref.classInfo
            val decls1 = cinfo.decls.cloneScope
            ctx.atPhase(this.next) { implicit ctx =>
              // Remove synthetic cast methods introduced by ExtensionMethods,
              // they are no longer needed after this phase.
              decls1.unlink(cinfo.decl(nme.UNDERLYING2EVT).symbol)
              decls1.unlink(cinfo.decl(nme.EVT2UNDERLYING).symbol)
            }
            cinfo.derivedClassInfo(decls = decls1)
          case _ =>
            ref.info
        }
      case _ =>
        ref.info
    }
    val info1 = elimErasedValueType(info)
    if (info1 eq ref.info) ref
    else ref match {
      case ref: SymDenotation => ref.copySymDenotation(info = info1)
      case _ => ref.derivedSingleDenotation(ref.symbol, info1)
    }
  }

  // FIXME: Is using a TypeMap here the best solution?
  def elimErasedValueType(tpe: Type)(implicit ctx: Context) = new TypeMap() {
    def apply(tp: Type) = tp match {
      case ErasedValueType(_, underlying) =>
        apply(underlying)
      case _ =>
        mapOver(tp)
    }
  }.apply(tpe)

  def transformTypeOfTree(tree: Tree)(implicit ctx: Context): Tree =
    tree.withType(elimErasedValueType(tree.tpe))

  override def transformApply(tree: Apply)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val Apply(fun, args) = tree
    val name = fun.symbol.name

    // The casts to and from ErasedValueType are no longer needed once ErasedValueType
    // has been eliminated.
    val t =
      if ((name eq nme.UNDERLYING2EVT) || (name eq nme.EVT2UNDERLYING))
        args.head
      else
        tree
    transformTypeOfTree(t)
  }

  // FIXME: This is probably missing some nodes, all the current ones were added to make tests pass
  override def transformIdent(tree: Ident)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformSelect(tree: Select)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformBlock(tree: Block)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformIf(tree: If)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformSeqLiteral(tree: SeqLiteral)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
  override def transformTypeTree(tree: TypeTree)(implicit ctx: Context, info: TransformerInfo): Tree =
    transformTypeOfTree(tree)
}
