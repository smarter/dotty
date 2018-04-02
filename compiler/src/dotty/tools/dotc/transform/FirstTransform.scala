package dotty.tools.dotc
package transform

import core._
import Names._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.transform.MegaPhase._
import ast.Trees._
import ast.untpd
import Flags._
import Types._
import Constants.Constant
import Contexts.Context
import Symbols._
import SymDenotations._
import Decorators._
import dotty.tools.dotc.core.Annotations.ConcreteAnnotation
import dotty.tools.dotc.core.Denotations.SingleDenotation
import scala.collection.mutable
import DenotTransformers._
import typer.Checking
import NameOps._
import NameKinds.{AvoidClashName, OuterSelectName}
import StdNames._

object FirstTransform {
  val name = "firstTransform"
}


/** The first tree transform
 *   - eliminates some kinds of trees: Imports, NamedArgs
 *   - stubs out native methods
 *   - eliminates self tree in Template and self symbol in ClassInfo
 *   - collapses all type trees to trees of class TypeTree
 *   - converts idempotent expressions with constant types
 *   - drops branches of ifs using the rules
 *          if (true) A else B    ==> A
 *          if (false) A else B   ==> B
 */
class FirstTransform extends MiniPhase with InfoTransformer { thisPhase =>
  import ast.tpd._

  override def phaseName = FirstTransform.name

  /** eliminate self symbol in ClassInfo */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context): Type = tp match {
    case tp @ ClassInfo(_, _, _, _, self: Symbol) =>
      tp.derivedClassInfo(selfInfo = self.info)
    case _ =>
      tp
  }

  override protected def mayChange(sym: Symbol)(implicit ctx: Context): Boolean = sym.isClass

  override def checkPostCondition(tree: Tree)(implicit ctx: Context): Unit = {
    tree match {
      case Select(qual, name) if !name.is(OuterSelectName) && tree.symbol.exists =>
        assert(
          qual.tpe.derivesFrom(tree.symbol.owner) ||
            tree.symbol.is(JavaStatic) && qual.tpe.derivesFrom(tree.symbol.enclosingClass),
          i"non member selection of ${tree.symbol.showLocated} from ${qual.tpe} in $tree")
      case _: TypeTree =>
      case _: Import | _: NamedArg | _: TypTree =>
        assert(false, i"illegal tree: $tree")
      case _ =>
    }
  }

  /** elimiate self in Template */
  override def transformTemplate(impl: Template)(implicit ctx: Context): Tree = {
    cpy.Template(impl)(self = EmptyValDef)
  }

  override def transformDefDef(ddef: DefDef)(implicit ctx: Context) = {
    if (ddef.symbol.hasAnnotation(defn.NativeAnnot)) {
      ddef.symbol.resetFlag(Deferred)
      DefDef(ddef.symbol.asTerm,
        _ => ref(defn.Sys_errorR).withPos(ddef.pos)
          .appliedTo(Literal(Constant("native method stub"))))
    } else ddef
  }

  private object collectBinders extends TreeAccumulator[List[Ident]] {
    def apply(annots: List[Ident], t: Tree)(implicit ctx: Context): List[Ident] = t match {
      case t @ Bind(_, body) =>
        val annot = untpd.Ident(tpnme.BOUNDTYPE_ANNOT).withType(t.symbol.typeRef)
        apply(annot :: annots, body)
      case _ =>
        foldOver(annots, t)
    }
  }

  /** Replace type tree `t` of type `T` with `TypeTree(T)`, but record all
   *  nested Bind nodes in annotations. These are interpreted in TreeTypeMaps
   *  so that bound symbols can be properly copied.
   */
  private def toTypeTree(tree: Tree)(implicit ctx: Context) = {
    val binders = collectBinders.apply(Nil, tree)
    val result: Tree = TypeTree(tree.tpe).withPos(tree.pos)
    (result /: binders)(Annotated(_, _))
  }

  override def transformOther(tree: Tree)(implicit ctx: Context) = tree match {
    case tree: Import => EmptyTree
    case tree: NamedArg => transformAllDeep(tree.arg)
    case tree => if (tree.isType) toTypeTree(tree) else tree
  }

  override def transformIdent(tree: Ident)(implicit ctx: Context) =
    if (tree.isType) toTypeTree(tree) else constToLiteral(tree)

  override def transformSelect(tree: Select)(implicit ctx: Context) =
    if (tree.isType) toTypeTree(tree) else constToLiteral(tree)

  override def transformTypeApply(tree: TypeApply)(implicit ctx: Context) =
    constToLiteral(tree)

  override def transformApply(tree: Apply)(implicit ctx: Context) =
    constToLiteral(foldCondition(tree))

  override def transformTyped(tree: Typed)(implicit ctx: Context) =
    constToLiteral(tree)

  override def transformBlock(tree: Block)(implicit ctx: Context) =
    constToLiteral(tree)

  override def transformIf(tree: If)(implicit ctx: Context) =
    tree.cond match {
      case Literal(Constant(c: Boolean)) => if (c) tree.thenp else tree.elsep
      case _ => tree
    }

  /** Perform one of the following simplification if applicable:
   *
   *      true  && y   ==>  y
   *      false && y   ==>  false
   *      true  || y   ==>  true
   *      false || y   ==>  y
   */
  private def foldCondition(tree: Apply)(implicit ctx: Context) = tree.fun match {
    case Select(x @ Literal(Constant(c: Boolean)), op) =>
      tree.args match {
        case y :: Nil if y.tpe.widen.isRef(defn.BooleanClass) =>
          op match {
            case nme.ZAND => if (c) y else x
            case nme.ZOR  => if (c) x else y
            case _ => tree
          }
        case _ => tree
      }
    case _ => tree
  }

  // invariants: all modules have companion objects
  // all types are TypeTrees
  // all this types are explicit
}
