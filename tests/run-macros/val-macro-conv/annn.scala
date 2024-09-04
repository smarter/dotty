package a

import scala.quoted.*

sealed trait Exp
class App[Elem <: Exp, Args <: Tuple /* of Exp*/] extends Exp
class Sel[Qual <: Exp, Name <: String] extends Exp
class Id[T] extends Exp
class Self[T] extends Exp

class ann[E <: Exp](g: Target[E]) extends annotation.StaticAnnotation with annotation.RefiningAnnotation
object ann:
  def the[T]: T = ???
end ann

class Target[E <: Exp]
object Target:
  def dummy[E <: Exp]: Target[E] = ???
  // def apply(x: T): Boolean
  // given [T]: Conversion[T => Boolean, Target[T]]
  transparent inline implicit def conv[T](inline f: T => Boolean): Target[?] = ${Macro.convImpl[T]('f)}
object Macro:
  def convImpl[T: Type](using Quotes)(f: Expr[T => Boolean]): Expr[Target[?]] =
    import quotes.reflect.*

    class ToExp extends TreeAccumulator[TypeTree]:
      def foldTree(acc: TypeTree, tree: Tree)(owner: Symbol): TypeTree = //Type[? <: Exp] =
        tree match
          case i: Ident =>
            // todo: need a typetree to workaround issue with refersToParam
            // i.tpe.asType match case '[t] => Type.of[Id[t]]
            i.tpe match
              case tp @ TermRef(_: NoPrefix, _) =>
                val tptId = TypeTree.ref(Symbol.requiredClass("a.Id"))
                Applied(tptId, List(Singleton(Ref.term(tp))))
              case _ =>
                i.tpe.asType match
                  case '[t] => TypeTree.of[Id[t]]
          case Select(qual, name) =>
            val qualTree = foldTree(acc, qual)(owner)
            val tptSel = TypeTree.ref(Symbol.requiredClass("a.Sel"))
            Applied(tptSel, List(qualTree, Singleton(Literal(StringConstant(name)))))
          case Apply(fun, args) =>
            val funTree = foldTree(acc, fun)(owner)
            val argsTrees = args.map(foldTree(acc, _)(owner))
            val tptApp = TypeTree.ref(Symbol.requiredClass("a.App"))
            Applied(tptApp, funTree :: argsTrees)
          case TypeApply(fun, args) if fun.symbol eq Symbol.requiredMethod("a.ann.the") =>
            val tptSelf = TypeTree.ref(Symbol.requiredClass("a.Self"))
            Applied(tptSelf, args)
          case _ =>
            foldOverTree(acc, tree)(owner)

    val input = Expr.betaReduce('{$f(ann.the[T])}).asTerm
    println("input: " + input.show)
    val targ = ToExp().foldTree(TypeTree.of[Exp], input)(Symbol.spliceOwner)
    val targs = List(targ)

    val dummyRef = Symbol.requiredMethod("a.Target.dummy")
    // because subst doesn't work on trees, we'll need a conversion from @ann[E](...) to @ann[E](...)
    val z1 = Ref(dummyRef).appliedToTypeTrees(targs)
    println("z1: " + z1.show)
    z1.asExprOf[Target[?]]

    // val ClsTypeTree = TypeTree.ref(Symbol.requiredClass("a.Target"))
    // val z =
    //   Apply(
    //     TypeApply(
    //       Select.unique(New(Applied(ClsTypeTree, targs)), "<init>"),
    //       targs
    //     ), List())
    // println("z: " + z.show)
    // z.asExprOf[Target[?]]
