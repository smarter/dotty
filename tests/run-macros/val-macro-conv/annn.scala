package a

import scala.quoted.*

class ann[T, Env](g: Target[T, Env]) extends annotation.StaticAnnotation with annotation.RefiningAnnotation

trait Target[T, Env]
class StringTarget[T, Env](x: String) extends Target[T, Env]
object Target:
  // def apply(x: T): Boolean
  // given [T]: Conversion[T => Boolean, Target[T]]
  transparent inline implicit def conv[T](inline f: T => Boolean): Target[T, ?] = ${Macro.convImpl[T]('f)}
object Macro:
  def convImpl[T: Type](using Quotes)(f: Expr[T => Boolean]): Expr[Target[T, ?]] =
    import quotes.reflect.*
    // At this point in Typer we see TermRef, but they get substituted by TermParamRef for the final MethodType
    val buf: collection.mutable.ListBuffer[TermRef] = collection.mutable.ListBuffer.empty
    class MyTraverser extends TreeTraverser:
       override def traverseTree(tree: Tree)(owner: Symbol): Unit =
         tree match
           case tree: Term =>
             tree.tpe match
               case tp @ TermRef(_: NoPrefix, name) =>
                 println("owner: " + Symbol.spliceOwner.owner.owner + " ")
                 buf += tp
               case tp =>
                 // println("other: " + tp)
           case _ =>
             // println("hi: " + tree)
         traverseTreeChildren(tree)(owner)

    // println("#####")           
    (new MyTraverser).traverseTree(f.asTerm)(Symbol.spliceOwner)
    buf.head.asType match
      case '[env] =>
        val lit = Literal(StringConstant(f.show))
        // val z = '{ new StringTarget[T, env](${lit.asExprOf[String]}) }
        val ClsTypeTree =
          TypeTree.ref(Symbol.requiredClass("a.StringTarget"))
        val z =
          Apply(Select.unique(New(Applied(ClsTypeTree, List(TypeTree.of[T], Singleton(Ref.term(buf.head))))), "<init>"), List(lit))

        println("z: " + z)
        z.asExprOf[Target[T, ?]]
