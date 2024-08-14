package a

import scala.annotation.*
import scala.language.implicitConversions
import scala.quoted.*

class checkBase extends StaticAnnotation with RefiningAnnotation
class check[T](f: T => Boolean) extends checkBase

trait Force[+T]
object Force:
  given [S]: Force[S] = ???

object Macros:
  // transparent inline def myMacro[T, S <: T @checkBase](x: T): S = ${myMacroImpl[T, S]('x)}
  type Inv[S] <: S
  // transparent inline def myMacro[T, S <: T](x: T)(using Force[T], Force[S]): S = ${myMacroImpl[T, S]('x)}

  transparent inline implicit def myMacro[T, S <: T](x: T): Inv[S] = ${Macros2.myMacroImpl[T, S]('x)}
  // transparent inline implicit def myMacro[T >: Inv[S], S](x: T): S = ${Macros2.myMacroImpl[T, S]('x)}


  // def myMacroImpl[T: Type, S <: T @checkBase : Type](x: Expr[T])(using Quotes): Expr[S] =
object Macros2:
  def myMacroImpl[T: Type, S <: T : Type](x: Expr[T])(using Quotes): Expr[Macros.Inv[S]] =
    import quotes.reflect.*
    // we might have multiple layers of checks.
    // TODO: use a plugin to strip annotations after typer to avoid compiler crash without Matthieu's branch?
    // failsafe: give up if S isn't of the form X ::= RT[X @check] | T
    // need other conv to handle List[T] to List[X]
    // need to deal with invariance: Array[PosInt] vs Array[Int]
    println(TypeRepr.of[S].show)
    '{ $x.asInstanceOf[Macros.Inv[S]] }
    // '{ $x.asInstanceOf[S] }


trait Validator[From, To <: From]:
  extension (f: From) def validate(): To

object Validator:
  // given [T: Validator]: Validator[Array[T]]
  // transparent inline given [T: Validator]: Validator[RT[Array[T]]] = ???


  /**
   *  Equivalent to:
   *
   *      given identity[T]: Validator[T, T] with
   *        extension (f: T) def validate(): T = f
   *
   *  but avoids unnecessary allocations.
   */
  given identity[T]: Validator[T, T] = identitySingleton.asInstanceOf[Validator[T, T]]
  private val identitySingleton: Validator[Any, Any] = new:
    extension (f: Any) def validate(): Any = f

  transparent inline given [T, S <: T]: Validator[T, Inv[S]] = ${Macros2.myMacroImpl[T, S]}

  given [T, S](using Validator[T, S]): Validator[Array[T], Array[S]] with
    extension (f: Array[T])
      def validate(): Array[S] = f.map(_.validate())

object ValidatorConv:
  given [T, S <: T](using Validator[T, S]): Conversion[T, S] with
    def apply(t: T): S = t.validate()
