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

  // transparent inline implicit def myMacro[T, S <: T](x: T): Inv[S] = ${Macros2.myMacroImpl[T, S]('x)}
  // transparent inline implicit def myMacro[T >: Inv[S], S](x: T): S = ${Macros2.myMacroImpl[T, S]('x)}


  transparent inline def myMacro[T, S <: T](x: T): Inv[S] = ${Macros2.myMacroImpl[T, S]('x)}

object Macros2:
  def myMacroImpl[T: Type, S <: T : Type](x: Expr[T])(using Quotes): Expr[Macros.Inv[S]] =
    import quotes.reflect.*
    // we might have multiple layers of checks.
    // TODO: use a plugin to strip annotations after typer to avoid compiler crash without Matthieu's branch?
    // failsafe: give up if S isn't of the form X ::= RT[X @check] | T
    // need other conv to handle List[T] to List[X]
    // need to deal with invariance: Array[PosInt] vs Array[Int]
    println(TypeRepr.of[S])//.show)
    '{ $x.asInstanceOf[Macros.Inv[S]] }
    // '{ $x.asInstanceOf[S] }

  def valImpl[T: Type, S /*<: T*/ : Type](using Quotes): Expr[Validator[T, Macros.Inv[S]]] =
    import quotes.reflect.*
    // we might have multiple layers of checks.
    // TODO: use a plugin to strip annotations after typer to avoid compiler crash without Matthieu's branch?
    // failsafe: give up if S isn't of the form X ::= RT[X @check] | T
    // need other conv to handle List[T] to List[X]
    // need to deal with invariance: Array[PosInt] vs Array[Int]
    val tpe = TypeRepr.of[S]
    tpe match
      case tp: AndType =>
        println("failed")
        report.error("blaaaaaa")
        return '{ ??? }
      case _ =>

    println(TypeRepr.of[S].show)
    '{ Validator.identitySingleton.asInstanceOf[Validator[T, Macros.Inv[S]]] }
    // '{ $x.asInstanceOf[S] }

end Macros2

trait Validator[From, To/* <: From*/]:
  extension (f: From) def validate(): To

object Validator:
  import Macros.Inv
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
  def identity[T]: Validator[T, T] = identitySingleton.asInstanceOf[Validator[T, T]]
  val identitySingleton: Validator[Any, Any] = new:
    extension (f: Any) def validate(): Any = f

  transparent inline given [T, S/* <: T*/]: Validator[T, Inv[S]] = ${Macros2.valImpl[T, S]}

  import scala.reflect.{classTag, ClassTag}
  given [T: ClassTag, S <: T](using Validator[T, S]): Validator[Array[T], Array[S]] with
    extension (f: Array[T])
      def validate(): Array[S] = f.map(_.validate())(using classTag[T].asInstanceOf[ClassTag[S]])

object ValidatorConv:
  // Not S <: T because Array[PosInt] !:<:< Array[Int]
  // alt: keep the subtype but use Array[S] & Array[T] ?
  given [T, S /*<: T*/](using Validator[T, S]): Conversion[T, S] with
    def apply(t: T): S = t.validate()
