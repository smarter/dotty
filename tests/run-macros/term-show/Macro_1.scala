import scala.quoted.*

object TypeToolbox {
  inline def show(inline className: String): String = ${ showImpl('className) }
  private def showImpl(className: Expr[String])(using Quotes) : Expr[String] =
    import quotes.reflect.*
    val Expr(name) = className: @unchecked
    val res = Symbol.requiredClass(name).tree.show
    println(res)
    Expr(res)
}
