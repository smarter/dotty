import scala.quoted.*

given OptionToExpr2[T: Type: ToExpr]: ToExpr[Option[T]] with {
  def apply(x: Option[T])(using Quotes): Expr[Option[T]] = x match {
    case x: Some[T] => Expr(x) // avoid ambiguity
    case None => Expr(None)
  }
}

given SomeToExpr2[T: Type: ToExpr]: ToExpr[Some[T]] = ???
