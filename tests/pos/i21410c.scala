object Test:
  def foo[T](x: Option[T]): T = ???
  transparent inline def foo[T <: Tuple](x: T): Any = ??? : String

  val tup: (Int, String) = (1, "")

  val x = foo(tup)
  val y: String = x

  val x2: String = foo(tup) // error
