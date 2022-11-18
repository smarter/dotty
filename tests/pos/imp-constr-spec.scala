class Foo[A]

object Test:
  implicit def fooInt: Foo[Int] = ???
  implicit def fooAny: Foo[Any] = ???
  def lookup[T](x: T)(using foo: Foo[T]): Unit = {}
  lookup(1)
