class Inv[T]
class A {
  // def foo[A](x: A => A)/*()*/: A = ???
  def foo[A](x: A => A)(): A = ???

  // val y: String = foo(x => x)
  val y: String = foo(x => x)()
}
