// class Inv[T]
class A {
  // def foo[A](x: A => A)/*()*/: A = ???
  // def foo[A](x: A => A)(): A = ???

  // val y: String = foo(x => x)
  // val y: String = foo(x => x)()


  // class From
  // given Conversion[From, String => String] = ???
  // def foo(x: Int): From = ???

  // val x: String = foo(1)("")

  class B { val b: Int = 1 }
  extension (x: B)
    def foo(y: Int): B = new B
    def foo(y: String) = 1
    def bar() = 1

  val b = new B
  b.foo(1).bar()
}
