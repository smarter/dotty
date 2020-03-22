class foo(a: Int) extends scala.annotation.Annotation


class A {
  @foo(x) def bla(x: Int): Unit = {} // error: not found: x
}

@foo(x) class B(x: Int) // error: not found: x



class C {
  val x: Int = 1
  @foo(x) def bla(z: Int): Unit = {} // OK, but why?
}
