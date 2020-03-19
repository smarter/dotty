class foo(a: Int) extends scala.annotation.Annotation

class A {
  @foo(x) def bla(x: Int): Unit = {} // error: not found: x
}
