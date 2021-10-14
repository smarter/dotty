class A {
  class B {
    type T
  }
  class C extends B

  class D extends C {
    type T = Int
  }

  def foo(x: B): x.T = ???
  def foo(x: C): x.T = ???

  val x: Int = foo(new D)
}
