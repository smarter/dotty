object O {
  class A
  class B

  def f[T](x: T, y: T): T = y
  val x: A = f(new A { }, new B { })
}
