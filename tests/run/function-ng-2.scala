object Test {
  def foo2[A, B](f: PolyFunction { def apply(x: A): B }): Unit = {
    val a: A = ???
    val b: B = f(a)
  }
  foo2(new PolyFunction {
    def apply(x: Int): List[x.type] = List(x)
  })
}
