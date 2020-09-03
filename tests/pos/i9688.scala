object Test {
  def foo(x: Any*): Unit = {}
  def foo(x: Any): Unit = {}

  foo("")
}
