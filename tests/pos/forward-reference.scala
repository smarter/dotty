object Test {
  def hello[T]: T = ???
  val x = hello[Outer.Inner]
  object Outer {
    class Inner // by the time we pickle `x`, the symbol for `Inner` has not yet been
                // registered or even pre-registered
  }
}
