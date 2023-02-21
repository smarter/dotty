object Foo:
	def bar(x : Bar.YOf[scala.Any]): scala.Unit = scala.Predef.???
	bar(scala.Predef.???)

trait K:
  trait X:
    type CType <: Bar.YOf[scala.Any]
    def foo : K#X =
      val x : CType = scala.Predef.???
      x // was: error: Found: CType, Expected: K#X

object Bar:
  type YOf[T] = K#X {type M}
