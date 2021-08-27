sealed trait Assertion[Cond <: Boolean]

object Assertion {
  given Assertion[true] with {
  }
}

trait Tensor[S <: Tuple] {
  // === isn't defined
  def +[S2 <: Tuple](other: Tensor[S2])(using Assertion[S === S2]): Tensor[S] = new Tensor {} // error
}

object Tensor {
  def mk[S <: Tuple]: Tensor[S] = new Tensor {}
}

object Foo {
  val t1: Tensor[("batch", "len", "embed")] = Tensor.mk
  val t2: Tensor[("embed", "hid")] = Tensor.mk
  def foo(x: Any) = x
  // ok t1 + t2
  // ok
//  foo(t1 + t2)

  def bar(x: Any) = {
    x
  }

  bar ( foo (t1 +t2))
}
