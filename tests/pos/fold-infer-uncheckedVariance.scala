import scala.annotation.unchecked.uncheckedVariance

class Lo
// class Hi extends Lo
// class Duck extends Hi

class Foo[-T >: Null] {
  def tpe: T @uncheckedVariance = ???
}

object Test {
  def foo(xs: List[Int], x: Foo[Lo]): Unit = {
    val r = xs.foldLeft(x) { (acc, x) =>
      // B >: Foo[Lo] <: Foo[T]
      // T >: Null <: Lo
      val m = acc.tpe
      acc
    }
    val s: Foo[Lo] = r
  }
}
