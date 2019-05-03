trait Hi extends dotty.DottyPredef.Function1[Any, Int]

object Test {
  def test1(f: Int => Int) = {
    f(1)
  }

  def main(args: Array[String]): Unit = {
    val z = test1(new Hi/*PolyFunction*/ {
      def apply(x: Any): Int = x match {
        case x: Int => x
      }
    })
    println(z)
  }
}
