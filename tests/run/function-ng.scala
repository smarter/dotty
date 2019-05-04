trait Hi extends dotty.DottyPredef.Function1[Any, Int]
    with dotty.DottyPredef.Function2[Any, Any, Int]

object Test {
  def test1(f: Int => Int) = {
    f(1)
  }
  def test2(f: (Int, Int) => Int) = {
    f(1, 2)
  }

  def main(args: Array[String]): Unit = {
    val h = new Hi/*PolyFunction*/ {
      def apply(x: Any): Int = x match {
        case x: Int => x
      }
      def apply(x: Any, y: Any): Int = (x,y) match {
        case (x: Int, y: Int) => x + y
      }
    }
    println(test1(h))
    println(test2(h))
  }
}
