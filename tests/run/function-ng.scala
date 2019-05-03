object Test {
  def test1(f: (Int, Int) => Int) = {
    f(1, 2)
  }

  def main(args: Array[String]): Unit = {
    val z = test1(new PolyFunction {
      def apply(x: Any, y: Any): Int = (x,y) match {
        case (x: Int, y: Int) => x + y
      }
    })
    println(z)
  }
}
