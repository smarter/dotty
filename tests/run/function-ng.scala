object Test {
  def test1(f: (Int, Int) => Int) = {
    f(1, 2)
  }

  def main(args: Array[String]): Unit = {
    val z = test1(new PolyFunction {
      def apply(x: Int, y: Int): Int = x + y
    })
    println(z)
  }
}
