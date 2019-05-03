object Test {
  def test1(f: (Int, Int, Int) => Int) = {
    f(1, 2, 3)
  }

  def main(args: Array[String]): Unit = {
    val z = test1(new PolyFunction {
      def apply(x: Int, y: Int, z: Int): Int = x + y + z
    })
    println(z)
  }
}
