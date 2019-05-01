object Test {
  def test1(f: PolyFunction { def apply(x: List[Int]): Int }) = {
    f(List(1, 2, 3))
  }

  def main(args: Array[String]): Unit = {
    val z = test1(new PolyFunction {
      def apply(x: List[Int]): Int = x.sum
    })
    println(z)
  }
}
