class Comp extends java.lang.Comparable[Test]

object Test {
  def main(args: Array[String]): Unit = {
    java.util.Collections.min(new java.util.ArrayList[Test](new Comp))
  }
}
// object Test {
//   def main(args: Array[String]): Unit = {
//     val t = new Test {def compareTo(x$0: Test): Int = 0 }
//     t.test
//   }
// }
