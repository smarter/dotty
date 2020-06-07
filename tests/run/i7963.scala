object Test {
  def main(args: Array[String]): Unit = {
    val l = new java.util.ArrayList[String]
    l.add("foo")
    java.util.Collections.min(l)
  }
}
// object Test {
//   def main(args: Array[String]): Unit = {
//     val t = new Test {def compareTo(x$0: Test): Int = 0 }
//     t.test
//   }
// }
