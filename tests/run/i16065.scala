trait MyConsumer[T] {
  var x: Int = 1
  def accept(x: T): Unit
}

object Test {
  def main(args: Array[String]): Unit = {
    val c: MyConsumer[_ >: String] = x => ()
    c.accept("foo")
  }
}
