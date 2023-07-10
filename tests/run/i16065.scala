trait MyConsumer[T] {
  var x: Int = 1
  def accept(x: T): Unit
}

trait MyProdCons[T] {
  var x: Int = 1
  def accept(x: T): T
}

object Test {
  def main(args: Array[String]): Unit = {
    val c: MyConsumer[_ >: String] = x => ()
    c.accept("foo")
    val d: MyProdCons[_ <: String] = x => x
  }
}
