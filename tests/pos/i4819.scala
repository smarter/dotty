trait One[X]  {
  def concat(suffix: Int): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Foo

class Bar extends One[String] with Two[Foo] {
  val x: String = concat(0)
  val y = concat[Int](0)
  val z: Foo = concat(0)
}
