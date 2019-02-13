trait One[X]  {
  def concat(suffix: Int): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

// Should be an error due to clashing forwarders after erasure but crashes with doubledef
// class Foo extends One[Foo] with Two[Foo] {
//   concat[Int](0) // OK
//   // See also tests/neg/i4819.scala
// }

class Foo

class Bar extends One[String] with Two[Foo] {
  val x: String = concat(0)
  val y = concat[Int](0)
  val z: Foo = concat(0)
}
