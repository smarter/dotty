trait One[X]  {
  def concat(suffix: Int)(implicit dummy: DummyImplicit): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Foo

class Bar extends One[Foo] with Two[Foo] {
  concat(0) // error: ambiguous overload
  concat[Int](0) // OK
}

