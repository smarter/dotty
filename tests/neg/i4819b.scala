trait One[X]  {
  def concat(suffix: Int): X = ???
}

trait Two[Y <: Foo] {
  def concat[Dummy](suffix: Int): Y = ???
}

class Foo

class Bar extends One[Foo] with Two[Foo] // error: double definition of concat after erasure

