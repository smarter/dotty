class Foo[A] {
  def get: A = ???
}
class A {
  // def foo[T <: Foo[_]]: T = ???
  // foo.get // OK

  def list[T <: List[_]]: T = ???
  // list.head: Int
  // list.::(1)

  def foldLeft1[B <: List[_]](op: (B, Int) => B): B = ???
  // def foldLeft[B >: Nil.type](op: (B, Int) => B): B = ???

  val l = foldLeft1((acc, i) => acc.::(i))
}
