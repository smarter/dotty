class Foo[A, B <: A] {
  // def get: A = ???
  def hi: Foo[A, B] = this
}
class A {
  // def foo[T <: Foo[_]]: T = ???
  // foo.get // OK

  // def list[T <: List[_]]: T = ???
  // list.head: Int
  // list.::(1)

  // def foldLeft1[B <: List[_]](op: (B, Int) => B): B = ???
  // val l = foldLeft1((acc, i) => acc.::(i))

  // def foldLeft2[B >: Nil.type](op: (B, Int) => B): B = ???

  // val l = foldLeft2((acc, i) => acc.::(i))

  // List(1, 2, 3).foldLeft(Nil)((acc, i) => acc.::(i))

  // implicit def conv(x: List[Int]): Int = 0


  // extension fooOps on (x: List[Int]) {
  //   def hi: Int = 0
  // }

  def foo[B >: Foo[Int, Int]](op: B => B) = ???
  foo(b => b.hi)

  // def foo[B >: List[Int]](op: B => Int) = ???
  // foo(b => b.hi)

  // List(1, 2, 3).foldLeft(Nil)((acc, i) => acc.::(i))
  Seq(1, 2, 3).foldLeft(Set(0))((acc, i) => acc + i)
  // List(1, 2, 3).foldLeft(Nil)((acc, i) => acc)
}
