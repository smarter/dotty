// trait TC[F[_]]

// object TC {
//   implicit def instance1: TC[[x] =>> Int => x] = new TC[[x] =>> Int => x] {}
//   implicit def instance2: TC[List] = new TC[List] {}
// }

// case class Foo1[F1[_], A1](value: F1[A1]) {
//   def bar(other: Foo3[F1, A1])(implicit f: TC[F1]): Foo3[F1, A1] = other
//   def bla(other: Foo3[F1, A1])(implicit f: TC[F1]): Foo3[F1, A1] = other
// }
// case class Foo2[F2[_], A2](value: F2[A2]) {
//   def bla(other: Foo3[F2, A2])(implicit f: TC[F2]): Foo3[F2, A2] = other
// }
// case class Foo3[F3[_], A3](value: F3[A3])

// val xs = List(1, 2, 3)
// // def foo1 = Foo(xs).bar(Foo(xs))
// // def foo2 = Foo(xs).bar(foo1)
// // def foo3 = Foo(xs).bar(Foo(xs)).bar(Foo(xs))
// // def foo4 = Foo(xs).bar(Foo(xs).bar(Foo[List, Int](xs)))
// def foo5 = Foo1(xs).bar(Foo2(xs).bla(Foo3(xs)))

class Inv[A]
class X
class Y extends X

object Test {
  implicit def impX: Inv[X] = ???
  implicit def impY: Inv[Y] = ???

  def foo[T](x: T)(implicit inv: Inv[T]): Inv[T] = ???
  // def wrap[T <: X](x: T, y: Inv[T]): Any = ???
  def wrap[S](y: Inv[S]): Any = ???
  val y: Y = ???

  // foo(y) // OK
  wrap(foo(y)) // ambiguous
}
