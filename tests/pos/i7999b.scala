trait TC[F[_]]

object TC {
  implicit def instance1: TC[[x] =>> Int => x] = new TC[[x] =>> Int => x] {}
  implicit def instance2: TC[List] = new TC[List] {}
}

case class Foo[F[_], A](value: F[A]) {
  def bar(other: Foo[F, A])(implicit F: TC[F]): Foo[F, A] = other
}

val xs = List(1, 2, 3)
def foo1 = Foo(xs).bar(Foo(xs))
def foo2 = Foo(xs).bar(foo1)
def foo3 = Foo(xs).bar(Foo(xs)).bar(Foo(xs))
def foo4 = Foo(xs).bar(Foo(xs).bar(Foo[List, Int](xs)))
def foo5 = Foo(xs).bar(Foo(xs).bar(Foo(xs)))
