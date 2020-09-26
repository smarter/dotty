trait Foo[F[_]] {
  def foo[G[x] >: F[x]]: G[Unit]
}

trait M[A] {
  def bla: Int = 1
  def baz(f: Int => Int): Int = f(1)
}

object Test {
  def bar(x: Foo[M]): Unit = {
    // error: value bla is not a member of G[Unit], where:    G is a type variable with constraint >: M and <: [x] =>> Any
    x.foo.bla
    // XX: why does the typer add a block here?
    // error: value baz is not a member of M[Unit] - did you mean M[Unit].baz?
    x.foo.baz(x => x)
  }
}
