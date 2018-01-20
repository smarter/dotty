trait Foo[+A]
class Bar[A] extends Foo[A]

class Two[A, B]
class One[A] extends Two[A, A]

object Test {
  def g[F[+_]](fi: F[Int]): F[Any] = fi
  def foo[F[_, _]](fi: F[Int, Int]) = fi

  // val a = new One[Int]
  // foo(a)

  // g(new Bar[Int]: Foo[Int]) // OK

  g(new Bar[Int])           // does not typecheck, despite having strictly more
                            // information about the type of the argument
}
