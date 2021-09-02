trait Monad[F[_]]:
  extension [A](fa: F[A])
    def *>[B](fb: F[B]): F[B] = ???

object Test {


  // implicit val AALIST: Monad[[A] =>> List[A]] = ???
  implicit val AALIST: Monad[[A] =>> List[A]] = ???
  // implicit val BBFUNC: Monad[[A] =>> Int => A] = ???
  implicit def BBFUNC[T]: Monad[[A] =>> T => A] = ???

  val y = List(1, 2) *> List(3, 4)

  // def foo[T, A, B](x: List[A])(y: T => B) = x
  // def foo[T, A, B](x: T => A)(y: T => B) = x

  // val l: List[Int] = List(1)
  // val z = foo(l)(l)
}
