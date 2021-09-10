trait Foo[F[_]]:
  extension [A](fa: F[A])
    def foo[B](fb: F[B]): Int

@main def Test =
  given listFoo: Foo[List] with
    extension [A](fa: List[A])
      def foo[B](fb: List[B]): Int = 1
  
  given functionFoo[T]: Foo[[A] =>> T => A] with
    extension [A](fa: T => A)
      def foo[B](fb: T => B): Int = 2
  
  val x = List(1, 2).foo(List(3, 4))
  assert(x == 1, x)
