trait Foo[F[_]]:
  extension [A](fa: F[A])
    def foo[B](fb: F[B]): Int

object Test:

  // def test1 =
  //   // ()
  //   given listFoo: Foo[List] with
  //     extension [A](fa: List[A])
  //       def foo[B](fb: List[B]): Int = 1
      
  //   given functionFoo[T]: Foo[[A] =>> T => A] with
  //     extension [A](fa: T => A)
  //       def foo[B](fb: T => B): Int = 2

  //   val x = List(1, 2).foo(List(3, 4))
  //   assert(x == 1, x)
  // end test1

  def test1 =
    ()
    // given functionFoo[T]: Foo[[A] =>> T => A] with
    //   extension [A](fa: T => A)
    //     def foo[B](fb: T => B): Int = 2

    // given listFoo[T <: AnyVal]: Foo[[A] =>> A => T] with
    //   extension [A](fa: A => T)
    //     def foo[B](fb: B => T): Int = 1

    // val f1: Int => Int = x => x
    // val f2: Int => Int = x => x
    // val x = f1.foo(f2)
    // assert(x == 1, x)
  end test1

  def test2 =
    // object XX {
    //   def foo[A, B](x: A => A): Int = 1
    //   @annotation.targetName("dd")
    //   def foo[A, B](x: A => B): Int = 1

    //   // val f = (1, 1)
    //   val f: Int => Int = ???
    //   foo(f)
    // }

    // trait Bla[A, B]:
    //   // extension (x: A) def foo(y: A => A): Int
    //   extension (x: A => B) def foo(y: (A, B)): Int
    // trait Bla2[A, B]:
    //   extension (x: A => B) def foo(y: (A, B)): Int
    //   // extension (x: A) def foo(y: A => B): Int

    // // given bla1[X <: AnyVal]: Bla[X, AnyVal] = ???
    // // given bla2[T, S <: T]: Bla2[S, T] = ???

    // given bla1[T >: AnyVal, S]: Bla[S, T] = ???
    // given bla2[T, S <: Int]: Bla2[S, T] = ???

    // val f: Int => Int = ???
    // f.foo((1, 1))

    // trait Bla[F[_], A]:
    //   extension (x: F[Int] => F[A]) def foo(y: F[A]): Int
    // trait Bla2[G[_], A]:
    //   extension (x: G[Int] => G[A]) def foo(y: G[A]): Int

    // class Inv[+T]
    // class Inv2[+T] extends Inv[T]

    // given bla1[T /*<: AnyVal*/]: Bla[Inv, T] = ???
    // given bla2[H[+X /*<: AnyVal*/]]: Bla2[H, Int] = ???

    // // val f = new Inv2[Int]
    // val f: Inv2[Int] => Inv2[Int] = ???
    // val g: Inv2[Int] = ???
    // f.foo(g)
    // 1.foo(f)
    // f.foo("", "")

    // [T, S]: Bla[T, S]
    // [T]: Bla[T, T]

    // given functionFooAnyVal[T >: AnyVal]: Foo[[A] =>> T => A] with
    //   extension [A](fa: T => A)
    //     def foo[B](fb: T => B): Int = 3

    // given functionFooInt[T]: Foo[[A] =>> A => T] with
    //   extension [A](fa: A => )
    //     def foo[B](fb: T => B): Int = 4

    // val x = List(1, 2).foo(List(3, 4))
    // assert(x == 4, x)
  end test2
  
  def main(args: Array[String]): Unit =
    test1
    test2
