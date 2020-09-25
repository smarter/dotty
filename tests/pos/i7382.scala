object Minimized {

  trait Exit[+E, +A]

  trait Fiber[+E, +A]

  trait ZIO[-R, +E, +A] {
    final def raceWith[R1 <: R, E1, E2, B, C](that: ZIO[R1, E1, B])(leftDone: (Exit[E, A], Fiber[E1, B]) => ZIO[R1, E2, C], rightDone: (Exit[E1, B], Fiber[E, A]) => ZIO[R1, E2, C]): ZIO[R1, E2, C] =
      ???
  }

  def race[R, A, B, C](left: ZIO[R, Nothing, A], right: ZIO[R, Nothing, B])(f: (Either[A, B] => C)): ZIO[R, Nothing, C] =
    left.raceWith(right)(
      (exit, right) => ???, // XXX: inferred type of right is Fiber[Any, B] rather than Fiber[Nothing, B]
      (exit, left) => ???,
    )
}
