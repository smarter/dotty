trait I[F[_], A]

def magic[F[_], A](in: I[F, A]): F[A] =
  val deps: Vector[I[F, _]] = ???
  // TODO: map gets inferred to ?B := [i.A] because we start with ?B >: F[?A] and only later get ?A := i.A
  // see discussion in i8900a3.scala to deal with that.
  val xx: Vector[F[_]] = deps.map(i => magic(i)) // error
  ???
