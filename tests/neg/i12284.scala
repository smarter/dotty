trait I[F[_], A]

def magic[F[_], A](in: I[F, A]): F[A] =
  val deps: Vector[I[F, _]] = ???
  val xx: Vector[F[_]] = deps.map(i => magic(i)) // error
  ???
