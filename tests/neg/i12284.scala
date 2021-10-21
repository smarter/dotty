trait I[H[_], C]

def magic[F[_], A](in: I[F, A]): F[A] = ???
def moo[G[_], B](igb: I[G, B]): G[B] =
  val deps: Vector[I[G, _]] = ???
  // TODO: map gets inferred to ?B := [i.A] because we start with ?B >: F[?A] and only later get ?A := i.A
  // see discussion in i8900a3.scala to deal with that.
  val xx: Vector[G[_]] = deps.map(i => magic(i)) // error
  ???
