trait crash {
  type Abs[A, B, C <: B]

  def indexK[F[_]]: F[Any] = ???

  def res: Abs[Any, Any, Any] = indexK
}
