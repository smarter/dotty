class LT[-X, +Y] // error: cyclic instantiation of T to D[D[T]]
object LT {
 given lt[X <: Y, Y]: LT[X, Y] = ???
}

trait C[X]
trait D[X] extends C[X]
trait A { // error: cyclic instantiation of T to D[D[T]]
  def foo[S <: C[T], T <: C[S]](using LT[D[T], S], LT[D[S], T]): S = ???

  foo
}
