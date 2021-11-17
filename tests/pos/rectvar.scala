trait Contra[-A]
trait Cov[+B]

trait Test {
  def foo[S](x: S): S
  def rec[T <: Cov[T]]/*(x: T)*/: Contra[T]
  // def rec[T <: Cov[U], U <: T]/*(x: T)*/: Contra[T]

  foo({
    // T#2 <: Cov[T#2]
    // Contra[T#2] <: S#1
    // Contra[T#2] <: Contra[T'#1] <: S#1
    // T'#1 <: T#2
    // T'#1 <: Cov[T#2]
    // T'#1 <: Cov[T''#1] <: Cov[T#2]
    // T''#1 <: T#2
    rec
  })
}
