sealed trait Nat
case class Z() extends Nat
case class S[PARAM <: Nat](pred: PARAM) extends Nat

class A {
  implicit def succ[SUCC <: Nat](implicit n: SUCC): S[SUCC] = S(n)
  implicit def base: Z/*.type*/ = Z()

  def test: Unit = {
    def lookup[N <: Nat](n: N)(implicit m: N) = m
    // val f = S(S(Z()))
    val z = lookup(S(S(Z()))) // ===> S(Z) : S[Nat & Product & Serializable]
    val z2: S[S[Z]] = z
  }
}
