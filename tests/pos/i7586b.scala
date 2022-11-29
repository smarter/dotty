trait Nat

case object Z extends Nat
type Z = Z.type

case class S[N <: Nat](pred: N) extends Nat

case class Sum[N <: Nat, M <: Nat, R <: Nat](result: R)

given zero: Z = Z
given succ[N <: Nat](using n: N): S[N] = S(n)

given sumZ[N <: Nat](using n: N): Sum[Z, N, N] = Sum(n)

given sumS[N <: Nat, M <: Nat, R <: Nat](
  using sum: Sum[N, M, R]
): Sum[S[N], M, S[R]] = Sum(S(sum.result))

def add[N <: Nat, M <: Nat, R <: Nat](n: N, m: M)(
  using sum: Sum[N, M, R]
): R = sum.result

case class Prod[N <: Nat, M <: Nat, R <: Nat](result: R)

def test =
  val res = add(S(Z), S(S(Z)))
  val res2: S[S[S[Z]]] = res
