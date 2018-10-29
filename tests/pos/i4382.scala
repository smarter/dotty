object App {
  type Id[A] >: A <: A
  def v1: Id[_] = 1

  type HkL[A] >: A
  def v2: HkL[_] = 1

  type HkU[A] <: A
  def v3: HkU[_] = ???

  type HkAbs[A]
  def v4: HkAbs[_] = ???
}
