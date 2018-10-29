object App {
  type T[A <: ((a : A) => a.type)] = A
  val a: T[_] = (x: Int) => x
}
