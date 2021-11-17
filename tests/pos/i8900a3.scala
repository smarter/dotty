class ContraCo[-T, +S](val t: S)
class CoContra[+T, -S](val t: T)
object Test {
  type Id[T] = T
  def unwrap[Outer](inv: CoContra[Outer, Outer]): Outer = inv.t
  def wrap[Inner](i: Inner): CoContra[Id[Inner], Id[Inner]] = new CoContra(i)

  val a = unwrap({
    class Local
    val local = new Local
    wrap(local)
  })
}

// Outer := Id[Inner]
// Inner
// ...
// Inner := Local
// ==> we want Outer := Id[? <: Object]
//     so setInst would do approx with variance = 0 ?
//     for top-level / hk arg, use a TypeBox?
//     for top-level, we could ensure we always instantiate Inner := Outer rather than Outer := Inner
//
// What if we don't know that Inner refers to local things when instantiating Outer?
// Delay instantiating Outer until all tvars it refers to have been instantiated?
// ... but what about recursive references? Outer <: Foo[Inner]; Inner <: Foo[Outer]
// Delay instantiating Outer until all tvars *newer than it* have been instantiated
// => gc() need to process tvars in order

// object Test {
//   type F[A] <: (A, A)
  

//   should only be possible in compiler-generated type
//   val x: F[scala.runtime.TypeBox[Nothing, Int]#CAP] = ???

//   val y: Int = x._1
// }
