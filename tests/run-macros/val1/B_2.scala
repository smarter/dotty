import a.*

package b {
  // class Box[T](val x: T)
  class Box(val x: Int)
  
  import Macros.Inv
  
  class A(a: Inv[Int @check[Int](x => Box(3).x == 3)])
  
  type PosInt = Inv[Int @check[Int](x => x > 0)]
  // class B(a: Inv[PosInt @check[PosInt](x => x % 2 == 0)])

  class B(a: Array[PosInt])
  
  
  // class A(a: Int @check[Int](x => Box(3).x == 3))
}

object Test:
  import b.*
  import ValidatorConv.given
  // transparent inline implicit def conv[T, S <: T @a.checkBase](x: T): S = a.Macros.myMacro[T, S](x)
  // transparent inline implicit def conv[T, S <: T](x: T): S = a.Macros.myMacro[T, S](x)
  def main(args: Array[String]): Unit =
    // import a.Macros.myMacro
    // new a.A(a.Macros.myMacro(1): Macros.Inv[Int @check[Int](x => Box(3).x == 3)])
    // new a.A(a.Macros.myMacro(1))
    // new A(1)
    val x: Array[Int] = Array(1)
    new B(x)

