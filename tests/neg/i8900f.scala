trait Base {
  type M
  def m: M = ???
}
trait A extends Base {
  type M >: Int | String
}
trait B extends Base {
  type M <: Int & String
}
object Test {
  // def foo[T](z: T, x: A & B => T): T = z
  def foo2[T](z: T, x: T): T = z

  def main(args: Array[String]): Unit = {
    // val x = foo(1, x => (??? : x.M))
    // val x1: String = a // ClassCastException

    // val a: (A & B)#M = foo2(1,
    val a = foo2(1, // error: A & B is not a legal path
      if false then
        var x: A & B = ???
        // ??? : x.M
        x.m
      else 1
    )

    val b: String = a // ClassCastException
  }
}

