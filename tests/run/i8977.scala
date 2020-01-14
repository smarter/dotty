object Test {
  val pi = 3.1415926

  def main(args: Array[String]): Unit = {
    System.out.printf("pi = %6.4f\n", pi)

    System.out.printf("pi = %6.4f\n", Seq(pi):_*)
    // also: Array[Int] !<:< Array(_ <: Object]
    //                       ==> should erase to Object?
    // cannot appear in signatures (except when translating varargs?)
    // also: Array[Int] !<:< Array[T] where T <: Object
    //                       ==>  translated to Array[T & ActualObject]
    // (overrides safe because we check signatures)
  }
}
