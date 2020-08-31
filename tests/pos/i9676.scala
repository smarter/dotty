// import scala.Predef.{any2stringadd => _}
// trait SubtypeOf[A, B]

// object Test {
//   def instance[G, F <: G]: SubtypeOf[F, G] = new SubtypeOf[F, G] {}

//   val x: SubtypeOf[Int, Any] = instance
// }

trait SubtypeOf[A[_], B[_]]

object Test {
  def instance[G[_], F[a] <: G[a]]: SubtypeOf[F, G] = new SubtypeOf[F, G] {}

  val x: SubtypeOf[List, Seq] = instance
}
