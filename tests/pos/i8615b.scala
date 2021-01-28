class ArrayOrdering[N] extends Comparable[Array[N]] {
  override def compareTo(x: Array[N]): Int = 0
}

// class ArrayIntOrdering extends scala.math.Ordering[Array[Int]] {
//   def compare(x: Array[Int], y: Array[Int]) = ??? // works fine
// }
