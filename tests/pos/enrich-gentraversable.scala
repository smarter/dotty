import scala.language.implicitConversions
import scala.language.postfixOps

object Test {
  import scala.collection.generic.IsIterable
  import scala.collection.{BuildFrom, Iterable, IterableOps, View}
  import scala.collection.immutable.TreeMap

  def testIterableOps = {
    class FilterMapImpl[A, Repr](r: Repr, it: IterableOps[A, Iterable, _]) {
      final def filterMap[B, That](f: A => Option[B])(implicit bf: BuildFrom[Repr, B, That]): That = ???
    }

    val fmi: FilterMapImpl[(Int, String), Map[Int, String]] = ???
    // val fmm = fmi.filterMap({ case (k, v) => Some((k, v)) })
    val fmm = fmi.filterMap({ case (k, v) => Some(k -> v) })
    val y: Map[Int, String] = fmm
  }
}
