import scala.reflect.ClassTag
import scala.language.implicitConversions

class A
class B

object Test {
  def doubleSeq[T](x: T): Seq[T] = Seq(x, x)
  def doubleArray[T: ClassTag](x: T): Array[T] = Array(x, x)

  def box(x: Integer*): Unit = {}
  def widen(x: Long*): Unit = {}
  def conv(x: B*): Unit = {}

  box(doubleSeq(1): _*)
  box(doubleArray(1): _*)

  widen(doubleSeq(1): _*)
  widen(doubleArray(1): _*)

  implicit def aToB(x: A): B = new B
  val a: A = new A
  conv(doubleSeq(a): _*)
  conv(doubleArray(a): _*)
}
