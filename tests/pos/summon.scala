class X
class Y

class A:
  implicit val x: X = new X
  implicit val y: Y = new Y
  def mySummon[T](using x: T): x.type = x

  val z: X = mySummon

