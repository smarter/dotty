class X
class Y

class A:
  implicit val x: X = new X
  implicit val y: Y = new Y
  def foo[T](using x: T): x.type = ???

  val z: X = foo
