trait Base
trait Sub extends Base

trait Foo[T]:
  val x: T
object Foo:
  given Foo[Base] with
    val x: Base = new Base {}
  given Foo[Sub] with
    val x: Sub = new Sub {}

class Test:
  def fromParam[T](x: T => Any)(using foo: Foo[T]): T = foo.x

  def id[S](x: S): S = x

  def test =
    fromParam((x: Base) => x) // ok before
    id(fromParam((x: Base) => x)) // ok now (before: ambiguous given for Foo[Base & S])
    id(id(fromParam((x: Base) => x))) // ok now (before: ambiguous given for Foo[Base & S])
