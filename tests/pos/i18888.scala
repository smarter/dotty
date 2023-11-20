trait Context:
  type Elem[T]// = T

trait TypeClass[T]:
  extension (using nb: Context)(x: nb.Elem[T])
    def tcMeth: T


class Bla
object Bla:
  given fb: TypeClass[Bla] = ???

class Wrapper[T]
object Wrapper:
  given fa[T](using TypeClass[T]): TypeClass[Wrapper[T]] = ???


object Test:
  def test1(using nb: Context) =
    // val x1: nb.Elem[Bla] = ???
    val x2: nb.Elem[Wrapper[Bla]] = ???
    // x1.tcMeth // ok
    x2.tcMeth // error
  //   summon[TypeClass[Wrapper[Bla]]].tcMeth(x2) // ok

  // def test2(using nb: Context, tc: TypeClass[Wrapper[Bla]]) =
  //   val y: nb.Elem[Wrapper[Bla]] = ???
  //   y.tcMeth // ok
