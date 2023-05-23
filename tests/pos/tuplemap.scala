// val x = List(1, 2, 3).map(Foo.apply)

val f: [T <: Int] => T => T =
  [T] => (x: T) => x

val a = (1,2,3).map(List(_))

case class Foo[T <: Int](x: T)

val b = (1,2,3).map(Foo(_))

val c = (1,2,3).map[[_] =>> Int](_ + 1)
