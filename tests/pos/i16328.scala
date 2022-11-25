class Foo
trait Typeclass[T] {
  def someString(x: T): String
}
given Typeclass[Foo] with {
  def someString(x: Foo): String = "Foo impl"
}
class Bar(val foo: Foo)
given (Foo => Boolean) = _ => true

val latestTable: Map[scala.reflect.ClassTag[_], Seq[_]] = Map(summon[scala.reflect.ClassTag[Foo]] -> Seq(new Foo))

def typeclassConstrained[T](x: T) /*(using Typeclass[T])*/ : String = ???//summon[Typeclass[T]].someString(x)
def latest[T: scala.reflect.ClassTag](constraint: T => Boolean): T = {
  latestTable(summon[scala.reflect.ClassTag[T]]).asInstanceOf[Seq[T]].filter(constraint).head
}

// val x = latest((x: Foo) => true)
// val y = typeclassConstrained(x)
val x = typeclassConstrained(latest((x: Foo) => true))
