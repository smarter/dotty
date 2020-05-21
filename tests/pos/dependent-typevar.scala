class Foo[A] {
  def bar[B >: A <: String]: String = ""
}
object Test {
  val ret = (new Foo).bar
}
