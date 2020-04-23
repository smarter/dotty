class Outer {
  trait TraitX extends ClassY
  class ClassY
}
object Api {
  def foo(o1: Outer, o2: Outer): o1.ClassY with o2.TraitX = {}
}
