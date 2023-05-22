object Test {
  val pv0: [T] => List[T] = ???        // error
  val pv1: Any = [T] => Nil            // error
  val pv2: [T] => List[T] = [T] => Nil // error // error

  val intraDep = [T] => (x: T, y: List[x.type]) => List(y) // error

  def bar[A]: A => A = x => x
  val bar2: [T] => T => T = bar(_) // ok
  val bar3: [T] => T => T = bar // error
}
