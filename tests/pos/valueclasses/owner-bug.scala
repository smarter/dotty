class Meter(val underlying: Int) extends AnyVal {
  def foo(): Boolean = true
}

trait Reporter[T] {
  def report(x: T): Unit
}

class Example extends Reporter[Meter] {
  override def report(x: Meter) = {
    println(x.underlying)
  }
}
