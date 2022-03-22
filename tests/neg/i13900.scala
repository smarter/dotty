opaque type Inlined[T] = T

object Inlined:

  given fromValueWide[Wide]: Conversion[Wide, Inlined[Wide]] = ???

  def myMax: Int = 1 max 2
