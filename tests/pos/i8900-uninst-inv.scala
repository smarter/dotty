class Inv[A <: Singleton](x: A)
object Inv {
  def empty[A <: Singleton]: Inv[A] = new Inv(???)
}

class Inv2[A](x: A)
object Inv2 {
  def empty[A]: Inv2[A] = new Inv2(???)
}

object Test {
  def inv(cond: Boolean) = // leak: Inv[x.type]
    if (cond)
      // new Inv(1)
      val x: Int = 1
      new Inv(x)
    else
      Inv.empty
}
