class S extends pkg.J {
  override def i(): Int = 2
}
object Test {
  val s = new S

  val i1 = s.i
  val i2 = s.i()
}
