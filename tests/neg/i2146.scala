object Test {
  case class A()
  case class B()

  def foo[A, B]: implicit A => implicit B => Int = { implicit b: B =>  // error: found Int, required: implicit A => implicit B => Int
    42
  }
}
