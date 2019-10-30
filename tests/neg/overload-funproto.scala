class A(f1: Int => Int, x1: Int) {
  def this(f2: Int => Int, x2: Long) = this(arg => x1, 0) // error
}
