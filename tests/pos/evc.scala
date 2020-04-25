trait Univ extends Any

class VC1(x: Int) extends AnyVal with Univ
class VC2(x: Int) extends AnyVal with Univ

class Test {
  def foo(x: VC1 | VC2): Unit = {}
}
