class Tool {
  type Foo

  def inputGrist: List[{ type Bla <: Foo }] = Nil
}

class Module {
  type Hello

  val tools: List[Tool {type Foo = Hello}] = Nil

  def test = {
    val haha = tools.flatMap(_.inputGrist)
  }
}
