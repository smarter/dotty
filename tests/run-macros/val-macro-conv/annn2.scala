package a

// def f(x: Int): Int @ann(() => x) = x

class dummy[T](g: Boolean) extends annotation.StaticAnnotation with annotation.RefiningAnnotation

object Test:
  // def foo(elem: Int, bla: Int @ann[Int, elem.type](Target.conv((x: Int) => elem == 0))) = bla

  // FIXED: WAS:
  // // We correctly infer `elem.type`, but because it's not a SingletonTypeTree(Ident) but instead a TermRef (transformed into TermParamRef),
  // // refersToParamOf fails on it.
  def foo(elem: Int, bla: Int @ann(Target.conv((x: Int) => elem == 0))) = bla

  // def foo(elem: Int, bla: Int @dummy(elem == 0)) = bla
  // def foo(elem: Int, bla: Int @dummy[elem.type](0 == elem)) = bla
  foo(1, 2)
