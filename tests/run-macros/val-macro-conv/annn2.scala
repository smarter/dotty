package a

// def f(x: Int): Int @ann(() => x) = x

class dummy[T](g: Boolean) extends annotation.StaticAnnotation with annotation.RefiningAnnotation

// transparent inline def mydef= 42
// transparent inline def isPowerOfTwoOrig(x: Int) = x != 0 && (x & (x - 1)) == 0
transparent inline def isPowerOfTwoIP(inline x: Int) = x != 0 && (x & (x - 1)) == 0

// transparent inline def isMultipleOf(x: Int, y: Int) = x % y == 0
// isMultipleOf(x, 2)
//


// OK
// class foo(elem: Int, bla: Int @ann(Target.conv((x: Int) => elem == mydef)))

// OK
// class foo(elem: Int, bla: Int @ann(Target.conv((x: Int) => isPowerOfTwoOrig(elem))))

// weird error due to remaining proxy var
// class foo(elem: Int, bla: Int @ann(Target.conv((x: Int) => isPowerOfTwoOrig(elem+1))))

// OK
class foo(elem: Int, bla: Int @ann(Target.conv((x: Int) => isPowerOfTwoIP(elem+1))))

object Test:
  // def foo(elem: Int, bla: Int @ann[Int, elem.type](Target.conv((x: Int) => elem == 0))) = bla
  // def foo(elem: Int, bla: Int @ann(new StringTarget[Int, elem.type](""))) = bla

  // FIXED: WAS:
  // // We correctly infer `elem.type`, but because it's not a SingletonTypeTree(Ident) but instead a TermRef (transformed into TermParamRef),
  // // refersToParamOf fails on it.
  // def foo(elem: Int, bla: Int @ann(Target.conv((x: Int) => elem == 0))) = bla

  // def foo(elem: Int, bla: Int @dummy(elem == 0)) = bla
  // def foo(elem: Int, bla: Int @dummy[elem.type](0 == elem)) = bla
  new foo(1, 3)
