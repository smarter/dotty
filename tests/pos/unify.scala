//// crash:
// class Unrelated
// trait A:
//   def foo[S >: T <: T | Int, T](x: T => Any, y: T): T = y
//   foo((x: String) => x, 1) // doesn't crash anymore, ideally should compile?


// class Unrelated
// trait A:
//   def foo[S >: T <: T | Int, T](x: T => T): T
//   foo((x: String) => 1)
  
