
def foo[A](x: A): A = x
def bar[A]: A => A = x => x
type F[A] = A => A
def baz[A]: F[A] = x => x
//def doubleTypes[A](using A)[B](x: B): B = x

// // These vals should already compile on main
// object evenBefore:
//   val foo0: Int => Int = foo
//   val foo1: Int => Int = foo[Int]
//   val foo2: [T] => T => T = [T] => (x: T) => foo[T](x)

//   val bar0: Int => Int = bar
//   val bar1: Int => Int = bar[Int]
//   val bar2: [T] => T => T = [T] => (x: T) => bar[T](x)

//   val baz0: Int => Int = baz
//   val baz1: Int => Int = baz[Int]
//   val baz2: [T] => T => T = [T] => (x: T) => baz[T](x)

// These vals should not compile on main and compile with this feature (the first failure might swallow the others)
object After:
  val foo2: [T] => T => T = foo // should be indistinguishible from evenBefore.foo2

  // val bar2: [T] => T => T = bar // should be indistinguishible from evenBefore.bar2

  // val baz2: [T] => T => T = baz // should be indistinguishible from evenBefore.baz2

  // other examples with using params and extension methods

  //val asVal: [A1] => A1 ?=> [B1] => B1 => B1 = doubleTypes
  

// // Should not compile even with this feature, to be moved to a neg test
// object notEvenAfter:
//   val withoutTypeFoo = foo // should infer Any => Any
//   val checkFoo: [T] => T => T = withoutTypeFoo // should fail as Any => Any is not a subtype of [T] => T => T

//   val withoutTypeBar = bar // should infer Any => Any ?
//   val checkBar: [T] => T => T = withoutTypeBar // should fail as Any => Any is not a subtype of [T] => T => T

//   val withoutTypeBaz = baz // should infer Any => Any ?
//   val checkBaz: [T] => T => T = withoutTypeBaz // should fail as Any => Any is not a subtype of [T] => T => T

//   val failedFoo = [T] => foo[T] // should fail as polymorphic functions need a term parameter
//   val failedBar = [T] => bar[T] // should fail as polymorphic functions need a term parameter
//   val failedBaz = [T] => baz[T] // should fail as polymorphic functions need a term parameter
  
//   //val asVal: [B1] => B1 => B1 = doubleTypes // should fail, as [A] is first argument
