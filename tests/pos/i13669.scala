trait MyExtensions:
  extension (lhs: Int) def bash: Unit = {}
object MyExtensions extends MyExtensions

// object MyExtensions:
//   extension (lhs: Int) def bash: Unit = {}

export MyExtensions.*
val fails = 1.bash


