class /[D, T]
class Delegating[D]

type Aux[E] = Container { type Elements = E }

class Container:
  type Elements = Delegating[Delegates]
  type Delegates

class Resolution[E](value: Aux[E]):
  type Type = Aux[E]

def element0: Container { type Delegates = Unit } = ???

def element22(
    transmittable0: Resolution[?])
: Container {
    type Delegates =
      transmittable0.Type
  } = ???

// def test22: Resolution[Delegating[Aux[Delegating[Unit]]]] =
def test22 =
  Resolution( // Resolution[Delegating[(?1 : Resolution[Delegating[Unit]])#Type]]
    element22(
      Resolution(element0)))
