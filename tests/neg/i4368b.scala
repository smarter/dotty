object Test1 {
  trait X {
    type A = B
    type B
  }
  trait Y {
    type A
    type B = A
  }
  trait Z extends X with Y // error: cyclic
}

object Test2 {
  trait W {
    type A
    type B
  }
  trait X { z: W =>
    type A = z.B
    type B
  }
  trait Y { z: W =>
    type A
    type B = z.A
  }
  trait Z extends X with Y // error: illegal inheritance // error: cyclic
}

object Test6 {
  trait W { type T <: W; val t: T }
  trait X {
    type A = b.T
    val a : A = b.t
    type B <: W
    val b : B
  }
  trait Y {
    type A <: W
    val a : A
    type B = a.T
    val b = a.t
  }
  trait Z extends X with Y // error: cyclic
}
