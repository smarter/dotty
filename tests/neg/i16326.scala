trait Writer[A]

object Writer:
  implicit val wi: Writer[Int] = ???
  implicit val ws: Writer[String] = ???

def write[A](using writer: Writer[A])(a: A) = ""

def foo(o: Option[Boolean]): Unit =
  o.map[Boolean](_ =>
    o.map[Boolean](_ =>
      val b = write(1).++((1, 2).toString) // error
      false
    )
    false
  )
