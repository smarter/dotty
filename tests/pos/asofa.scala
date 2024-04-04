object asOfA:
	transparent inline def unapply[F](e: Any): Option[Any] = Some(e.asInstanceOf[F])

class A:
  def test(x: Int) = x match
    case asOfA[Int](e) => e


