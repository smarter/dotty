trait Contra[-T]
trait X
trait Y extends X

trait Inv[T]

def contra[T](using contra: Contra[T]): Contra[T] = contra

def test(using cx: Contra[X], cy: Contra[Y]): Unit =
  contra

def inv[T](using inv: Inv[T]): Inv[T] = inv

def test2(using cx: Inv[X], cy: Inv[Y]): Unit =
  inv

