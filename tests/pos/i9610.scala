trait ZIO[-R, +E, +A] {
  def interruptAs(fiberId: Long): ZIO[Any, Nothing, Exit[E, A]] =
    ???
}

trait Exit[+E, +A] {
  def flatten[E1 >: E, B](implicit ev: A <:< Exit[E1, B]): Exit[E1, B] =
    ???
}

def unsafeRunWith[R, E, A](zio: ZIO[R, E, A])(k: Exit[E, A] => Any): Long => (Exit[E, A] => Any) => Unit =
  fiberId => k => unsafeRunAsync(zio.interruptAs(fiberId))(exit => k(exit.flatten))
   // |                                                              ^^^^^^^^^^^^
   // |                                                  Found:    Exit[Any, A]
   // |                                                  Required: Exit[E, A]

def unsafeRunAsync[R, E, A](zio: ZIO[R, E, A])(k: Exit[E, A] => Any): Unit =
  ???
