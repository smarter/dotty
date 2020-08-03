trait Monad[F[_]]

case class StateT[G[_], S, A](run: S => G[(S, A)])

object Test {
  class MonadOps[M[_], A](ma: M[A]) {
    def whileM_(p: M[Boolean]): M[Unit] = ???
  }

  def inspect[I[_], X, B](f: X => B): StateT[I, X, B] = ???

  type Id[A] = A

  val increment: StateT[Id, Int, Unit] = ???

  new MonadOps(increment).whileM_(inspect(i => i > 4))
}

// trait Monad[F[_]]

// case class StateT[G[_], S, A](run: S => G[(S, A)])

// object Test {
//   class MonadOps[M[_[_], _, _], G[_], S, A](M: M[G, S, A]) {
//   // class MonadOps[M[GG[_], SS, AA] >: StateT[GG, SS, AA], G[_], S, A](M: M[G, S, A]) {
//     def whileM_(p: M[G, S, Boolean]): M[G, S, Unit] = ???
//   }

//   def inspect[I[_], X, B](f: X => B): StateT[I, X, B] = ???

//   type Id[A] = A

//   val increment: StateT[Id, Int, Unit] = ???

//   new MonadOps(increment).whileM_(inspect(i => i > 4))
// }

// trait Monad[F[_]]

// case class StateT[G[_], S, A](run: S => G[(S, A)])

// object Test {
//   class MonadOps[G[_], S, A](StateT: StateT[G, S, A]) {
//     def whileM_(p: StateT[G, S, Boolean]): StateT[G, S, Unit] = ???
//   }

//   def inspect[I[_], X, B](f: X => B): StateT[I, X, B] = ???

//   type Id[A] = A

//   val increment: StateT[Id, Int, Unit] = ???

//   new MonadOps(increment).whileM_(inspect(i => i > 4))
// }


// object Test {
//   def foo[A >: List[B], B](x: A)(y: B => Boolean): Unit = ???

//   val li: List[Int] = ???
//   foo(li)(x => x > 0) // A >: List[Int] | List[B]
// }
