package dotty

object DottyPredef {

  type Function0[+R] =
    scala.Function0[R]
  type Function1[-T1, +R] =
    PolyFunction { def apply(a: T1): R }
    // scala.Function1[T1, R]
  type Function2[-T1, -T2, +R] =
    PolyFunction { def apply(a: T1, b: T2): R }
    // scala.Function2[T1, T2, R]
  type Function3[-T1, -T2, -T3, +R] =
    PolyFunction { def apply(a: T1, b: T2, c: T3): R }
    // scala.Function3[T1, T2, T3, R]
  type Function4[-T1, -T2, -T3, -T4, +R] =
    scala.Function4[T1, T2, T3, T4, R]
  type Function5[-T1, -T2, -T3, -T4, -T5, +R] =
    scala.Function5[T1, T2, T3, T4, T5, R]
  type Function6[-T1, -T2, -T3, -T4, -T5, -T6, +R] =
    scala.Function6[T1, T2, T3, T4, T5, T6, R]
  type Function7[-T1, -T2, -T3, -T4, -T5, -T6, -T7, +R] =
    scala.Function7[T1, T2, T3, T4, T5, T6, T7, R]
  type Function8[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, +R] =
    scala.Function8[T1, T2, T3, T4, T5, T6, T7, T8, R]
  type Function9[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, +R] =
    scala.Function9[T1, T2, T3, T4, T5, T6, T7, T8, T9, R]

  type Function10[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, +R] =
    scala.Function10[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R]
  type Function11[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, +R] =
    scala.Function11[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R]
  type Function12[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, +R] =
    scala.Function12[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R]
  type Function13[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, +R] =
    scala.Function13[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R]
  type Function14[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, +R] =
    scala.Function14[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R]
  type Function15[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, +R] =
    scala.Function15[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R]
  type Function16[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, +R] =
    scala.Function16[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R]
  type Function17[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, +R] =
    scala.Function17[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R]
  type Function18[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, +R] =
    scala.Function18[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R]
  type Function19[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, +R] =
    scala.Function19[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R]
  type Function20[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, +R] =
    scala.Function20[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R]
  type Function21[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, +R] =
    scala.Function21[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R]
  type Function22[-T1, -T2, -T3, -T4, -T5, -T6, -T7, -T8, -T9, -T10, -T11, -T12, -T13, -T14, -T15, -T16, -T17, -T18, -T19, -T20, -T21, -T22, +R] =
    scala.Function22[T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R]


  @forceInline final def assert(assertion: => Boolean, message: => Any): Unit = {
    if (!assertion)
      assertFail(message)
  }

  @forceInline final def assert(assertion: => Boolean): Unit = {
    if (!assertion)
      assertFail()
  }

  def assertFail(): Unit = throw new java.lang.AssertionError("assertion failed")
  def assertFail(message: => Any): Unit = throw new java.lang.AssertionError("assertion failed: " + message)

  @forceInline final def implicitly[T](implicit ev: T): T = ev

  @forceInline def locally[T](body: => T): T = body

  /**
   * Retrieve the single value of a type with a unique inhabitant.
   *
   * @example {{{
   * object Foo
   * val foo = valueOf[Foo.type]
   * // foo is Foo.type = Foo
   *
   * val bar = valueOf[23]
   * // bar is 23.type = 23
   * }}}
   * @group utilities
   */
  inline def valueOf[T]: T = implicit match {
    case ev: ValueOf[T] => ev.value
  }

  inline def the[T](implicit x: T): x.type = x
}
