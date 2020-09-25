object Test {
  def assertDifferent[T, U](expr: IArray[T], sources: IArray[U]*): Unit = ???

  val arr1 = IArray[Int](1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
  assertDifferent(arr1.scan(0)(_ + _), arr1)
}
