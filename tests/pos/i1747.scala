abstract class Coll[E] extends java.util.Collection[E] {

  // fails in elimevt check, can we get it to work? (why doesn't it fail earlier?)
  // override def toArray[T](a: Array[T with Object]): Array[T with Object] = ???
  override def toArray[T <: Object](a: Array[T]): Array[T] = ???
  // override def toArray[T](a: Array[T]): Array[T] = ???
}

