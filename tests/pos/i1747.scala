// TODO: check that all of those have same sig and work

abstract class Coll1[E] extends java.util.Collection[E] {
  override def toArray[T](a: Array[T with Object]): Array[T with Object] = ???
}

abstract class Coll2[E] extends java.util.Collection[E] {
  override def toArray[T <: Object](a: Array[T]): Array[T] = ???
}

abstract class Coll3[E] extends java.util.Collection[E] {
  override def toArray[T](a: Array[T]): Array[T] = ???
}
