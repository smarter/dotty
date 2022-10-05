class Foo extends Ann {
  def value = 1
  def annotationType = classOf[Ann]
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Foo
    val y: Ann = x
    val z: Int @Ann(1) = 1
    // val x: scala.annotation.Annotation = new Ann {
    // // val x: java.lang.annotation.Annotation = new Ann {
    //   def annotationType = classOf[Ann]
    // }
    // println(x)
  }
}
