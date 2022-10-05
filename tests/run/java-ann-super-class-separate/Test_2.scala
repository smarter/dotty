class Foo extends Ann_1 {
  def value = 1
  def annotationType = classOf[Ann_1]
}

object Test {
  def main(args: Array[String]): Unit = {
    val x = new Foo
    val y: Ann_1 = x
    val z: Int @Ann_1(1) = 1
    // val x: scala.annotation.Annotation = new Ann {
    // // val x: java.lang.annotation.Annotation = new Ann {
    //   def annotationType = classOf[Ann]
    // }
    // println(x)
  }
}
