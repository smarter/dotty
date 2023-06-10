trait A:
  def foo(x: Int)(using str: String): Int

object Test {
  import TypeToolbox.*
  def main(args: Array[String]): Unit = {
    show("A")
  }
}
