
import Macros._

import scala.internal.quoted.Matcher._

import scala.internal.Quoted.{patternHole, patternBindHole}

object Test {

  def main(args: Array[String]): Unit = {
    val b: Boolean = true
    val x: Int = 42
    val y: Int = 52
    var z: Int = 62
    var z2: Int = 62
    def f(a: Int): Int = 72
    def f2(a: Int, b: Int): Int = 72
    def g[A]: A = ???
    def h[A](a: A): A = a
    def fs(a: Int*): Int = 72

    matches(1, 1)
    matches(1, 2)
    matches(1: Int, 1)
    matches(1: Int, 1: Int)
    matches(1, 1: Int)
    matches(3, patternHole[Int])
    matches(x, patternHole[Int])
    matches(5, patternHole[Any])
    matches(6 + x, patternHole[Int])
    matches(6 + x, 6 + patternHole[Int])
    matches(6 + x, patternHole[Int] + x)
    matches(6 + x, patternHole[Int] + patternHole[Int])
    matches(6 + x + y, 6 + patternHole[Int] + y)
    matches(4, patternHole[String])
    matches(6 + x, 7 + patternHole[Int])
    matches(6 + x, patternHole[Int] + 4)
    matches(g[Int], patternHole[String])
    matches(h[Int](7), h[String](patternHole[String]))
    matches(h[Int](6), h[Int](7))
    matches({z = 4}, {z = 5})
    matches({z = 4}, {z2 = 4})
    matches(f(4), patternHole[Int])
    matches(f(5), f(patternHole[Int]))
    matches(g[Int], patternHole[Int])
    matches(h[Int](7), patternHole[Int])
    matches(h[Int](8), h[Int](patternHole[Int]))
    matches(this, this)
    matches(this, patternHole[this.type])
    matches(new Foo(1), new Foo(1))
    matches(new Foo(1), patternHole[Foo])
    matches(new Foo(1), new Foo(patternHole[Int]))
    matches(if (b) x else y, if (b) x else y)
    matches(if (b) x else y, patternHole[Int])
    matches(if (b) x else y, if (patternHole[Boolean]) patternHole[Int] else patternHole[Int])
    matches(while (b) x, while (b) x)
    matches(while (b) x, patternHole[Unit])
    matches(while (b) x, while (patternHole[Boolean]) patternHole[Int])
    matches({z = 4}, {z = 4})
    matches({z = 4}, patternHole[Unit])
    matches({z = 4}, {z = patternHole[Int]})
    // matches({z = 4}, {varHole = 4})
    matches(1, {1})
    matches({1}, 1)
    // Should these match?
    // matches({(); 1}, 1)
    // matches(1, {(); 1})
    matches(fs(), fs())
    matches(fs(), fs(patternHole[Seq[Int]]: _*))
    matches(fs(1, 2, 3), fs(1, 2, 3))
    matches(fs(1, 2, 3), fs(patternHole[Int], patternHole[Int], 3))
    matches(fs(1, 2, 3), fs(patternHole[Seq[Int]]: _*))
    matches(f2(1, 2), f2(1, 2))
    matches(f2(a = 1, b = 2), f2(a = 1, b = 2))
    matches(f2(a = 1, b = 2), f2(a = patternHole[Int], b = patternHole[Int]))
    // Should these match?
    // matches(f2(a = 1, b = 2), f2(1, 2))
    // matches(f2(b = 2, a = 1), f2(1, 2))
    matches(super.toString, super.toString)
    matches(() => "abc", patternHole[() => String])
    matches((() => "abc")(), (patternHole[() => String]).apply())
    matches((x: Int) => "abc", patternHole[Int=> String])
    matches(((x: Int) => "abc")(4), (patternHole[Int => String]).apply(4))
    matches((x: Int) => "abc", (x: Int @patternBindHole) => patternHole[String])
    matches(StringContext("abc", "xyz"), StringContext("abc", "xyz"))
    matches(StringContext("abc", "xyz"), StringContext(patternHole, patternHole))
    matches(StringContext("abc", "xyz"), StringContext(patternHole[Seq[String]]: _*))
    matches({ val a: Int = 45 }, { val a: Int = 45 })
    matches({ val a: Int = 45 }, { @patternBindHole val a: Int = patternHole })
    matches({ val a: Int = 45 }, { lazy val a: Int = 45 })
    matches({ val a: Int = 45 }, { var a: Int = 45 })
    matches({ val a: Int = 45 }, { @patternBindHole var a: Int = patternHole })
    matches({ lazy val a: Int = 45 }, { val a: Int = 45 })
    matches({ lazy val a: Int = 45 }, { lazy val a: Int = 45 })
    matches({ lazy val a: Int = 45 }, { var a: Int = 45 })
    matches({ lazy val a: Int = 45 }, { @patternBindHole val a: Int = patternHole })
    matches({ lazy val a: Int = 45 }, { @patternBindHole var a: Int = patternHole })
    matches({ var a: Int = 45 }, { val a: Int = 45 })
    matches({ var a: Int = 45 }, { lazy val a: Int = 45 })
    matches({ var a: Int = 45 }, { var a: Int = 45 })
    matches({ var a: Int = 45 }, { @patternBindHole val a: Int = patternHole })
    matches({ var a: Int = 45 }, { @patternBindHole lazy val a: Int = patternHole })
    matches({ println(); println() }, { println(); println() })
    matches({ { println() }; println() }, { println(); println() })
    matches({ println(); { println() } }, { println(); println() })
    matches({ println(); println() }, { println(); { println() } })
    matches({ println(); println() }, { { println() }; println() })
    matches({ def a: Int = 45 }, { def a: Int = 45 })
    matches({ def a: Int = 45 }, { @patternBindHole def a: Int = patternHole[Int] })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int, y: Int): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int)(y: Int): Int = 45 })
    matches({ def a(x: Int, y: Int): Int = 45 }, { def a(x: Int): Int = 45 })
    matches({ def a(x: Int)(y: Int): Int = 45 }, { def a(x: Int): Int = 45 })
    matches({ def a(x: String): Int = 45 }, { def a(x: String): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int @patternBindHole): Int = 45 })
    matches({ def a(x: Int): Int = 45 }, { def a(x: Int @patternBindHole): Int = 45 })
    matches({ def a(x: Int): Int = x }, { def b(y: Int): Int = y })
    matches({ def a: Int = a }, { def b: Int = b })
    matches({ lazy val a: Int = a }, { lazy val b: Int = b })
    matches(1 match { case _ => 2 }, 1 match { case _ => 2 })
    matches(1 match { case _ => 2 }, patternHole[Int] match { case _ => patternHole[Int] })
    matches(??? match { case None => 2 }, ??? match { case None => 2 })
    matches(??? match { case Some(1) => 2 }, ??? match { case Some(1) => 2 })
    // matches(??? match { case Some(1) => 2 }, ??? match { case Some(patternMatchHole()) => 2 })
    // matches(??? match { case Some(n) => 2 }, ??? match { case Some(patternMatchBindHole(n)) => 2 })
    // matches(??? match { case Some(n @ Some(m)) => 2 }, ??? match { case Some(patterMatchBindHole(n @ Some(patternMatchBindHole(m)))) => 2 })
    matches(try 1 catch { case _ => 2 }, try 1 catch { case _ => 2 })
    matches(try 1 finally 2, try 1 finally 2)
    matches(try 1 catch { case _ => 2 }, try patternHole[Int] catch { case _ => patternHole[Int] })
    matches(try 1 finally 2, try patternHole[Int] finally patternHole[Int])

  }
}

class Foo(a: Int)
