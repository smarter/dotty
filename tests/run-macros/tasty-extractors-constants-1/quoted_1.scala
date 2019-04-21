import scala.quoted._
import scala.quoted.autolift._

import scala.quoted.matching._

import scala.tasty._
import scala.tasty.util._

object Macros {

  implicit inline def testMacro: Unit = ${impl}

  def impl(implicit reflect: Reflection): Expr[Unit] = {

    val buff = new StringBuilder
    def stagedPrintln(x: Any): Unit = buff append java.util.Objects.toString(x) append "\n"

    3.toExpr match { case Literal(n) => stagedPrintln(n) }
    '{4} match { case Literal(n) => stagedPrintln(n) }
    '{"abc"} match { case Literal(n) => stagedPrintln(n) }
    '{null} match { case Literal(n) => stagedPrintln(n) }

    '{new Object} match { case Literal(n) => println(n); case _ => stagedPrintln("OK") }

    '{print(${buff.result()})}
  }
}
