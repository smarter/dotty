package dotty.tools.backend.jvm

import org.junit.Assert._
import org.junit.Test

import scala.tools.asm.Opcodes

class TestBCode extends DottyBytecodeTest {
  import ASMConverters._
  @Test def nullChecks = {
    val source = """
                 |class Foo {
                 |  def foo(x: AnyRef): Int = {
                 |    val bool = x == null
                 |    if (x != null) 1
                 |    else 0
                 |  }
                 |}
                 """.stripMargin

    checkBCode(source) { dir =>
      val clsIn      = dir.lookupName("Foo.class", directory = false).input
      val clsNode    = loadClassNode(clsIn)
      val methodNode = getMethod(clsNode, "foo")
      correctNumberOfNullChecks(2, methodNode.instructions)
    }
  }

  /** This test verifies that simple matches are transformed if possible
   *  despite no annotation
   */
  @Test def basicTransformNonAnnotated = {
    val source = """
                 |object Foo {
                 |  def foo(i: Int) = i match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |    case 0 => println(0)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  /** This test verifies that simple matches with `@switch` annotations are
   *  indeed transformed to a switch
   */
  @Test def basicTransfromAnnotated = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Int) = (i: @switch) match {
                 |    case 2 => println(2)
                 |    case 1 => println(1)
                 |    case 0 => println(0)
                 |  }
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")
      assert(verifySwitch(methodNode))
    }
  }

  @Test def failTransform = {
    val source = """
                 |object Foo {
                 |  import scala.annotation.switch
                 |  def foo(i: Any) = (i: @switch) match {
                 |    case x: String => println("string!")
                 |    case x :: xs   => println("list!")
                 |  }
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Foo$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val methodNode = getMethod(moduleNode, "foo")

      assert(verifySwitch(methodNode, shouldFail = true))
    }
  }

  /** Make sure that creating multidim arrays reduces to "multinewarray"
   *  instruction
   */
  @Test def multidimArraysFromOfDim = {
    val source = """
                 |object Arr {
                 |  def arr = Array.ofDim[Int](2, 1)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method     = getMethod(moduleNode, "arr")

      val hadCorrectInstr =
        instructionsFromMethod(method)
        .collect {
          case x @ NewArray(op, _, dims)
            if op == Opcode.multianewarray && dims == 2 => x
        }
        .length > 0

      assert(hadCorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(method).mkString("\n"))
    }
  }

  @Test def arraysFromOfDim = {
    val source = """
                 |object Arr {
                 |  def arr1 = Array.ofDim[Int](2)
                 |  def arr2 = Array.ofDim[Unit](2)
                 |  def arr3 = Array.ofDim[String](2)
                 |  def arr4 = Array.ofDim[Map[String, String]](2)
                 |}""".stripMargin
    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val arr1       = getMethod(moduleNode, "arr1")
      val arr2       = getMethod(moduleNode, "arr2")
      val arr3       = getMethod(moduleNode, "arr3")

      val arr1CorrectInstr =
        instructionsFromMethod(arr1)
        .collect {
          case x @ IntOp(op, oprnd)
            if op == Opcode.newarray && oprnd == Opcode.int => x
        }
        .length > 0

      assert(arr1CorrectInstr,
             "Did not contain \"multianewarray\" instruction in:\n" +
             instructionsFromMethod(arr1).mkString("\n"))

      val arr2CorrectInstr =
        instructionsFromMethod(arr2)
        .collect {
          case x @ TypeOp(op, oprnd)
            if op == Opcode.anewarray && oprnd == Opcode.boxedUnit => x
        }
        .length > 0

      assert(arr2CorrectInstr,
             "arr2 bytecode did not contain correct `anewarray` instruction:\n" +
             instructionsFromMethod(arr2)mkString("\n"))

      val arr3CorrectInstr =
        instructionsFromMethod(arr3)
        .collect {
          case x @ TypeOp(op, oprnd)
            if op == Opcode.anewarray && oprnd == Opcode.javaString => x
        }
        .length > 0

      assert(arr3CorrectInstr,
             "arr3 bytecode did not contain correct `anewarray` instruction:\n" +
             instructionsFromMethod(arr3).mkString("\n"))
    }
  }

  @Test def arraysFromDimAndFromNewEqual = {
    val source = """
                 |object Arr {
                 |  def arr1 = Array.ofDim[Int](2)
                 |  def arr2 = new Array[Int](2)
                 |}""".stripMargin

    checkBCode(source) { dir =>
      val moduleIn   = dir.lookupName("Arr$.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val arr1       = getMethod(moduleNode, "arr1")
      val arr2       = getMethod(moduleNode, "arr2")

      // First two instructions of `arr1` fetch the static reference to `Array`
      val instructions1 = instructionsFromMethod(arr1).drop(2)
      val instructions2 = instructionsFromMethod(arr2)

      assert(instructions1 == instructions2,
        "Creating arrays using `Array.ofDim[Int](2)` did not equal bytecode for `new Array[Int](2)`\n" +
        diffInstructions(instructions1, instructions2))
    }
  }

  /** Verifies that arrays are not unnecessarily wrapped when passed to Java varargs methods */
  @Test def dontWrapArraysInJavaVarargs = {
    val source =
      """
        |import java.nio.file._
        |class Test {
        |  def test(xs: Array[String]) = {
        |     val p4 = Paths.get("Hello", xs: _*)
        |  }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val arrayWrapped = instructionsFromMethod(method).exists {
        case inv: Invoke => inv.name.contains("wrapRefArray")
        case _ => false
      }

      assert(!arrayWrapped, "Arrays should not be wrapped when passed to a Java varargs method\n")
    }
  }

  @Test def efficientTryCases = {
    val source =
      """
        |class Test {
        |  def test =
        |    try print("foo")
        |    catch {
        |      case _: scala.runtime.NonLocalReturnControl[_] => ()
        |    }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      val moduleIn = dir.lookupName("Test.class", directory = false)
      val moduleNode = loadClassNode(moduleIn.input)
      val method = getMethod(moduleNode, "test")

      val hasInstanceof = instructionsFromMethod(method).exists {
        case TypeOp(Opcodes.INSTANCEOF, _) => true
        case _ => false
      }

      assert(!hasInstanceof, "Try case should not issue INSTANCEOF opcode\n")
    }
  }

  @Test def noBoxingInSyntheticEquals = {
    val source =
      """
        |case class Case(x: Long)
        |class Value(val x: Long) extends AnyVal
      """.stripMargin

    checkBCode(source) { dir =>
      for ((clsName, methodName) <- List(("Case", "equals"), ("Value$", "equals$extension"))) {
        val moduleIn = dir.lookupName(s"$clsName.class", directory = false)
        val moduleNode = loadClassNode(moduleIn.input)
        val equalsMethod = getMethod(moduleNode, methodName)

        val callsEquals = instructionsFromMethod(equalsMethod).exists {
            case i @ Invoke(_, _, "equals", _, _) => true
            case i => false
        }

        assert(!callsEquals, s"equals method should not be called in the definition of $clsName#$methodName\n")
      }
    }
  }

  @Test def partialFunctions = {
    val source =
      """object Foo {
        |  def magic(x: Int) = x
        |  val foo: PartialFunction[Int, Int] = { case x => magic(x) }
        |}
      """.stripMargin

    checkBCode(source) { dir =>
      // We test that the anonymous class generated for the partial function
      // holds the method implementations and does not use forwarders
      val clsIn = dir.lookupName("Foo$$anon$1.class", directory = false).input
      val clsNode = loadClassNode(clsIn)
      val applyOrElse = getMethod(clsNode, "applyOrElse")
      val instructions = instructionsFromMethod(applyOrElse)
      val callMagic = instructions.exists {
        case Invoke(_, _, "magic", _, _) => true
        case _ => false
      }
      assertTrue(callMagic)
    }
  }
}
