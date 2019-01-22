import java.io.{ByteArrayInputStream, ByteArrayOutputStream, ObjectInputStream, ObjectOutputStream, PrintWriter, StringWriter}
import java.lang.invoke.{MethodHandleInfo, SerializedLambda}

class C {
  val f1 = ((x1: Int,
    x2: String,
    x3: Int,
    x4: Int,
    x5: Int,
    x6: Int,
    x7: Int,
    x8: Int,
    x9: Int,
    x10: Int,
    x11: Int,
    x12: Int,
    x13: Int,
    x14: Int,
    x15: Int,
    x16: Int,
    x17: Int,
    x18: Int,
    x19: Int,
    x20: Int,
    x21: Int,
    x22: Int,
    x23: Int,
    x24: Int,
    x25: Int,
    x26: Int) => x2 + x1)
}

object Test {
  def main(args: Array[String]): Unit = {
    val c = new C
    serializeDeserialize(c.f1)
  }

  def serializeDeserialize[T <: AnyRef](obj: T) = {
    val buffer = new ByteArrayOutputStream
    val out = new ObjectOutputStream(buffer)
    out.writeObject(obj)
    val in = new ObjectInputStream(new ByteArrayInputStream(buffer.toByteArray))
    in.readObject.asInstanceOf[T]
  }
}

