package dotty.tools
package dotc
package core
package tasty

import TastyFormat._
import collection.mutable
import TastyBuffer._
import core.Symbols.{Symbol, ClassSymbol}
import ast.tpd
import Decorators._

class TastyPickler(val rootCls: ClassSymbol) {

  private val sections = new mutable.ArrayBuffer[(NameRef, TastyBuffer)]

  val nameBuffer = new NameBuffer

  def newSection(name: String, buf: TastyBuffer) =
    sections += ((nameBuffer.nameIndex(name.toTermName), buf))

  def assembleParts(): Array[Byte] = {
    def lengthWithLength(buf: TastyBuffer) = {
      buf.assemble()
      buf.length + natSize(buf.length)
    }

    val uuidLow: Long = pjwHash64(nameBuffer.bytes)
    val uuidHi: Long = sections.iterator.map(x => pjwHash64(x._2.bytes)).fold(0L)(_ ^ _)

    val headerBuffer = {
      val buf = new TastyBuffer(header.length + 24)
      for (ch <- header) buf.writeByte(ch.toByte)
      buf.writeNat(MajorVersion)
      buf.writeNat(MinorVersion)
      buf.writeUncompressedLong(uuidLow)
      buf.writeUncompressedLong(uuidHi)
      buf
    }

    val totalSize =
      headerBuffer.length +
      lengthWithLength(nameBuffer) + {
        for ((nameRef, buf) <- sections) yield
          natSize(nameRef.index) + lengthWithLength(buf)
      }.sum
    val all = new TastyBuffer(totalSize)
    all.writeBytes(headerBuffer.bytes, headerBuffer.length)
    all.writeNat(nameBuffer.length)
    all.writeBytes(nameBuffer.bytes, nameBuffer.length)
    for ((nameRef, buf) <- sections) {
      all.writeNat(nameRef.index)
      all.writeNat(buf.length)
      all.writeBytes(buf.bytes, buf.length)
    }
    assert(all.length == totalSize && all.bytes.length == totalSize, s"totalSize = $totalSize, all.length = ${all.length}, all.bytes.length = ${all.bytes.length}")
    all.bytes
  }

  /** The address in the TASTY file of a given tree, or None if unknown.
   *  Note that trees are looked up by reference equality,
   *  so one can reliably use this function only directly after `pickler`.
   */
  var addrOfTree: tpd.Tree => Option[Addr] = (_ => None)

  /**
   * Addresses in TASTY file of symbols, stored by pickling.
   * Note that trees are checked for reference equality,
   * so one can reliably use this function only dirrectly after `pickler`
   */
  var addrOfSym: Symbol => Option[Addr] = (_ => None)

  val treePkl = new TreePickler(this)

  /** Returns a non-cryptographic 64-bit hash of the array.
   *
   *  from https://en.wikipedia.org/wiki/PJW_hash_function#Implementation
   */
  private def pjwHash64(data: Array[Byte]): Long = {
    var h = 0L
    var high = 0L
    var i = 0
    while (i < data.length) {
      h = (h << 4) + data(i)
      high = h & 0xF0000000L
      h ^= high >> 24
      h &= ~high
      i += 1
    }
    h
  }
}
