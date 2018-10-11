package net.tixxit.gulon

/**
 * The coding strategy for a product quantizer. It requires 2 key variables:
 *  - `width`: The bit width of the per-quantizer code. Each quantizer will
 *             have `2^width` clusters/codes. 
 *  - `length`: The total number of quantizers in the product quantizer.
 *
 * Thus, a product quantizer will produce codes of roughly `width * length +
 * K` bites, where `K` is some constant.
 */
trait Coder {

  /** Codes are opaque and dependent upon the coder itself. */
  type Code

  def width: Int
  def length: Int
  def wrapCode(encoded: Array[Byte]): Code
  def unwrapCode(code: Code): Array[Byte]
  def buildCode(indices: Array[Int]): Code
  def getIndex(code: Code, i: Int): Int
}

object Coder {

  /**
   * Construct a `Coder` for the given bit width and length. Supported 
   * widths are limited to 2, 4, 8, 10, 12, 16.
   *
   * @param width the bit-width of each quantizer's code
   * @param length the total number of quantizers
   */
  def apply(width: Int, length: Int): Option[Coder] = width match {
    case 2 => Some(Coder2(length))
    case 4 => Some(Coder4(length))
    case 8 => Some(Coder8(length))
    case 10 => Some(BytePlus(Coder2(length)))
    case 12 => Some(BytePlus(Coder4(length)))
    case 16 => Some(BytePlus(Coder8(length)))
    case _ => None
  }

  private sealed trait CoderImpl extends Coder {
    type Code = Array[Byte]
    def wrapCode(encoded: Array[Byte]): Code = encoded
    def unwrapCode(code: Code): Array[Byte] = code
  }

  private abstract class BytePackedCoder(val width: Int) extends CoderImpl {
    val codesPerByte: Int = 8 / width
    val bytesPerCode: Int = (length + codesPerByte - 1) / codesPerByte

    def buildCode(indices: Array[Int]): Code = {
      val code = new Array[Byte](bytesPerCode)
      buildCodeWithOffset(code, indices, 0)
      code
    }

    def getIndex(code: Code, i: Int): Int =
      getIndexWithOffset(code, 0, i)

    def length: Int
    def buildCodeWithOffset(code: Array[Byte], indices: Array[Int], offset: Int): Unit
    def getIndexWithOffset(bytes: Array[Byte], offset: Int, i: Int): Int
  }

  private case class Coder2(length: Int) extends BytePackedCoder(2) {
    def buildCodeWithOffset(code: Array[Byte], indices: Array[Int], offset: Int): Unit = {
      var i = 0
      while (i < indices.length) {
        val id = indices(i) & 0x3
        val j = i >>> 2
        code(j) = (code(j) | (id << ((i & 0x3) * 2))).toByte
        i += 1
      }
    }

    def getIndexWithOffset(bytes: Array[Byte], offset: Int, i: Int): Int =
      (bytes(offset + (i >>> 2)) >>> ((i & 0x3) * 2)) & 0x3
  }

  private case class Coder4(length: Int) extends BytePackedCoder(4) {
    def buildCodeWithOffset(code: Array[Byte], indices: Array[Int], offset: Int): Unit = {
      var i = 0
      while (i < indices.length) {
        val id = indices(i) & 0xF
        val j = i >>> 1
        code(j) = (code(j) | (id << ((i & 0x1) * 4))).toByte
        i += 1
      }
    }

    def getIndexWithOffset(bytes: Array[Byte], offset: Int, i: Int): Int =
      (bytes(offset + (i >>> 1)) >>> ((i & 0x1) * 4)) & 0xF
  }

  private case class Coder8(length: Int) extends BytePackedCoder(8) {
    def buildCodeWithOffset(code: Array[Byte], indices: Array[Int], offset: Int): Unit = {
      var i = 0
      while (i < indices.length) {
        code(i) = indices(i).toByte
        i += 1
      }
    }

    def getIndexWithOffset(bytes: Array[Byte], offset: Int, i: Int): Int =
      bytes(offset + i) & 0xFF
  }

  private case class BytePlus(lsb: BytePackedCoder) extends CoderImpl {
    val length: Int = lsb.length

    val width: Int = lsb.width + 8

    def buildCode(indices: Array[Int]): Code = {
      val code = new Array[Byte](length + lsb.bytesPerCode)
      var i = 0
      val len = length
      while (i < len) {
        code(i) = (indices(i) >>> lsb.width).toByte
        i += 1
      }
      lsb.buildCodeWithOffset(code, indices, len)
      code
    }

    def getIndex(code: Code, i: Int): Int = {
      val b1 = (code(i) & 0xFF) << lsb.width
      val b0 = lsb.getIndexWithOffset(code, length, i) & 0xFF
      b1 | b0
    }
  }
}
