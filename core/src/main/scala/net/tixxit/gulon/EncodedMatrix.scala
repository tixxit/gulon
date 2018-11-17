package net.tixxit.gulon

import java.util.Arrays
import com.google.protobuf.ByteString

trait EncodedVector {
  def length: Int
  def apply(i: Int): Int
}

sealed abstract class EncodedMatrix {
  val coder: Coder
  def encodings: Vector[coder.Code]

  def length: Int = coder.length
  def apply(i: Int): EncodedVector =
    new EncodedVector {
      def length: Int = encodings.length
      def apply(j: Int): Int = coder.getIndex(encodings(j), i)
    }

  def unwrappedEncodings: Vector[Array[Byte]] =
    encodings.map(coder.unwrapCode(_))

  override def hashCode: Int =
    (classOf[EncodedMatrix], coder, unwrappedEncodings.map(Arrays.hashCode(_))).hashCode
  override def equals(that: Any): Boolean = that match {
    case (that: EncodedMatrix) =>
      coder == that.coder &&
        unwrappedEncodings.zip(that.unwrappedEncodings).forall {
          case (x, y) => Arrays.equals(x, y)
        }
    case _ => false
  }
}

object EncodedMatrix {
  def toProtobuf(m: EncodedMatrix): protobuf.EncodedMatrix =
    protobuf.EncodedMatrix(m.coder.width,
                        m.coder.length,
                        m.unwrappedEncodings.map(ByteString.copyFrom(_)))

  def fromProtobuf(m: protobuf.EncodedMatrix): EncodedMatrix = {
    val coder = Coder(m.codeWidth, m.length)
    val encodings = m.encodings.iterator
      .map { byteString =>
        coder.wrapCode(byteString.toByteArray())
      }
      .toVector
    EncodedMatrix(coder)(encodings)
  }

  def apply(coder0: Coder)(encodings0: Vector[coder0.Code]): EncodedMatrix =
    new EncodedMatrix {
      final val coder: coder0.type = coder0
      def encodings = encodings0
    }
}
