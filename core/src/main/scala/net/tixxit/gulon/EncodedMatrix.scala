package net.tixxit.gulon

import com.google.protobuf.ByteString

trait EncodedVector {
  def length: Int
  def apply(i: Int): Int
}

sealed trait EncodedMatrix {
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
