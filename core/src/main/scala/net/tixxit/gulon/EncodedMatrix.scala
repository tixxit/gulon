package net.tixxit.gulon

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
      def apply(j: Int): Int = coder.getIndex(encodings(i), j)
    }

  def unwrappedEncodings: Vector[Array[Byte]] =
    encodings.map(coder.unwrapCode(_))
}

object EncodedMatrix {
  def apply(coder0: Coder)(encodings0: Vector[coder0.Code]): EncodedMatrix =
    new EncodedMatrix {
      final val coder: coder0.type = coder0
      def encodings = encodings0
    }
}
