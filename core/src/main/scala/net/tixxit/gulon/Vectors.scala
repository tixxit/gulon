package net.tixxit.gulon

final case class Vectors(matrix: Matrix, from: Int, until: Int) {
  def data: Array[Array[Float]] = matrix.data
  def dimension: Int = until - from
  def size: Int = matrix.rows
}

object Vectors {
  def apply(matrix: Matrix): Vectors = Vectors(matrix, 0, matrix.cols)

  def subvectors(matrix: Matrix, numSubvectors: Int): Seq[Vectors] =
    new Seq[Vectors] {
      def length: Int = numSubvectors

      def iterator: Iterator[Vectors] =
        Iterator.tabulate(length)(apply(_))

      val idealSize: Int = (matrix.cols + numSubvectors - 1) / numSubvectors

      def apply(i: Int): Vectors = {
        val shortFall = (idealSize * numSubvectors) - matrix.cols
        val full = numSubvectors - shortFall
        if (i < full) {
          val from = i * idealSize
          Vectors(matrix, from, from + idealSize)
        } else {
          val from = full * idealSize + (i - full) * (idealSize - 1)
          Vectors(matrix, from, from + idealSize - 1)
        }
      }
    }
}
