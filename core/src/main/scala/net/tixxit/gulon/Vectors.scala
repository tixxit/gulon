package net.tixxit.gulon

final case class Vectors(matrix: Matrix, from: Int, until: Int) {
  def data: Array[Array[Float]] = matrix.data
  def dimension: Int = until - from
  def size: Int = matrix.rows

  def slice(): Matrix = {
    val len = until - from
    val offset = from
    val s = new Array[Array[Float]](data.length)
    var i = 0
    while (i < s.length) {
      val row = data(i)
      val sr = new Array[Float](len)
      var j = 0
      while (j < len) {
        sr(j) = row(j + offset)
        j += 1
      }
      s(i) = sr
      i += 1
    }
    Matrix(size, dimension, s)
  }

  override def hashCode: Int =
    (0 until size).map { i =>
      val row = data(i)
      Vector.range(from, until).map(row(_))
    }.hashCode * 31

  override def equals(that: Any): Boolean = that match {
    case (that: Vectors) if this.size == that.size && this.dimension == that.dimension =>
      var i = 0
      val lhs = this.data
      val lOffset = this.from
      val rhs = that.data
      val rOffset = that.from
      val rows = size
      val cols = dimension
      while (i < rows) {
        val lrow = lhs(i)
        val rrow = rhs(i)
        var j = 0
        while (j < cols) {
          if (lrow(j + lOffset) != rrow(j + rOffset))
            return false
          j += 1
        }
        i += 1
      }
      true
    case _ => false
  }
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
