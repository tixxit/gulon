package net.tixxit.gulon

case class Matrix(rows: Int, cols: Int, data: Array[Array[Float]]) {
  override def hashCode: Int =
    (classOf[Matrix], ArrayUtils.deepHashCode(data)).hashCode

  override def equals(that: Any): Boolean = that match {
    case (that: Matrix) => ArrayUtils.deepEquals(data, that.data)
    case _ => false
  }
}
