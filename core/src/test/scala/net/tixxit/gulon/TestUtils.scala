package net.tixxit.gulon

object TestUtils {
  def nearlyEqual(x: Float, y: Float, eps: Float = 0.0001f): Boolean = {
    val err = eps * math.max(math.abs(x), math.abs(y))
    ((x - err) <= y) && ((x + err) >= y)
  }

  def nearlyEqualArrays(xs: Array[Float], ys: Array[Float], eps: Float = 0.001f): Boolean =
    (xs.length == ys.length) && xs.zip(ys).forall { case (x, y) => nearlyEqual(x, y, eps) }

  def nearlyEqualMatrices(x: Matrix, y: Matrix, eps: Float = 0.001f): Boolean =
    x.data.zip(y.data).forall { case (lhs, rhs) =>
      lhs.zip(rhs).forall { case (x, y) =>
        nearlyEqual(x, y, eps)
      }
    }
}
