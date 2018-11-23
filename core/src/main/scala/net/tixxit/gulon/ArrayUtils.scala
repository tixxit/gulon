package net.tixxit.gulon

import java.lang.Object
import java.util.Arrays

object ArrayUtils {
  def deepHashCode[T](xs: Array[T]): Int =
    Arrays.deepHashCode(xs.asInstanceOf[Array[Object]])

  def deepEquals[T](xs: Array[T], ys: Array[T]): Boolean =
    Arrays.deepEquals(xs.asInstanceOf[Array[Object]], ys.asInstanceOf[Array[Object]])

  def shallowHashCode[T <: AnyRef](xs: Array[T]): Int =
    Arrays.hashCode(xs.asInstanceOf[Array[Object]])

  def shallowEquals[T <: AnyRef](xs: Array[T], ys: Array[T]): Boolean =
    Arrays.equals(xs.asInstanceOf[Array[Object]], ys.asInstanceOf[Array[Object]])

  def fill(n: Int, x: Float): Array[Float] = {
    val xs = new Array[Float](n)
    if (x == 0f) xs
    else {
      var i = 0
      while (i < xs.length) {
        xs(i) = x
        i += 1
      }
    }
    xs
  }

  def max(xs: Array[Float]): Float = {
    require(xs.length > 0, "empty array")
    var i = 0
    var mx = Float.NegativeInfinity
    while (i < xs.length) {
      mx = math.max(xs(i), mx)
      i += 1
    }
    mx
  }
}
