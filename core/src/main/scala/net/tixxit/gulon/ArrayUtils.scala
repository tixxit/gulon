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
}
