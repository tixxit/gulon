package net.tixxit.gulon

import java.util.Arrays

/**
 * A wrapper around a set of centroids.
 */
final class Clustering private (val centroids: Array[Array[Float]]) {
  def size: Int = centroids.length

  override def hashCode: Int =
    (getClass, ArrayUtils.deepHashCode(centroids)).hashCode

  override def equals(that: Any): Boolean = that match {
    case (that: Clustering) =>
      ArrayUtils.deepEquals(centroids, that.centroids)
    case _ => false
  }
}

object Clustering {
  def apply(centroids: Array[Array[Float]]): Clustering =
    new Clustering(centroids)

  def unapply(clustering: Clustering): Some[Array[Array[Float]]] =
    Some(clustering.centroids)
}
