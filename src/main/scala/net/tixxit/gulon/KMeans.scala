package net.tixxit.gulon

import scala.util.Random

final class KMeans(
  val dimension: Int,
  offsets: Array[Float],
  val centroids: Array[Array[Float]]
) {
  final def k: Int = centroids.length

  private def assign(vecs: Matrix, assignments: Array[Int]): Unit = {
    val rng = new Random(0)
    var i = 0
    val data = vecs.data
    while (i < data.length) {
      val row = data(i)
      var k = 0
      var min = Float.MaxValue
      while (k < centroids.length) {
        val c = centroids(k)
        var j = 0
        var d = 0f
        while (j < row.length) {
          d += row(j) * c(j)
          j += 1
        }
        d = offsets(k) - 2 * d
        if (d < min || (d == min && rng.nextBoolean())) {
          assignments(i) = k
          min = d
        }
        k += 1
      }
      i += 1
    }
  }

  def iterate(vecs: Matrix, iters: Int): KMeans = {
    val assignments = new Array[Int](vecs.rows)
    (0 until iters).foldLeft(this) { (kmeans, i) =>
      kmeans.assign(vecs, assignments)
      KMeans.fromAssignment(centroids.length, dimension, vecs, assignments)
    }
  }
}

object KMeans {
  def compute(k: Int, vecs: Matrix, n: Int, seed: Int = 0): Matrix =
    Matrix(k, vecs.cols, KMeans.init(k, vecs, seed).iterate(vecs, n).centroids)

  private def apply(dimension: Int, centroids: Array[Array[Float]]): KMeans = {
    val offsets = new Array[Float](centroids.length)
    var i = 0
    while (i < centroids.length) {
      val c = centroids(i)
      var j = 0
      var s = 0f
      while (j < c.length) {
        val x = c(j)
        s += x * x
        j += 1
      }
      offsets(i) = s
      i += 1
    }
    new KMeans(dimension, offsets, centroids)
  }

  def init(k: Int, vecs: Matrix, seed: Int = 0): KMeans = {
    val rng = new Random(seed)
    val centroids = Array.fill(k) {
      val i = rng.nextInt(vecs.data.length)
      val c = vecs.data(i)
      java.util.Arrays.copyOf(c, c.length)
    }
    KMeans(vecs.cols, centroids)
  }

  private def fromAssignment(k: Int, dimension: Int, vecs: Matrix, assignments: Array[Int]): KMeans = {
    val centroids: Array[Array[Float]] = new Array[Array[Float]](k)
    val counts: Array[Int] = new Array[Int](k)
    var i = 0
    while (i < centroids.length) {
      centroids(i) = new Array[Float](dimension)
      i += 1
    }
    // Calculate the means incrementally.
    i = 0
    val data = vecs.data
    while (i < data.length) {
      val v = data(i)
      val k = assignments(i)
      val c = centroids(k)
      val n = counts(k) + 1
      var j = 0
      while (j < c.length) {
        val p = c(j)
        c(j) = p + ((v(j) - p) / n)
        j += 1
      }
      counts(k) = n
      i += 1
    }
    KMeans(dimension, centroids)
  }
}
