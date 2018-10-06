package net.tixxit.gulon

import scala.util.Random

final case class Vectors(matrix: Matrix, from: Int, until: Int) {
  def data: Array[Array[Float]] = matrix.data
  def dimension: Int = until - from
  def size: Int = matrix.rows
}

object Vectors {
  def apply(matrix: Matrix): Vectors = Vectors(matrix, 0, matrix.cols)

  def subvectors(matrix: Matrix, numSubvectors: Int): Seq[Vectors] =
    new Seq[Vectors] {
      def length: Int = (matrix.cols + numSubvectors - 1) / numSubvectors

      def iterator: Iterator[Vectors] =
        Iterator.tabulate(length)(apply(_))

      def apply(i: Int): Vectors = {
        val short = (length * numSubvectors) - matrix.cols
        val full = numSubvectors - short
        if (i < full) {
          val from = i * length
          Vectors(matrix, from, from + length)
        } else {
          val from = full * length + (i - full) * (length - 1)
          Vectors(matrix, from, from + length - 1)
        }
      }
    }
}

final class KMeans(
  val dimension: Int,
  offsets: Array[Float],
  val centroids: Array[Array[Float]]
) {
  final def k: Int = centroids.length

  private def assign(vecs: Vectors, assignments: Array[Int]): Unit = {
    val rng = new Random(0)
    val from = vecs.from
    val until = vecs.until
    val data = vecs.data
    var i = 0
    while (i < data.length) {
      val row = data(i)
      var k = 0
      var min = Float.MaxValue
      while (k < centroids.length) {
        val c = centroids(k)
        var j = 0
        var d = 0f
        val len = c.length
        while (j < len) {
          d += row(j + from) * c(j)
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

  def iterate(vecs: Vectors, iters: Int): KMeans = {
    val assignments = new Array[Int](vecs.size)
    (0 until iters).foldLeft(this) { (kmeans, i) =>
      kmeans.assign(vecs, assignments)
      KMeans.fromAssignment(centroids.length, dimension, vecs, assignments)
    }
  }
}

object KMeans {
  def compute(k: Int, vecs: Vectors, n: Int, seed: Int = 0): Matrix =
    Matrix(k, vecs.dimension, KMeans.init(k, vecs, seed).iterate(vecs, n).centroids)

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

  def init(k: Int, vecs: Vectors, seed: Int = 0): KMeans = {
    val rng = new Random(seed)
    val centroids = Array.fill(k) {
      val i = rng.nextInt(vecs.data.length)
      val row = vecs.data(i)
      java.util.Arrays.copyOfRange(row, vecs.from, vecs.until)
    }
    KMeans(vecs.dimension, centroids)
  }

  private def fromAssignment(k: Int, dimension: Int, vecs: Vectors, assignments: Array[Int]): KMeans = {
    val centroids: Array[Array[Float]] = new Array[Array[Float]](k)
    val counts: Array[Int] = new Array[Int](k)
    var i = 0
    while (i < centroids.length) {
      centroids(i) = new Array[Float](dimension)
      i += 1
    }
    // Calculate the means incrementally.
    val data = vecs.data
    val from = vecs.from
    val until = vecs.until
    i = 0
    while (i < data.length) {
      val v = data(i)
      val k = assignments(i)
      val c = centroids(k)
      val n = counts(k) + 1
      var j = 0
      while (j < c.length) {
        val p = c(j)
        c(j) = p + ((v(j + from) - p) / n)
        j += 1
      }
      counts(k) = n
      i += 1
    }
    KMeans(dimension, centroids)
  }
}
