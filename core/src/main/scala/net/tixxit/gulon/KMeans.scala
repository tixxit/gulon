package net.tixxit.gulon

import scala.concurrent.ExecutionContext
import scala.util.Random

import cats.Monad
import cats.effect.IO
import cats.implicits._

final class KMeans(
  val dimension: Int,
  offsets: Array[Float],
  val centroids: Array[Array[Float]]
) {
  final def k: Int = centroids.length

  def assign(vecs: Vectors, assignments: Array[Int]): Unit = {
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
    (0 until iters).foldLeft(this) { (prev, i) =>
      prev.assign(vecs, assignments)
      KMeans.fromAssignment(centroids.length, dimension, vecs, assignments)
    }
  }
}

object KMeans {
  case class ProgressReport(numIterations: Int,
                            maxIterations: Int,
                            stepSize: SummaryStats)

  object ProgressReport {
    def init(maxIterations: Int): ProgressReport =
      ProgressReport(0, maxIterations, SummaryStats.zero)
  }

  case class Config(numClusters: Int,
                    maxIterations: Int,
                    seed: Int = 0,
                    executionContext: ExecutionContext,
                    report: ProgressReport => IO[Unit] = _ => IO.pure(()))

  def computeClusters(vecs: Vectors, config: Config): IO[KMeans] =
    Monad[IO].tailRecM(Option.empty[(KMeans, Int)]) {
      case None =>
        for {
          _ <- IO.shift(config.executionContext)
          init <- IO.delay(KMeans.init(config.numClusters, vecs, config.seed))
          report = ProgressReport(0, config.maxIterations, SummaryStats.zero)
          _ <- config.report(report)
        } yield Left(Some((init, 0)))
      case Some((prev, i)) if i <= config.maxIterations =>
        for {
          _ <- IO.shift(config.executionContext)
          next <- IO.delay(prev.iterate(vecs, 1))
          report = ProgressReport(i, config.maxIterations, stepSize(prev.centroids, next.centroids))
          _ <- config.report(report)
        } yield Left(Some((next, i + 1)))
      case Some((last, i)) =>
        IO.pure(Right(last))
    }

  // Calculates the average step size of the centroids between 2 iterations.
  private def stepSize(prevCentroids: Array[Array[Float]], nextCentroids: Array[Array[Float]]): SummaryStats = {
    var stats = SummaryStats.newBuilder()
    var i = 0
    while (i < prevCentroids.length) {
      stats.update(MathUtils.distance(prevCentroids(i), nextCentroids(i)))
      i += 1
    }
    stats.result()
  }

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
