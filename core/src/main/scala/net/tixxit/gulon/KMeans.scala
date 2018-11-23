package net.tixxit.gulon

import java.util.Arrays
import scala.concurrent.ExecutionContext
import scala.util.Random

import cats.Monad
import cats.effect.{ContextShift, IO}
import cats.implicits._

final class KMeans private (
  val dimension: Int,
  val centroids: Array[Array[Float]]
) {
  private[this] val offsets: Array[Float] = KMeans.calculateOffsets(centroids)

  final def k: Int = centroids.length

  // Need to know:
  //   Distance to current centroid
  //   Minimum distance to all other centroids
  //   After each reclustering:
  //     - get deltas of centroid movement
  //     - decrease min distance by how delta of closest centroid + max of all other deltas
  //     - only check all centroids if membership definitely changed

  def assign(vecs: Vectors): Array[Int] = {
    val assignments = new Array[Int](vecs.size)
    assign(vecs, assignments)
    assignments
  }

  private def assign(vecs: Vectors,
                     assignments: Array[Int],
                     start: Int,
                     end: Int): Unit = {
    val rng = new Random(0)
    val from = vecs.from
    val until = vecs.until
    val data = vecs.data
    var i = start
    while (i < end) {
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

  def parAssign(vecs: Vectors)(implicit contextShift: ContextShift[IO]): IO[Array[Int]] = {
    val batchSize = 25000
    val assignments = new Array[Int](vecs.size)
    List.range(0, assignments.length, batchSize)
      .parTraverse { from =>
        val until = math.min(assignments.length, from + batchSize)
        IO.shift.map { _ =>
          assign(vecs, assignments, from, until)
        }
      }
      .as(assignments)
  }

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

  override def hashCode: Int =
    (getClass, dimension, ArrayUtils.deepHashCode(centroids)).hashCode

  override def equals(that: Any): Boolean = that match {
    case (that: KMeans) if k == that.k =>
      dimension == that.dimension && ArrayUtils.deepEquals(centroids, that.centroids)
    case _ => false
  }
}

object KMeans {
  case class ProgressReport(numIterations: Int,
                            maxIterations: Int,
                            stepSize: SummaryStats,
                            converged: Boolean)

  object ProgressReport {
    def init(maxIterations: Int): ProgressReport =
      ProgressReport(0, maxIterations, SummaryStats.zero, false)
  }

  case class Config(numClusters: Int,
                    maxIterations: Int,
                    seed: Int = 0,
                    report: ProgressReport => IO[Unit] = _ => IO.pure(()))

  def computeClusters(vecs: Vectors, config: Config)(implicit contextShift: ContextShift[IO]): IO[KMeans] =
    Monad[IO].tailRecM(Option.empty[(KMeans, Array[Int], Int)]) {
      case None =>
        for {
          _ <- IO.shift
          init <- IO.delay(KMeans.init(config.numClusters, vecs, config.seed))
          assignments <- init.parAssign(vecs)
          report = ProgressReport(0, config.maxIterations, SummaryStats.zero, false)
          _ <- config.report(report)
        } yield Left(Some((init, assignments, 0)))
      case Some((prev, prevAssignments, i)) if i <= config.maxIterations =>
        for {
          _ <- IO.shift
          next <- IO.delay(KMeans.fromAssignment(config.numClusters, vecs.dimension, vecs, prevAssignments))
          assignments <- next.parAssign(vecs)
          converged = Arrays.equals(prevAssignments, assignments)
          report = ProgressReport(i, config.maxIterations, stepSize(prev.centroids, next.centroids), converged)
          _ <- config.report(report)
          i0 = if (converged) config.maxIterations + 1
               else i + 1
        } yield Left(Some((next, assignments, i0)))
      case Some((last, _, i)) =>
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

  final def calculateOffsets(centroids: Array[Array[Float]]): Array[Float] = {
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
    offsets
  }

  final def apply(dimension: Int, centroids: Array[Array[Float]]): KMeans =
    new KMeans(dimension, centroids)

  private def initializeCentroids(vectors: Vectors, k: Int, seed: Int = 0): Array[Array[Float]] = {
    val rng = new Random(seed)
    Array.fill(k) {
      val i = rng.nextInt(vectors.data.length)
      val row = vectors.data(i)
      java.util.Arrays.copyOfRange(row, vectors.from, vectors.until)
    }
  }

  def init(k: Int, vecs: Vectors, seed: Int = 0): KMeans =
    KMeans(vecs.dimension, initializeCentroids(vecs, k, seed))

  final def fromAssignment(k: Int, dimension: Int, vecs: Vectors, assignments: Array[Int]): KMeans = {
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

  final def calculateCentroids(vectors: Vectors,
                               assignments: Array[Int],
                               k: Int): Array[Array[Float]] = {
    val dimension = vectors.dimension
    val centroids: Array[Array[Float]] = Array.fill(k)(new Array[Float](dimension))
    val counts: Array[Int] = new Array[Int](k)

    // Calculate the means incrementally.
    val data = vectors.data
    val from = vectors.from
    val until = vectors.until
    var i = 0
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
    centroids
  }

  private def assignChunk(vectors: Vectors,
                          start: Int, end: Int,
                          centroids: Array[Array[Float]],
                          assignments: Array[Int],
                          intraClusterDistance: Array[Float],
                          extraClusterDistance: Array[Float],
                          updatedDistance: Array[Boolean],
                          offsets: Array[Float]): Boolean = {
    val rng = new Random(start)
    val from = vectors.from
    val until = vectors.until
    val data = vectors.data
    var i = start
    var changed = false
    while (i < end) {
      val row = data(i)
      if (intraClusterDistance(i) >= extraClusterDistance(i)) {
        var k = 0
        var extraMin = Float.MaxValue
        var intraMin = Float.MaxValue
        var cluster = -1
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
          if (d < intraMin || (d == intraMin && rng.nextBoolean())) {
            extraMin = intraMin
            intraMin = d
            cluster = k
          } else if (d < extraMin) {
            extraMin = d
          }
          k += 1
        }
        changed = changed || assignments(i) != cluster
        assignments(i) = cluster
        intraClusterDistance(i) = intraMin
        extraClusterDistance(i) = extraMin
        updatedDistance(i) = true
      }
      i += 1
    }
    changed
  }

  private[this] val BatchSize = 25000

  private def calculateDeltas(xs: Array[Array[Float]], ys: Array[Array[Float]]): Array[Float] = {
    require(xs.length == ys.length)
    val deltas = new Array[Float](xs.length)
    var i = 0
    while (i < xs.length) {
      deltas(i) = MathUtils.distance(xs(i), ys(i))
      i += 1
    }
    deltas
  }

  def computeClusters2(vectors: Vectors, config: Config)(implicit contextShift: ContextShift[IO]): IO[(Clustering, Assignment)] = {
    val assignments = new Array[Int](vectors.size)
    val intraClusterDistance = ArrayUtils.fill(vectors.size, Float.PositiveInfinity)
    val extraClusterDistance = ArrayUtils.fill(vectors.size, Float.NegativeInfinity)
    config.report(ProgressReport(0, config.maxIterations, SummaryStats.zero, false))
      .flatMap { _ =>
        Monad[IO].tailRecM((initializeCentroids(vectors, config.numClusters, config.seed), 0)) {
          case (centroids, config.maxIterations) =>
              IO.pure(Right((Clustering(centroids), Assignment(assignments))))
          case (centroids, iters) =>
            assign(vectors, centroids, assignments, intraClusterDistance, extraClusterDistance)
              .flatMap {
                case Some((Clustering(nextCentroids), stepSize)) =>
                  config.report(ProgressReport(iters + 1, config.maxIterations, stepSize, false))
                    .as(Left((nextCentroids, iters + 1)))
                case None =>
                  config.report(ProgressReport(iters + 1, config.maxIterations, SummaryStats.zero, true))
                    .as(Right((Clustering(centroids), Assignment(assignments))))
              }
        }
      }
  }

  def assign(vectors: Vectors,
             centroids: Array[Array[Float]],
             assignments: Array[Int],
             intraClusterDistance: Array[Float],
             extraClusterDistance: Array[Float])(implicit
             contextShift: ContextShift[IO]): IO[Option[(Clustering, SummaryStats)]] = {
    val offsets = calculateOffsets(centroids)
    val updatedDistance = new Array[Boolean](vectors.size)

    List.range(0, assignments.length, BatchSize)
      .parTraverse { start =>
        val end = math.min(assignments.length, start + BatchSize)
        IO.shift.map { _ =>
          assignChunk(vectors, start, end,
                      centroids, assignments,
                      intraClusterDistance, extraClusterDistance,
                      updatedDistance, offsets)
        }
      }
      .map { changes =>
        val stable = changes.foldLeft(false)(_ || _)
        if (stable) {
          None
        } else {
          val centroids0 = calculateCentroids(vectors, assignments, centroids.length)
          val deltas = calculateDeltas(centroids, centroids0)
          // TODO: We could calculate max deltas without each cluster in O(k) time.
          val maxDelta = ArrayUtils.max(deltas)
          var i = 0
          val len = updatedDistance.length
          while (i < len) {
            if (updatedDistance(i)) {
              val c = assignments(i)
              intraClusterDistance(i) = math.pow(math.sqrt(intraClusterDistance(i)) + deltas(c), 2).toFloat
              extraClusterDistance(i) = math.pow(math.sqrt(intraClusterDistance(i)) - maxDelta, 2).toFloat
            }
            i += 1
          }
          Some((Clustering(centroids0), SummaryStats.fromArray(deltas)))
        }
      }
  }
}
