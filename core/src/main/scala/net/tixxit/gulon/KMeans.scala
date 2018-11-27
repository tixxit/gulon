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

  private def calculateOffsets(rows: Array[Array[Float]], from: Int, until: Int): Array[Float] = {
    val offsets = new Array[Float](rows.length)
    val len = until - from
    var i = 0
    while (i < rows.length) {
      offsets(i) = distSq(rows(i), from, until)
      i += 1
    }
    offsets
  }

  private def calculateOffsets(rows: Array[Array[Float]]): Array[Float] =
    if (rows.length == 0) new Array[Float](0)
    else calculateOffsets(rows, 0, rows(0).length)


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

  private def dot(lhs: Array[Float], rhs: Array[Float], rStart: Int): Float = {
    var d = 0f
    var i = 0
    while (i < lhs.length) {
      d += lhs(i) * rhs(i + rStart)
      i += 1
    }
    d
  }

  // Choose an initial set of centroids for the k-means algorithm.  This is a
  // pretty basic implementation of KMeans++. It chooses the initial point at
  // random. Each successive iteration chooses the next centroid by selecting
  // from the points randomly, but weighted by the squared distance to their
  // nearest centroid. Thus, the initial points chosen are generally spaced out
  // fairly well.
  //
  // TODO: Parallelize this.
  private def initializeCentroids2(vectors: Vectors,
                                   vectorOffsets: Array[Float],
                                   k: Int,
                                   seed: Int = 0): (Clustering, Assignment) = {
    require(k > 0, s"expected k > 0: k=$k")

    val data = vectors.data
    val from = vectors.from
    val until = vectors.until
    val rng = new Random(seed)
    val centroids = new Array[Array[Float]](k)
    val distances = ArrayUtils.fill(vectors.size, Float.PositiveInfinity)
    val assignments = new Array[Int](vectors.size)

    var centroid = vectors.data(rng.nextInt(vectors.data.length))
    var c = 0
    while (c < k) {
      centroids(c) = centroid
      val offset = distSq(centroid, 0, centroid.length)
      var i = 0
      var totalWeight = 0f
      var maxVector = -1
      while (i < data.length) {
        val row = data(i)
        var d = distances(i)
        val d0 = offset - 2 * dot(centroid, row, from) + vectorOffsets(i)
        if (d0 < d) {
          d = d0
          distances(i) = d
          assignments(i) = c
        }
        val threshold = totalWeight
        totalWeight += d
        if ((rng.nextDouble() * totalWeight) > threshold) {
          maxVector = i
        }
        i += 1
      }
      centroid = Arrays.copyOfRange(data(maxVector), from, until)
      c += 1
    }

    val centroids0 = calculateCentroids(vectors, assignments, k)
    (Clustering(centroids0), Assignment(assignments))
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

  final case class CentroidDistances private (distances: Array[Array[Float]]) {
    @inline
    def apply(i: Int, j: Int): Float =
      math.max(distances(i)(j % CentroidDistances.MaxGroups),
               distances(j)(i % CentroidDistances.MaxGroups))
  }

  object CentroidDistances {
    @inline
    final val MaxGroups = 128

    // Calculate distances between each centroid and the sets of centroid groups.
    def fromCentroids(centroids: Array[Array[Float]]): CentroidDistances = {
      val distances = new Array[Array[Float]](centroids.length)
      val numGroups = math.min(MaxGroups, centroids.length)
      var i = 0
      while (i < centroids.length) {
        val centroid = centroids(i)
        val centroidDistances = ArrayUtils.fill(numGroups, Float.PositiveInfinity)
        var j = 0
        while (j < centroids.length) {
          val group = j % MaxGroups
          val d = MathUtils.distance(centroid, centroids(j))
          centroidDistances(group) = math.min(centroidDistances(group), d)
          j += 1
        }
        distances(i) = centroidDistances
        i += 1
      }
      CentroidDistances(distances)
    }
  }

  // This is the core K-Means logic. We work in chunks so that we can
  // parallelize single runs across multiple CPU cores. This is incredibly
  // stateful. This code modifies many of the input parameters provided,
  // notably `assignments`, `intraClusterDistances` and
  // `extraClusterDistances`. I've tried to keep it well commented, since the
  // vast majority of it is pretty obtuse/completely unobvious.
  //
  // Overall, the overarching goal of an efficient k-means algorithm is to
  // reduce the number of distance calculations, since we would otherwise be
  // performing n * k (!!) of them. This is balanced by the fact that almost
  // all worthwhile heuristics to skip distance calculations require more
  // storage. For small values of k and/or n, its mostly fine. However, Gulon
  // is often used with large values for both, so we can't just arbitrary store
  // all sorts of values in memory. In general, we assume that k << n and O(n)
  // extra storage, where the constant is fairly small`, is OK. To this end,
  // you'll see we often "group" centroids together into a constant number of
  // groups so that we can avoid adding a factor of `k` to our storage. It's a
  // bit messy, but the alternative is running out of heap for most practical
  // word embeddings.
  private def assignChunk(vectors: Vectors,
                          vectorOffsets: Array[Float],
                          start: Int, end: Int,
                          centroids: Array[Array[Float]],
                          centroidDistances: CentroidDistances,
                          assignments: Array[Int],
                          intraClusterDistance: Array[Float],
                          extraClusterDistance: Array[Float],
                          offsets: Array[Float]): Boolean = {
    val rng = new Random(start)
    val distances = new Array[Float](centroids.length)
    val from = vectors.from
    val until = vectors.until
    val data = vectors.data
    var i = start
    var changed = false
    while (i < end) {
      val row = data(i)
      // Upper-bound on distance from point i to the centroid currently
      // assigned to i.
      var intraMin = intraClusterDistance(i)
      // Lower-bound on distance from point i to the next nearest centroid that
      // isn't assigned to i.
      var extraMin = extraClusterDistance(i)
      // If intraMin < extraMin, then it is not possible for the point to
      // change clusters, so we can skip ALL distance calculations. This
      // happens quite a lot when the cluster gets close to converging.
      if (intraMin >= extraMin) {
        val vOffset = vectorOffsets(i)
        var cluster = assignments(i)
        // Exact squared distance to previously assigned centroid.
        intraMin = offsets(cluster) - 2 * dot(centroids(cluster), row, from) + vOffset
        // After calculating intraMin exactly, without the approximation used
        // in `assign`, we get a second chance to skip iterating over the
        // centroids.
        if (extraMin < 0 || intraMin >= (extraMin * extraMin)) {
          distances(cluster) = intraMin
          var k = 0
          while (k < centroids.length) {
            if (cluster != k) {
              val c = centroids(k)
              // A lower-bound on the distance from the currently asisgned
              // `cluster` to cluster `k`.
              val clusterDist = centroidDistances(cluster, k)
              // If cluster `k` is more than twice as far from the currently
              // assigned `cluster` as point i, then it is impossible for point
              // i to get assigned to cluster `k`, so we can skip the distance
              // calculation. Since `intraMin` is the squared distance, we have
              // to adjust the math a bit.
              if (intraMin >= 0.25 * clusterDist * clusterDist) {
                val d = offsets(k) - 2 * dot(c, row, from) + vOffset
                if (d < intraMin || (d == intraMin && rng.nextBoolean())) {
                  intraMin = d
                  cluster = k
                }
                distances(k) = d
              } else {
                distances(k) = Float.PositiveInfinity
              }
            }
            k += 1
          }
          // We need to get a somewhat tight lower bound on extraMin, the
          // distance from point i to the the _next_ nearest cluster than the
          // one it is assigned. We do this by taking the minimum lower bound
          // to all other clusters. In some cases, we have the exact distance
          // calculated above, but in other cases, where we were able to skip
          // the distance calculation, we can still get an OK lower bound using
          // centroidDistances (see below).
          intraMin = math.sqrt(intraMin).toFloat
          extraMin = Float.PositiveInfinity
          k = 0
          while (k < centroids.length) {
            if (k != cluster) {
              val d = distances(k)
              val lowerBoundDist =
                if (d < Float.PositiveInfinity) {
                  d
                } else {
                  // We can get a worst-case lowerbound on the distance to a
                  // cluster using the centroid distances.
                  val lb = centroidDistances(cluster, k) - intraMin
                  lb * lb
                }
              extraMin = math.min(extraMin, lowerBoundDist)
            }
            k += 1
          }
          changed = changed || assignments(i) != cluster
          assignments(i) = cluster
          intraClusterDistance(i) = intraMin
          extraClusterDistance(i) = math.sqrt(extraMin).toFloat
        }
      }
      i += 1
    }
    changed
  }

  private def distSq(row: Array[Float], from: Int, until: Int): Float = {
    val len = until - from
    var sum = 0f
    var i = 0
    while (i < len) {
      val x = row(i + from)
      sum += (x * x)
      i += 1
    }
    sum
  }

  // Returns the euclidean distance between each element of `xs` and `ys`.
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

  // This is a single step/iteration of the K-Means algorithm. This will chunk
  // up the input then assign all points in `vectors` to the nearest centroid
  // in parallel. The bulk of the work is done in `assignChunk`, so look there
  // if you want some insight into the specifics of the algorithm.
  def assign(vectors: Vectors,
             vectorOffsets: Array[Float],
             centroids: Array[Array[Float]],
             assignments: Array[Int],
             intraClusterDistance: Array[Float],
             extraClusterDistance: Array[Float])(implicit
             contextShift: ContextShift[IO]): IO[Option[(Clustering, SummaryStats)]] = {
    val offsets = calculateOffsets(centroids)
    // Calculates lower-bounds on all pair-wise distances between the
    // centroids. This is actually a fairly heavy weight operation - we should
    // consider parallelizing it.
    val centroidDistances = CentroidDistances.fromCentroids(centroids)

    List.range(0, assignments.length, BatchSize)
      .parTraverse { start =>
        val end = math.min(assignments.length, start + BatchSize)
        IO.shift.map { _ =>
          assignChunk(vectors, vectorOffsets, start, end,
                      centroids, centroidDistances, assignments,
                      intraClusterDistance, extraClusterDistance,
                      offsets)
        }
      }
      .map { changes =>
        val stable = !changes.foldLeft(false)(_ || _)
        if (stable) {
          None
        } else {
          // Compute the centroids of the clusters as the mean of all points
          // assigned to the cluster.
          val centroids0 = calculateCentroids(vectors, assignments, centroids.length)
          // Calculates how much each centroid moved during this iteration. These
          // are used to update our lower/upper distance bounds. See below.
          val deltas = calculateDeltas(centroids, centroids0)
          val maxDelta = ArrayUtils.max(deltas)
          var i = 0
          val len = assignments.length
          while (i < len) {
            val c = assignments(i)
            // Each centroid can move, at most, deltas(c) away from any point
            // assigned to it. This gives us a good upper bound on the distance.
            intraClusterDistance(i) = intraClusterDistance(i) + deltas(c)
            // This lower bound has to drop by maxDelta - the worst case
            // scenario being that the 2nd closest centroid was the one that
            // moved the most. We don't keep enough information around to know
            // get any tighter bounds.
            extraClusterDistance(i) = extraClusterDistance(i) - maxDelta
            i += 1
          }
          Some((Clustering(centroids0), SummaryStats.fromArray(deltas)))
        }
      }
  }

  private[this] val BatchSize = 25000

  /**
   * Computes a clustering of `vectors`. This will return after either
   * `config.maxIterations` iterations have completed or if the clustering has
   * converged.
   *
   * To reduce runtime, this uses some extra space. Beyond storing the
   * centroids and assignments, this requires the following extra space:
   *
   * - 4 * k * math.min(k, 100) bytes for cachcing centroid-to-centroid distances
   * - 4 * n * math.min(k, 10) bytes for storing lower-bounds on point-to-centroid distances
   * - 4 * n bytes for storing upper bounds on point-to-assigned-centroid distances
   * - 4 * n bytes for storing pre-computed vector magnitudes to speed up distance computations
   *
   * Importantly, this stores only O(n) extra space, which does not scale
   * linearly with the number of clusters.
   */
  def computeClusters2(vectors: Vectors, config: Config)(implicit contextShift: ContextShift[IO]): IO[(Clustering, Assignment)] = {
    config.report(ProgressReport(0, config.maxIterations, SummaryStats.zero, false))
      .flatMap { _ =>
        val vectorOffsets = calculateOffsets(vectors.matrix.data, vectors.from, vectors.until)
        val (Clustering(centroids0), Assignment(assignments)) =
          initializeCentroids2(vectors, vectorOffsets, config.numClusters, config.seed)
        val intraClusterDistance = ArrayUtils.fill(vectors.size, Float.PositiveInfinity)
        val extraClusterDistance = ArrayUtils.fill(vectors.size, Float.NegativeInfinity)

        Monad[IO].tailRecM((centroids0, 0)) {
          case (centroids, config.maxIterations) =>
              IO.pure(Right((Clustering(centroids), Assignment(assignments))))
          case (centroids, iters) =>
            assign(vectors, vectorOffsets, centroids, assignments, intraClusterDistance, extraClusterDistance)
              .flatMap {
                case Some((Clustering(nextCentroids), stepSize)) =>
                  config.report(ProgressReport(iters + 1, config.maxIterations, stepSize, false))
                    .as(Left((nextCentroids, iters + 1)))
                case None =>
                  config.report(ProgressReport(iters + 1, iters + 1, SummaryStats.zero, true))
                    .as(Right((Clustering(centroids), Assignment(assignments))))
              }
        }
      }
  }
}
