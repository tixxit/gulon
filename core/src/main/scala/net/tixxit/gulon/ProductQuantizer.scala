package net.tixxit.gulon

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import cats.implicits._

case class ProductQuantizer(
  numClusters: Int,
  quantizers: Vector[ProductQuantizer.Quantizer]
) {
  val coderFactory: Coder.Factory = {
    val maxWidth = 32 - java.lang.Integer.numberOfLeadingZeros(numClusters - 1)
    Coder.factoryFor(maxWidth).getOrElse {
      throw new IllegalArgumentException(s"too many clusters: $numClusters")
    }
  }

  val dimension: Int = quantizers.map(_.dimension).sum

  /**
   * Encodes/compresses a high dimensional `Matrix` (rows are vectors) into a
   * small `EncodedMatrix`. This will perform the encoding in parallel using
   * the implicit `ContextShift[IO]`.
   */
  def encode(vectors: Matrix)(implicit contextShift: ContextShift[IO]): IO[EncodedMatrix] = {
    val coder = coderFactory(vectors.rows)
    val subvectors = Vectors.subvectors(vectors, quantizers.size)
    val codes = quantizers.zip(subvectors)
      .parTraverse { case (q, v) =>
        IO.shift.map { _ =>
          coder.buildCode(q.clusters.assign(v))
        }
      }
    codes.map(EncodedMatrix(coder)(_))
  }

  /**
   * Decodes a compressed/encoded matrix into an approximation of the original
   * matrix.
   */
  def decode(encoded: EncodedMatrix): Matrix = {
    val data = Array.fill(encoded.length)(new Array[Float](dimension))
    encoded.encodings.zipWithIndex.foreach { case (code, k) =>
      val q = quantizers(k)
      val offset = q.from
      var i = 0
      val codeLen = encoded.length
      while (i < codeLen) {
        val row = data(i)
        val centroidIndex = encoded.coder.getIndex(code, i)
        val centroid = q.clusters.centroids(centroidIndex)
        var j = 0
        while (j < centroid.length) {
          row(j + offset) = centroid(j)
          j += 1
        }
        i += 1
      }
    }
    Matrix(data.length, dimension, data)
  }
}

object ProductQuantizer {
  case class Quantizer(from: Int,
                       clusters: KMeans) {
    def dimension: Int = clusters.dimension
    def until: Int = from + dimension
  }

  def toProtobuf(pq: ProductQuantizer): protobuf.ProductQuantizer =
    protobuf.ProductQuantizer(
      pq.numClusters,
      pq.quantizers.map { case Quantizer(from, kmeans) =>
        val centroids = kmeans.centroids.map { xs =>
          protobuf.FloatVector(xs)
        }
        protobuf.ProductQuantizer.Quantizer(
          from, kmeans.dimension, centroids)
      })

  def fromProtobuf(pq: protobuf.ProductQuantizer): ProductQuantizer =
    ProductQuantizer(
      pq.numClusters,
      pq.quantizers.iterator.map {
        case protobuf.ProductQuantizer.Quantizer(from, dim, centroids) =>
          Quantizer(from, KMeans(dim, centroids.iterator.map(_.values).toArray))
      }.toVector)

  case class Config(
    numClusters: Int,
    numQuantizers: Int,
    maxIterations: Int,
    report: ProgressReport => IO[Unit] = _ => IO.pure(()))

  case class ProgressReport(
    kMeansReports: Vector[KMeans.ProgressReport]
  ) {
    def completedIterations: Int = kMeansReports.map(_.numIterations).sum
    def totalIterations: Int = kMeansReports.map(_.maxIterations).sum
    def stepSize: SummaryStats = kMeansReports.map(_.stepSize).reduce(_ ++ _)
  }

  private def fromSubvectors(subvectors: Vector[Vectors], config: Config)(implicit contextShift: ContextShift[IO]): IO[ProductQuantizer] =
    Ref[IO].of(Vector.fill(subvectors.size)(KMeans.ProgressReport.init(config.maxIterations)))
      .flatMap { reportsRef =>
        def updateReport(i: Int, report: KMeans.ProgressReport): IO[Vector[KMeans.ProgressReport]] =
          reportsRef.modify { rs =>
            val updated = rs.updated(i, report)
            (updated, updated)
          }

        val quantizers = subvectors.zipWithIndex
          .parTraverse { case (vecs, i) =>
            def makeReport(report: KMeans.ProgressReport): IO[Unit] = for {
              reports <- updateReport(i, report)
              _ <- config.report(ProgressReport(reports))
            } yield ()

            val kmeansConfig = KMeans.Config(numClusters = config.numClusters,
                                             maxIterations = config.maxIterations,
                                             seed = i,
                                             report = makeReport)
            for {
              _ <- IO.shift
              clusters <- KMeans.computeClusters(vecs, kmeansConfig)
            } yield Quantizer(vecs.from, clusters)
          }

        quantizers.map(ProductQuantizer(config.numClusters, _))
      }

  def apply(vectors: Matrix, config: Config)(implicit contextShift: ContextShift[IO]): IO[ProductQuantizer] = {
    val subvectors = Vectors.subvectors(vectors, config.numQuantizers)
    fromSubvectors(subvectors.toVector, config)
  }
}
