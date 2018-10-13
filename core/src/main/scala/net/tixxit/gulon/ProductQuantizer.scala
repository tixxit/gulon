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

  def dimension: Int = quantizers.map(_.dimension).sum

  def encode(vectors: Matrix)(implicit contextShift: ContextShift[IO]): IO[EncodedMatrix] = {
    val coder = coderFactory(vectors.rows)
    val subvectors = Vectors.subvectors(vectors, quantizers.size)
    val assignments = new Array[Int](vectors.rows)
    val codes = quantizers.zip(subvectors)
      .parTraverse { case (q, v) =>
        IO.shift.map { _ =>
          q.clusters.assign(v, assignments)
          coder.buildCode(assignments)
        }
      }
    codes.map(EncodedMatrix(coder)(_))
  }
}

object ProductQuantizer {
  case class Quantizer(
    dimension: Int,
    from: Int,
    until: Int,
    clusters: KMeans)

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
            } yield Quantizer(vecs.dimension, vecs.from, vecs.until, clusters)
          }

        quantizers.map(ProductQuantizer(config.numClusters, _))
      }

  def apply(vectors: Matrix, config: Config)(implicit contextShift: ContextShift[IO]): IO[ProductQuantizer] = {
    val subvectors = Vectors.subvectors(vectors, config.numQuantizers)
    fromSubvectors(subvectors.toVector, config)
  }
}
