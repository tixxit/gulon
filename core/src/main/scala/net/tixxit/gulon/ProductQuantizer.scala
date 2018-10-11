package net.tixxit.gulon

import scala.concurrent.ExecutionContext

import cats.effect.IO
import cats.effect.concurrent.Ref
import cats.implicits._

case class ProductQuantizer(
  logK: Int,
  quantizers: Vector[ProductQuantizer.Quantizer]
) {
  def k: Int = 1 << logK
  def dimension: Int = quantizers.map(_.dimension).sum

  def encode(vectors: Matrix): EncodedMatrix = {
    val coder = Coder(logK, vectors.rows).get
    val subvectors = Vectors.subvectors(vectors, quantizers.size)
    val assignments = new Array[Int](vectors.rows)
    val codes = quantizers.zip(subvectors)
      .map { case (q, v) =>
        q.clusters.assign(v, assignments)
        coder.buildCode(assignments)
      }
    EncodedMatrix(coder)(codes)
  }
}

object ProductQuantizer {
  case class Quantizer(
    dimension: Int,
    from: Int,
    until: Int,
    clusters: KMeans)

  case class Config(
    logClusters: Int,
    numQuantizers: Int,
    maxIterations: Int,
    executionContext: ExecutionContext,
    report: ProgressReport => IO[Unit] = _ => IO.pure(()))

  case class ProgressReport(
    kMeansReports: Vector[KMeans.ProgressReport]
  ) {
    def completedIterations: Int = kMeansReports.map(_.numIterations).sum
    def totalIterations: Int = kMeansReports.map(_.maxIterations).sum
    def stepSize: SummaryStats = kMeansReports.map(_.stepSize).reduce(_ ++ _)
  }

  private def fromSubvectors(subvectors: Vector[Vectors], config: Config): IO[ProductQuantizer] =
    Ref[IO].of(Vector.fill(subvectors.size)(KMeans.ProgressReport.init(config.maxIterations)))
      .flatMap { reportsRef =>
        def updateReport(i: Int, report: KMeans.ProgressReport): IO[Vector[KMeans.ProgressReport]] =
          reportsRef.modify { rs =>
            val updated = rs.updated(i, report)
            (updated, updated)
          }

        implicit val contextShift = IO.contextShift(config.executionContext)

        val quantizers = subvectors.zipWithIndex
          .traverse { case (vecs, i) =>
            def makeReport(report: KMeans.ProgressReport): IO[Unit] = for {
              reports <- updateReport(i, report)
              _ <- config.report(ProgressReport(reports))
            } yield ()

            val numClusters = 1 << config.logClusters
            val kmeansConfig = KMeans.Config(numClusters = numClusters,
                                             maxIterations = config.maxIterations,
                                             seed = i,
                                             executionContext = config.executionContext,
                                             report = makeReport)
            IO.Par(for {
              _ <- IO.shift
              clusters <- KMeans.computeClusters(vecs, kmeansConfig)
            } yield Quantizer(vecs.dimension, vecs.from, vecs.until, clusters))
          }

        IO.Par.unwrap(quantizers).map(ProductQuantizer(config.logClusters, _))
      }

  def apply(vectors: Matrix, config: Config): IO[ProductQuantizer] = {
    val subvectors = Vectors.subvectors(vectors, config.numQuantizers)
    fromSubvectors(subvectors.toVector, config)
  }
}
