package net.tixxit.gulon
package command

import java.io.File
import java.lang.Integer
import java.nio.file.Path

import cats.data.Validated
import cats.effect.{ContextShift, ExitCode, IO}
import cats.implicits._
import com.google.protobuf.ByteString
import com.monovore.decline._

object BuildIndex {
  case class Config(
    numClusters: Int,
    numQuantizers: Int,
    maxIterations: Int,
    output: Path,
    input: Path)

  object Config {
    val numClusters = Opts.option[Int](
      "clusters", short="k", metavar="num_clusters",
      help="clusters per quantizer, between 1 and 65536")
        .validate("clusters must be at least 1")(_ > 0)
        .validate("too many clusters, must be at most 65536")(_ <= 65536)
        .withDefault(256)

    val numQuantizers = Opts.option[Int]("quantizers", short="m", metavar="num",
                                         help="number of quantizers used").withDefault(25)
    val maxIterations = Opts.option[Int]("max-iters", short="n", metavar="iterations",
                                         help="maximum number of iterations per quantizer").withDefault(100)
    val outputPath = Opts.option[Path]("output", short="o", metavar="file",
                                       help="index output file")
    val inputPath = Opts.argument[Path](metavar="file")
  }

  private def logReadProgress(report: WordVectors.ProgressReport): IO[Unit] =
    CommandUtils.logProgress(report.percentageRead, f"charsPerWord=${report.charsPerWord}%.1f memUsed=${CommandUtils.formatBytes(report.sizeEstimate)}")

  private def logProductQuantizer(report: ProductQuantizer.ProgressReport): IO[Unit] = {
    val p = report.completedIterations.toFloat / report.totalIterations
    CommandUtils.logProgress(p, s"iters=${report.completedIterations}/${report.totalIterations} step=${report.stepSize.mean} stdDev=${report.stepSize.stdDev}")
  }

  private def buildIndex(quantizer: ProductQuantizer, encoded: EncodedMatrix): index.Index = {
    val quantizers = quantizer.quantizers.map { 
      case ProductQuantizer.Quantizer(_, clusters) =>
        index.Quantizer(clusters.dimension, clusters.centroids.map(index.Vector(_)))
    }
    index.Index(dimension = quantizer.dimension,
                normalize = false,
                productQuantizer = index.ProductQuantizer(quantizer.coderFactory.width, dimension = quantizer.dimension, quantizers = quantizers),
                length = encoded.length,
                codes = encoded.unwrappedEncodings.map(ByteString.copyFrom(_)))
  }

  def run(implicit contextShift: ContextShift[IO]): Opts[IO[ExitCode]] = {
    import Config._

    (numClusters, numQuantizers, maxIterations, outputPath, inputPath)
      .mapN(Config(_, _, _, _, _))
      .map { config =>
        val pqConfig = ProductQuantizer.Config(numClusters = config.numClusters,
                                               numQuantizers = config.numQuantizers,
                                               maxIterations = config.maxIterations,
                                               report = logProductQuantizer)
        for {
          _ <- IO.delay(println("Reading word vectors"))
          vecs <- WordVectors.readWord2VecFile(config.input.toFile, logReadProgress)
          _ <- IO.delay(println("\nComputing product quantizer"))
          quantizer <- ProductQuantizer(vecs.vectors, pqConfig)
          _ <- IO.delay(println(s"\nEncoding vectors"))
          encodedVectors <- quantizer.encode(vecs.vectors)
          _ <- IO.delay(println(s"Building index for ${vecs.vectors.rows} vectors"))
          index = buildIndex(quantizer, encodedVectors)
          _ <- IO.delay(println(s"Writing index to ${config.output}"))
          _ <- CommandUtils.writePath(config.output)(o => IO.delay(index.writeTo(o)))
        } yield ExitCode(0)
      }
  }

  def command(implicit contextShift: ContextShift[IO]): Command[IO[ExitCode]] =
    Command("build-index", "build a nearest neighbour index", true)(run)
}
