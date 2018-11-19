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
    metric: Metric,
    numClusters: Int,
    numQuantizers: Int,
    maxIterations: Int,
    partitioned: Option[Config.Partitioned],
    output: Path,
    input: Path)

  object Config {
    case class Partitioned(
      numPartitions: Option[Int],
      limit: Option[Int])

    val numClusters = Opts.option[Int](
      "clusters", short="k", metavar="num_clusters",
      help="clusters per quantizer, between 1 and 65536")
        .validate("clusters must be at least 1")(_ > 0)
        .validate("too many clusters, must be at most 65536")(_ <= 65536)
        .withDefault(256)
    val metric = Opts.option[String]("metric", short="d", metavar="l2,cosine",
                                     help="distance metric to use")
      .mapValidated {
        case "l2" => Validated.valid(Metric.L2)
        case "cosine" => Validated.valid(Metric.Cosine)
        case unknownMetric => Validated.invalidNel(s"unsupported metric: $unknownMetric")
      }
    val numQuantizers = Opts.option[Int]("quantizers", short="m", metavar="num",
                                         help="number of quantizers used").withDefault(25)
    val maxIterations = Opts.option[Int]("max-iters", short="n", metavar="iterations",
                                         help="maximum number of iterations per quantizer").withDefault(100)
    val isPartitioned = Opts.flag("partitioned", short="p",
                                  help="enable faster queries by partitioning vectors").orFalse
    val numPartitions = Opts.option[Int]("partitions", metavar="num",
                                         help="set fixed number of partitions").orNone
    val limit = Opts.option[Int]("limit", short="l", metavar="num",
                                 help="number of partitions to search").orNone
    val partitioned = (isPartitioned, numPartitions, limit)
      .mapN((_, _, _))
      .mapValidated {
        case (true, np, l) =>
          Validated.valid(Some(Partitioned(np, l)))
        case (false, None, None) =>
          Validated.valid(None)
        case (false, _, _) =>
          Validated.invalidNel("--partitions and --limit are only applicable with --partitioned")
      }
    val outputPath = Opts.option[Path]("output", short="o", metavar="file",
                                       help="index output file")
    val inputPath = Opts.argument[Path](metavar="file")

    val opts = (metric, numClusters, numQuantizers, maxIterations, partitioned, outputPath, inputPath)
      .mapN(Config(_, _, _, _, _, _, _))
  }

  private def logReadProgress(report: WordVectors.ProgressReport): IO[Unit] =
    CommandUtils.logProgress(report.percentageRead, f"charsPerWord=${report.charsPerWord}%.1f memUsed=${CommandUtils.formatBytes(report.sizeEstimate)}")

  private def logProductQuantizer(report: ProductQuantizer.ProgressReport): IO[Unit] = {
    val p = report.completedIterations.toFloat / report.totalIterations
    CommandUtils.logProgress(p, s"iters=${report.completedIterations}/${report.totalIterations} step=${report.stepSize.mean} stdDev=${report.stepSize.stdDev}")
  }

  private def logPartitioner(report: KMeans.ProgressReport): IO[Unit] = {
    val p = report.numIterations.toFloat / report.maxIterations
    CommandUtils.logProgress(p, s"iters=${report.numIterations}/${report.maxIterations}")
  }

  def buildSublinearIndex(vecs: WordVectors,
                          metric: Metric,
                          partitions: Int,
                          strategy: Index.GroupedIndex.Strategy,
                          pqConfig: ProductQuantizer.Config)(implicit
                          contextShift: ContextShift[IO]): IO[Index] = for {
    _ <- IO.delay(println("\nComputing partitions"))
    coarseConfig = KMeans.Config(partitions, pqConfig.maxIterations, report = logPartitioner)
    clustering <- KMeans.computeClusters(Vectors(vecs.toMatrix), coarseConfig)
    _ <- IO.delay(println("\nRe-indexing word vectors"))
    grouped <- vecs.grouped(clustering)
    _ <- IO.delay(println("\nComputing product quantizer"))
    vectors = grouped.residuals
    quantizer <- ProductQuantizer(vectors, pqConfig)
    _ <- IO.delay(println(s"Building index for ${grouped.size} vectors"))
    index <- Index.grouped(grouped, quantizer, metric, strategy)
  } yield index

  def buildLinearIndex(vecs: WordVectors,
                       metric: Metric,
                       pqConfig: ProductQuantizer.Config)(implicit
                       contextShift: ContextShift[IO]): IO[Index] = for {
    _ <- IO.delay(println("\nRe-indexing word vectors"))
    sorted = vecs.sorted
    _ <- IO.delay(println("\nComputing product quantizer"))
    quantizer <- ProductQuantizer(sorted.toMatrix, pqConfig)
    _ <- IO.delay(println(s"Building index for ${sorted.size} vectors"))
    index <- Index.sorted(sorted, quantizer, metric)
  } yield index

  def buildIndex(vecs: WordVectors,
                 metric: Metric,
                 partitioned: Option[Config.Partitioned],
                 pqConfig: ProductQuantizer.Config)(implicit
                 contextShift: ContextShift[IO]): IO[Index] =
    partitioned match {
      case None =>
        buildLinearIndex(vecs, metric, pqConfig)(contextShift)
      case Some(Config.Partitioned(maybePartitions, maybeLimit)) =>
        val partitions = maybePartitions.getOrElse(vecs.size / 1000)
        val limit = maybeLimit.getOrElse(math.max((partitions * 0.05).toInt, 5))
        val strategy = Index.GroupedIndex.Strategy.LimitGroups(limit)
        buildSublinearIndex(vecs, metric, partitions, strategy, pqConfig)(contextShift)
    }

  def run(implicit contextShift: ContextShift[IO]): Opts[IO[ExitCode]] = {
    Config.opts.map { config =>
      val pqConfig = ProductQuantizer.Config(numClusters = config.numClusters,
                                             numQuantizers = config.numQuantizers,
                                             maxIterations = config.maxIterations,
                                             report = logProductQuantizer)
      for {
        _ <- IO.delay(println("Reading word vectors"))
        vecs <- WordVectors.readWord2VecFile(config.input.toFile, config.metric.normalized, logReadProgress)
        index <- buildIndex(vecs, config.metric, config.partitioned, pqConfig)
        _ <- IO.delay(println(s"Writing index to ${config.output}"))
        _ <- CommandUtils.writePath(config.output)(o => IO.delay(Index.toProtobuf(index).writeTo(o)))
      } yield ExitCode(0)
    }
  }

  def command(implicit contextShift: ContextShift[IO]): Command[IO[ExitCode]] =
    Command("build-index", "build a nearest neighbour index", true)(run)
}
