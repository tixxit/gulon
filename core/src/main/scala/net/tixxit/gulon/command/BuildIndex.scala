package net.tixxit.gulon
package command

import java.io.File
import java.lang.Integer
import java.nio.file.Path

import cats.data.Validated
import cats.effect.{Clock, ContextShift, ExitCode, IO}
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

  def buildSublinearIndex(vecs: WordVectors,
                          metric: Metric,
                          partitions: Int,
                          strategy: Index.GroupedIndex.Strategy,
                          pqConfig: ProductQuantizer.Config)(implicit
                          contextShift: ContextShift[IO], clock: Clock[IO]): IO[Index] = for {
    clustering <- CommandUtils.computePartitions(vecs, partitions, pqConfig.maxIterations)
    grouped <- CommandUtils.groupWordVectors(vecs, clustering)
    quantizer <- CommandUtils.quantizeVectors(grouped.residuals, pqConfig)
    index <- CommandUtils.logTask(s"Building index for ${grouped.size} word vectors",
                                  Index.grouped(grouped, quantizer, metric, strategy),
                                  s"Built index for ${grouped.size} word vectors")
  } yield index

  def buildLinearIndex(vecs: WordVectors,
                       metric: Metric,
                       pqConfig: ProductQuantizer.Config)(implicit
                       contextShift: ContextShift[IO], clock: Clock[IO]): IO[Index] = for {
    sorted <- IO.delay(vecs.sorted)
    quantizer <- CommandUtils.quantizeVectors(sorted.toMatrix, pqConfig)
    index <- CommandUtils.logTask(s"Building index for ${sorted.size} word vectors",
                                  Index.sorted(sorted, quantizer, metric),
                                  s"Built index for ${sorted.size} word vectors")
  } yield index

  def buildIndex(vecs: WordVectors,
                 metric: Metric,
                 partitioned: Option[Config.Partitioned],
                 pqConfig: ProductQuantizer.Config)(implicit
                 contextShift: ContextShift[IO], clock: Clock[IO]): IO[Index] =
    partitioned match {
      case None =>
        buildLinearIndex(vecs, metric, pqConfig)(contextShift, clock)
      case Some(Config.Partitioned(maybePartitions, maybeLimit)) =>
        val partitions = maybePartitions.getOrElse(vecs.size / 1000)
        val limit = maybeLimit.getOrElse(math.max((partitions * 0.05).toInt, 5))
        val strategy = Index.GroupedIndex.Strategy.LimitGroups(limit)
        buildSublinearIndex(vecs, metric, partitions, strategy, pqConfig)(contextShift, clock)
    }

  def run(implicit contextShift: ContextShift[IO], clock: Clock[IO]): Opts[IO[ExitCode]] = {
    Config.opts.map { config =>
      val pqConfig = ProductQuantizer.Config(numClusters = config.numClusters,
                                             numQuantizers = config.numQuantizers,
                                             maxIterations = config.maxIterations)
      for {
        vecs <- CommandUtils.readWord2VecWithLogging(config.input, config.metric.normalized)
        index <- buildIndex(vecs, config.metric, config.partitioned, pqConfig)
        _ <- CommandUtils.writeIndex(index, config.output)
      } yield ExitCode(0)
    }
  }

  def command(implicit contextShift: ContextShift[IO], clock: Clock[IO]): Command[IO[ExitCode]] =
    Command("build-index", "build a nearest neighbour index", true)(run)
}
