package net.tixxit.gulon
package command

import java.nio.file.Path

import cats.data.Validated
import cats.effect.{Clock, ContextShift, ExitCode, IO}
import cats.implicits._
import com.monovore.decline._

object Cluster {
  case class Options(
    numClusters: Int,
    maxIterations: Int,
    metric: Metric,
    vectors: Path)

  object Options {
    val numClusters = Opts.option[Int](
      "clusters", short="k", metavar="num_clusters",
      help="number of clusters")
        .validate("clusters must be at least 1")(_ > 0)
    val maxIterations = Opts.option[Int](
      "max-iters", short="n", metavar="iterations",
      help="maximum number of iterations per quantizer")
        .withDefault(100)
    val metric = Opts.option[String](
      "metric", short="d", metavar="l2|cosine",
      help="distance metric to use")
      .mapValidated {
        case "l2" => Validated.valid(Metric.L2)
        case "cosine" => Validated.valid(Metric.Cosine)
        case unknownMetric => Validated.invalidNel(s"unsupported metric: $unknownMetric")
      }
    val vectors = Opts.argument[Path](metavar="file")

    val opts = (numClusters, maxIterations, metric, vectors)
      .mapN(Options(_, _, _, _))
  }

  def run(implicit contextShift: ContextShift[IO], clock: Clock[IO]): Opts[IO[ExitCode]] =
    Options.opts.map { opts =>
      for {
        vectors <- CommandUtils.readWord2VecWithLogging(opts.vectors, opts.metric.normalized)
        result <- CommandUtils.computeClustering(vectors, opts.numClusters, opts.maxIterations)
        (clustering, assignments) = result
        //_ <- IO.delay(println(Vectors(Matrix(opts.numClusters, vectors.dimension, clustering.centroids)).toString))
      } yield ExitCode(0)
    }

  def command(implicit contextShift: ContextShift[IO], clock: Clock[IO]): Command[IO[ExitCode]] =
    Command("cluster", "compute a clustering of the vectors", true)(run)
}
