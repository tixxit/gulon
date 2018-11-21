package net.tixxit.gulon
package command

import java.nio.file.Path

import cats.implicits._
import cats.effect.{ContextShift, ExitCode, IO}
import com.monovore.decline._

object Test {
  case class Options(
    vectors: Path,
    index: Path,
    sampleSize: Int,
    epsilon: Float)

  object Options {
    val vectors = Opts.option[Path](
      "vectors", short="v", metavar="file",
      help="word2vec word vectors")
    val index = Opts.option[Path](
      "index", short="i", metavar="file",
      help="path to ANN index")
    val sampleSize = Opts.option[Int](
      "sample", short="s", metavar="size",
      help="number of queries to sample for recall calculation")
        .validate("must be greater than 0")(_ > 0)
        .withDefault(1000)
    val epsilon = Opts.option[Float](
      "error", short="e", metavar="relative error",
      help="amount of relative error allowed when calculating recall")
        .validate("must be non-negative")(_ >= 0f)
        .withDefault(0f)

    val opts: Opts[Options] =
      (vectors, index, sampleSize, epsilon).mapN(Options(_, _, _, _))
  }

  def printResults(recall: Map[Int, SummaryStats]): IO[Unit] =
    recall.toList.sortBy(_._1)
      .traverse_ { case (k, stats) =>
        IO.delay(println(s"R@$k: ${stats.mean} +/- ${stats.stdDev}"))
      }

  def run(implicit contextShift: ContextShift[IO]): Opts[IO[ExitCode]] =
    Options.opts.map { options =>
      for {
        vecs <- WordVectors.readWord2VecFile(options.vectors.toFile)
        index <- Index.read(CommandUtils.openPath(options.index))
        _ = println("sampling...")
        tests = Tests.sample(vecs.sorted, options.sampleSize)
        _ = println("calculating recall of index...")
        recall = tests.recallOf(index, options.epsilon)
        _ <- printResults(recall)
      } yield ExitCode(0)
    }

  def command(implicit contextShift: ContextShift[IO]): Command[IO[ExitCode]] =
    Command("test", "calculate recall of index", true)(run)
}
