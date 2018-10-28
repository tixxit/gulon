package net.tixxit.gulon
package command

import java.nio.file.Path

import cats.effect.{ContextShift, ExitCode, IO}
import cats.implicits._
import com.monovore.decline._

object Query {
  case class Options(
    k: Int,
    index: Path,
    query: Path)

  object Options {
    val k = Opts.option[Int](
      "neighbours", short="k", metavar="num",
      help="number of nearest neighbours to return")
        .validate("must be at least 1")(_ > 0)
        .withDefault(1)

    val index = Opts.option[Path](
      "index", short="i", metavar="file",
      help="path to ANN index")
    val query = Opts.argument[Path](metavar="file")

    val opts: Opts[Options] =
      (k, index, query).mapN(Options(_, _, _))
  }

  def printResults(results: Vector[(String, Index.Result)]): IO[Unit] =
    IO.delay {
      results.foreach { case (key, result) =>
        println(s"$key: ${result.map(_._1).mkString(",")}")
      }
    }

  def run(implicit contextShift: ContextShift[IO]): Opts[IO[ExitCode]] = {
    Options.opts.map { options =>
      for {
        vecs <- WordVectors.readWord2VecFile(options.query.toFile)
        index <- Index.read(CommandUtils.openPath(options.index))
        results = index.batchQuery(options.k, vecs.vectors)
        _ <- printResults(vecs.keys.zip(results))
      } yield ExitCode(0)
    }
  }

  def command(implicit contextShift: ContextShift[IO]): Command[IO[ExitCode]] =
    Command("query", "query nearest neighbour index", true)(run)
}
