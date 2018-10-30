package net.tixxit.gulon
package command

import java.io.BufferedReader
import java.nio.file.Path

import cats.effect.{ContextShift, ExitCode, IO}
import cats.implicits._
import com.monovore.decline._

object QueryWords {
  case class Options(
    k: Int,
    index: Path,
    query: Option[Path])

  object Options {
    val k = Opts.option[Int](
      "neighbours", short="k", metavar="num",
      help="number of nearest neighbours to return")
        .validate("must be at least 1")(_ > 0)
        .withDefault(1)

    val index = Opts.option[Path](
      "index", short="i", metavar="file",
      help="path to ANN index")
    val query = Opts.argument[Path](metavar="file").orNone

    val opts: Opts[Options] =
      (k, index, query).mapN(Options(_, _, _))
  }

  def printResult(key: String, maybeResult: Option[Index.Result]): IO[Unit] =
    IO.delay {
      maybeResult match {
        case Some(result) =>
          println(s"$key: ${result.map(_._1).mkString(",")}")
        case None =>
          println(s"$key: not found")
      }
    }

  def loop(index: Index, k: Int)(reader: BufferedReader): IO[Unit] =
    IO.suspend {
      Option(reader.readLine()) match {
        case Some(word) =>
          for {
            _ <- printResult(word, index.queryByWord(k, word))
            _ <- loop(index, k)(reader)
          } yield ()
        case None =>
          IO.pure(())
      }
    }

  def run(implicit contextShift: ContextShift[IO]): Opts[IO[ExitCode]] =
    Options.opts.map { options =>
      for {
        index <- Index.read(CommandUtils.openPath(options.index))
        _ <- CommandUtils.withPathOrStdIn(options.query)(loop(index, options.k))
      } yield ExitCode(0)
    }

  def command(implicit contextShift: ContextShift[IO]): Command[IO[ExitCode]] =
    Command("query-words", "query nearest neighbour index by word", true)(run)
}
