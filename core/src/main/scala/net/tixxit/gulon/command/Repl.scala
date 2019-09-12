package net.tixxit.gulon
package command

import java.nio.file.Path

import cats.data.{Validated, ValidatedNel}
import cats.effect.{ContextShift, ExitCode, IO}
import cats.implicits._
import com.monovore.decline._
import org.jline.reader.{EndOfFileException, LineReader, LineReaderBuilder}
import org.jline.utils.{AttributedString, AttributedStringBuilder, AttributedStyle}

object Repl {

  case class Options(index: Path)

  object Options {
    val index = Opts.option[Path](
      "index", short="i", metavar="file",
      help="path to ANN index")

    val opts: Opts[Options] = index.map(Options(_))
  }

  sealed trait QueryResult
  case class ScalarResult(value: Float) extends QueryResult
  case class PointResult(value: Array[Float]) extends QueryResult
  case class IndexResult(value: Index.Result) extends QueryResult

  sealed trait Query[+A]
  case class Add(lhs: Query[PointResult], rhs: Query[PointResult]) extends Query[PointResult]
  case class Sub(lhs: Query[PointResult], rhs: Query[PointResult]) extends Query[PointResult]
  case class Lookup(word: String) extends Query[PointResult]
  case class NN(query: Query[PointResult], k: Int) extends Query[IndexResult]
  case class Dist(x: Query[PointResult], y: Query[PointResult]) extends Query[ScalarResult]

  object Parser {
    import fastparse._
    import NoWhitespace._

    def ws[_: P]: P[Unit] = " ".rep
    def lookup[_: P]: P[Query[PointResult]] = P( "\"" ~/ (!"\"" ~ AnyChar.!).rep ~ "\"" )
      .map(_.mkString)
      .map(Lookup(_))
    def parens[_: P]: P[Query[PointResult]] = P( "(" ~/ pointExpr ~ ")" )
    def term[_: P]: P[Query[PointResult]] = P( parens | lookup )
    def pointExpr[_: P]: P[Query[PointResult]] = P( term ~ (ws ~ CharIn("+\\-").! ~ ws ~/ term).rep )
      .map { case (init, terms) =>
        terms.foldLeft(init) {
          case (x, ("+", y)) => Add(x, y)
          case (x, ("-", y)) => Sub(x, y)
        }
      }
    def nonNegative[_: P]: P[Int] = P( CharIn("0-9").rep(1).!.map(_.toInt) )
    def scalarExpr[_: P]: P[Query[ScalarResult]] =
      P( "dist(" ~/ ws ~ pointExpr ~ ws ~ "," ~ ws ~ pointExpr ~ ws ~ ")" )
        .map { case (x, y) => Dist(x, y) }
    def indexExpr[_: P]: P[Query[IndexResult]] =
      P( "nn(" ~/ ws ~ pointExpr ~ ws ~ "," ~ ws ~ nonNegative ~ ws ~ ")" )
        .map { case (q, k) => NN(q, k) }
    def expr[_: P]: P[Query[QueryResult]] = P( (pointExpr | indexExpr | scalarExpr) ~ End )
  }

  type Result[A] = ValidatedNel[String, A]

  def exec[A](index: Index, query: Query[A]): Result[A] = {
    def recur[A0](q: Query[A0]): Result[A0] =
      q match {
        case Add(x, y) =>
          (recur(x), recur(y)).mapN { (x, y) => PointResult(MathUtils.add(x.value, y.value)) }
        case Sub(x, y) =>
          (recur(x), recur(y)).mapN { (x, y) => PointResult(MathUtils.subtract(x.value, y.value)) }
        case Dist(x, y) =>
          (recur(x), recur(y)).mapN { (x, y) => ScalarResult(MathUtils.distance(x.value, y.value)) }
        case Lookup(word) =>
          index.lookup(word)
            .map { x => Validated.validNel(PointResult(x)) }
            .getOrElse(Validated.invalidNel(s"lookup failed: $word"))
        case NN(query, k) =>
          recur(query).map { point => IndexResult(index.query(k, point.value)) }
      }

    recur(query)
  }

  def repl(index: Index): IO[Unit] = {
    import fastparse.{Parsed, parse}

    def formatError(message: String): String =
      new AttributedStringBuilder()
        .style(AttributedStyle.DEFAULT.foreground(AttributedStyle.RED))
        .append("error> ")
        .style(AttributedStyle.DEFAULT)
        .append(message)
        .toAnsi

    def parseExecFormat(line: String): String =
      parse(line, Parser.expr(_)) match {
        case Parsed.Success(query, _) =>
          exec(index, query) match {
            case Validated.Valid(PointResult(result)) =>
              if (result.length <= 4) {
                s"[${result.mkString(", ")}]"
              } else {
                s"[${result.take(2).mkString(", ")}, ..., ${result.last}]"
              }
            case Validated.Valid(IndexResult(result)) =>
              s"[${result.map(_._1).mkString(", ")}]"
            case Validated.Valid(ScalarResult(result)) =>
              result.toString
            case Validated.Invalid(errors) =>
              errors.map(formatError(_)).toList.mkString("\n")
          }
        case f @ Parsed.Failure(expected, failIndex, extra) =>
          formatError(s"invalid query: ${extra.trace().longMsg}")
      }

    val prompt = new AttributedString("gulon> ", AttributedStyle.DEFAULT.foreground(AttributedStyle.CYAN)).toAnsi

    def loop(reader: LineReader): IO[Unit] =
      IO.delay(reader.readLine(prompt))
        .flatMap { line =>
          IO.delay(println(parseExecFormat(line))) >> loop(reader)
        }
        .recover {
          case (_: EndOfFileException) => ()
        }

    IO.delay(LineReaderBuilder.builder().build())
      .flatMap(loop)
  }


  def run(implicit contextShift: ContextShift[IO]): Opts[IO[ExitCode]] =
    Options.opts.map { options =>
      for {
        index <- Index.read(CommandUtils.openPath(options.index))
        _ <- repl(index)
      } yield ExitCode(0)
    }

  def command(implicit contextShift: ContextShift[IO]): Command[IO[ExitCode]] =
    Command("repl", "open up a REPL for an index", true)(run)
}
