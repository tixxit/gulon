package net.tixxit.gulon.command

import cats.effect.{ExitCode, IO, IOApp}
import com.monovore.decline._

object Main extends IOApp {
  val commands: Opts[IO[ExitCode]] =
    Opts.subcommand(BuildIndex.command)
      .orElse(Opts.subcommand(Query.command))
      .orElse(Opts.subcommand(QueryWords.command))
      .orElse(Opts.subcommand(Repl.command))
      .orElse(Opts.subcommand(Test.command))

  val app: Command[IO[ExitCode]] =
    Command("gulon",
            "build approximate nearest neighbour indexes for keyed vectors",
            true)(commands)

  def run(args: List[String]): IO[ExitCode] =
    app.parse(args) match {
      case Right(program) => program
      case Left(help) =>
        for {
          _ <- IO.delay(System.err.println(help))
        } yield ExitCode(1)
    }
}
