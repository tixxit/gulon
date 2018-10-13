package net.tixxit.gulon

import cats.effect.{ExitCode, IO, IOApp}
import com.monovore.decline._

import net.tixxit.gulon.command.BuildIndex

object Main extends IOApp {
  val commands: Opts[IO[ExitCode]] =
    Opts.subcommand(BuildIndex.command)

  val app: Command[IO[ExitCode]] =
    Command("gulon", "hodor", true)(commands)

  def run(args: List[String]): IO[ExitCode] =
    app.parse(args) match {
      case Right(program) => program
      case Left(help) =>
        for {
          _ <- IO.delay(System.err.println(help))
        } yield ExitCode(1)
    }
}
