package net.tixxit.gulon.command

import java.io.{BufferedReader, File,
                FileOutputStream, FileInputStream,
                InputStream, InputStreamReader, OutputStream}
import java.nio.file.Path

import cats.effect.IO

object CommandUtils {
  final def formatBytes(bytes: Long): String = {
    if (bytes < 1000L) s"${bytes}b"
    else if (bytes < 1000000L) f"${bytes / 1000d}%.1fkb"
    else if (bytes < 1000000000L) f"${bytes / 1000000d}%.1fmb"
    else f"${bytes / 1000000000d}%.1fgb"
  }

  final def maybeLogProgress(p: Option[Float], extra: String): IO[Unit] =
    p match {
      case Some(p) => logProgress(p, extra)
      case None =>
        IO.delay(print(s"[--------------------] ??.?% $extra\r"))
    }

  // [###                 ] __._% $extra
  final def logProgress(p: Float, extra: String): IO[Unit] = IO.delay {
    val numHashes = (p * 20).toInt
    val hashes = "#" * numHashes
    val spaces = " " * (20 - numHashes)
    val perc = p * 100
    print(f"[$hashes$spaces] $perc%2.1f%% $extra\r")
  }

  final def writePath(path: Path)(f: OutputStream => IO[Unit]): IO[Unit] =
    IO.delay(new FileOutputStream(path.toFile))
      .bracket(f)(o => IO.delay(o.close()))

  final def openPath(path: Path): IO[InputStream] =
    IO.delay(new FileInputStream(path.toFile))

  final def withPathOrStdIn(maybePath: Option[Path])(f: BufferedReader => IO[Unit]): IO[Unit] =
    maybePath match {
      case Some(path) =>
        def f0: InputStream => IO[Unit] = { in =>
          f(new BufferedReader(new InputStreamReader(in, "utf-8")))
        }
        openPath(path).bracket(f0)(is => IO.delay(is.close()))

      case None =>
        for {
          reader <- IO.delay(new BufferedReader(new InputStreamReader(System.in, "utf-8")))
          _ <- f(reader)
        } yield ()
    }
}
