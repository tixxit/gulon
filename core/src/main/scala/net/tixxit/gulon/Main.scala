package net.tixxit.gulon

import scala.concurrent.ExecutionContext

import cats.effect.{ExitCode, IO, IOApp}

object Main extends IOApp {
  case class Config(k: Int, m: Int, n: Int, path: Option[String])

  def parseArgs(args: List[String], config: Config = Config(16, 30, 10, None)): Config =
    args match {
      case "-m" :: m :: rest => parseArgs(rest, config.copy(m = m.toInt))
      case "-n" :: n :: rest => parseArgs(rest, config.copy(n = n.toInt))
      case "-k" :: k :: rest => parseArgs(rest, config.copy(k = k.toInt))
      case path :: rest => parseArgs(rest, config.copy(path = Some(path)))
      case Nil => config
    }

  private def formatBytes(bytes: Long): String = {
    if (bytes < 1000L) s"${bytes}b"
    else if (bytes < 1000000L) f"${bytes / 1000d}%.1fkb"
    else if (bytes < 1000000000L) f"${bytes / 1000000d}%.1fmb"
    else f"${bytes / 1000000000d}%.1fgb"
  }

  // [###                 ] __._% $extra
  private def logProgress(p: Float, extra: String): IO[Unit] = IO.delay {
    val numHashes = (p * 20).toInt
    val hashes = "#" * numHashes
    val spaces = " " * (20 - numHashes)
    val perc = p * 100
    print(f"[$hashes$spaces] $perc%2.1f%% $extra\r")
  }

  private def logReadProgress(report: WordVectors.ProgressReport): IO[Unit] =
    logProgress(report.percentageRead, f"charsPerWord=${report.charsPerWord}%.1f memUsed=${formatBytes(report.sizeEstimate)}")

  private def logProductQuantizer(report: ProductQuantizer.ProgressReport): IO[Unit] = {
    val p = report.completedIterations.toFloat / report.totalIterations
    logProgress(p, s"iters=${report.completedIterations}/${report.totalIterations} step=${report.stepSize.mean} stdDev=${report.stepSize.stdDev}")
  }

  def run(args: List[String]): IO[ExitCode] = {
    val opts = parseArgs(args)
    val config = ProductQuantizer.Config(numSubvectors = opts.m,
                                         numClusters = opts.k,
                                         maxIterations = opts.n,
                                         executionContext = ExecutionContext.global,
                                         report = logProductQuantizer)
    for {
      _ <- IO.delay(println("Reading word vectors"))
      vecs <- WordVectors.readWord2VecPath(opts.path.get, logReadProgress)
      _ <- IO.delay(println("\nComputing product quantizer"))
      quantizer <- ProductQuantizer(vecs.vectors, config)
      _ <- IO.delay(println())
      _ <- IO.delay(println(quantizer))
    } yield ExitCode(0)
  }
}
