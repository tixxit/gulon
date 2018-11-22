package net.tixxit.gulon
package command

import java.io.{BufferedReader, File,
                FileOutputStream, FileInputStream,
                InputStream, InputStreamReader, OutputStream}
import java.nio.file.Path

import cats.effect.{Clock, ContextShift, IO}
import cats.Eval
import cats.implicits._

object CommandUtils {
  final def formatBytes(bytes: Long): String = {
    if (bytes < 1000L) s"${bytes}b"
    else if (bytes < 1000000L) f"${bytes / 1000d}%.1fkb"
    else if (bytes < 1000000000L) f"${bytes / 1000000d}%.1fmb"
    else f"${bytes / 1000000000d}%.1fgb"
  }

  final def clearLine(count: Int = 1): IO[Unit] = for {
    _ <- IO.delay(print("\u001b[0K"))
    _ <- if (count > 1) IO.delay(print("\u001b[1A")).productREval(Eval.later(clearLine(count - 1)))
         else IO.pure(())
  } yield ()

  final def maybeLogProgress(p: Option[Float], extra: String): IO[Unit] =
    p match {
      case Some(p) => logProgress(p, extra)
      case None =>
        for {
          _ <- clearLine()
          _ <- IO.delay(print(s"[--------------------] ??.?% $extra\r"))
        } yield ()
    }

  // [###                 ] __._% $extra
  final def logProgress(p: Float, extra: String): IO[Unit] = {
    val numHashes = (p * 20).toInt
    val hashes = "#" * numHashes
    val spaces = " " * (20 - numHashes)
    val perc = p * 100
    for {
      _ <- clearLine()
      _ <- IO.delay(print(f"[$hashes$spaces] $perc%2.1f%% $extra\r"))
    } yield ()
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

  private def logReadProgress(report: WordVectors.ProgressReport): IO[Unit] =
    CommandUtils.maybeLogProgress(report.percentageRead, f"charsPerWord=${report.charsPerWord}%.1f memUsed=${formatBytes(report.sizeEstimate)}")

  final def printRunning(message: String): IO[Unit] =
    IO.delay(println(s"\u001b[36mRUNNING:\u001b[0m $message"))

  final def printSuccess(message: String): IO[Unit] =
    IO.delay(println(s"\u001b[32mSUCCESS:\u001b[0m $message"))

  final def printError(message: String): IO[Unit] =
    IO.delay(println(s"\u001b[31mERROR:\u001b[0m $message"))

  final def logTask[A](startMessage: String, task: IO[A], successMessage: String): IO[A] =
    logTask[A](startMessage, task, (_: A) => successMessage)

  final def logTask[A](startMessage: String, task: IO[A], successMessage: A => String): IO[A] = for {
    _ <- printRunning(startMessage)
    a <- task
    _ <- clearLine(2)
    _ <- printSuccess(successMessage(a))
  } yield a

  final def readWord2VecWithLogging(path: Path, normalized: Boolean): IO[WordVectors.Unindexed] =
    logTask("Reading word vectors",
            WordVectors.readWord2VecFile(path.toFile, normalized, logReadProgress),
            (vecs: WordVectors.Unindexed) => s"Read ${vecs.size} word vectors")

  final def writeIndex(index: Index, path: Path): IO[Unit] =
    logTask(s"Writing index to ${path}",
            CommandUtils.writePath(path)(o => IO.delay(Index.toProtobuf(index).writeTo(o))),
            s"Wrote index to ${path}")

  private def logPartitioner(report: KMeans.ProgressReport): IO[Unit] = {
    val p = report.numIterations.toFloat / report.maxIterations
    CommandUtils.logProgress(p, s"iters=${report.numIterations}/${report.maxIterations}")
  }

  final def computePartitions(vecs: WordVectors, partitions: Int, maxIterations: Int)(implicit contextShift: ContextShift[IO]): IO[KMeans] = {
    val coarseConfig = KMeans.Config(partitions, maxIterations, report = logPartitioner)
    logTask("Computing partitions",
            KMeans.computeClusters(Vectors(vecs.toMatrix), coarseConfig),
            s"Computed ${partitions} partitions")
  }

  final def groupWordVectors(vecs: WordVectors, clustering: KMeans)(implicit contextShift: ContextShift[IO]): IO[WordVectors.Grouped] =
    logTask("Reindexing word vectors", vecs.grouped(clustering), "Re-indexed word vectors")

  private def logProductQuantizer(report: ProductQuantizer.ProgressReport): IO[Unit] = {
    val p = report.completedIterations.toFloat / report.totalIterations
    CommandUtils.logProgress(p, s"iters=${report.completedIterations}/${report.totalIterations} step=${report.stepSize.mean} stdDev=${report.stepSize.stdDev}")
  }

  final def quantizeVectors(vectors: Matrix, pqConfig: ProductQuantizer.Config)(implicit contextShift: ContextShift[IO]): IO[ProductQuantizer] =
    logTask("Quantizing word vectors",
            ProductQuantizer(vectors, pqConfig.copy(report = logProductQuantizer)),
            s"Quantized ${vectors.rows} word vectors")

  def logTestsProgress(report: Tests.ProgressReport): IO[Unit] =
    CommandUtils.logProgress(report.percentage, f"complete=${report.completed}/${report.total} qps=${report.queriesPerSecond}%.1f")

  final def sampleTests(vecs: WordVectors.Indexed, sampleSize: Int)(implicit
                        contextShift: ContextShift[IO], clock: Clock[IO]): IO[Tests] =
    logTask("Sampling test vectors and precomputing distances",
            Tests.sample(vecs, sampleSize, report=logTestsProgress),
            s"Sampled ${sampleSize} vectors")

  final def calculateRecall(tests: Tests, index: Index, eps: Float)(implicit
                            contextShift: ContextShift[IO], clock: Clock[IO]): IO[Map[Int, SummaryStats]] = for {
    _ <- printRunning("Calculating recall of index")
    stats <- tests.recallOf(index, eps, report=logTestsProgress)
    _ <- clearLine(2)
  } yield stats
}
