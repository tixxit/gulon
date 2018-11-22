package net.tixxit.gulon

import scala.util.Random
import scala.concurrent.duration.MILLISECONDS

import cats.effect.{Clock, ContextShift, IO}
import cats.effect.concurrent.Ref
import cats.kernel.Monoid
import cats.implicits._

case class Tests(wordVectors: WordVectors.Indexed,
                 queries: Vector[Tests.Query]) {
  /**
   * Returns a map from number of neighbours queries to the recall across all
   * samples. The recall is returned as the summary stats, so that the variance
   * is available.
   */
  def recallOf(index: Index,
               eps: Float = 0f,
               report: Tests.ProgressReport => IO[Unit] = _ => IO.pure(()))(implicit
               contextShift: ContextShift[IO], clock: Clock[IO]): IO[Map[Int, SummaryStats]] =
    Tests.parTraverseWithProgress(queries, report) { case Tests.Query(query, results) =>
      IO.delay {
        val maxK = results.map(_.k).max
        // We need the exact distances from the query point to the result.
        val distances = index.query(maxK, query).map { case (word, _) =>
          val i = wordVectors.keyIndex.lookup(word).get
          MathUtils.distanceSq(query, wordVectors(i))
        }
        results
          .iterator
          .map { case Tests.QueryResult(k, maxDistanceSq) =>
            val cutoff =
              if (eps == 0f) maxDistanceSq
              else math.pow(math.sqrt(maxDistanceSq) * (1f + eps), 2).toFloat
            val tp = distances.take(k).count(_ <= cutoff)
            k -> SummaryStats(tp.toFloat / k)
          }
          .toMap
      }
    }.map(Monoid.combineAll(_))
}

object Tests {
  case class QueryResult(k: Int, maxDistanceSq: Float)
  case class Query(query: Array[Float], results: Vector[QueryResult])

  def nearestNeighbours(wordVectors: WordVectors.Indexed, query: Array[Float], k: Int): Index.Result = {
    val heap = Index.exactNearestNeighbours(wordVectors.toMatrix.data, query, k)
    Index.Result.fromHeap(wordVectors.keyIndex, heap)
  }

  val defaultKs: Vector[Int] = Vector(1, 2, 3, 5, 10, 25, 50, 100, 500, 1000)

  case class ProgressReport(completed: Int,
                            total: Int,
                            startTimeMs: Long,
                            reportTimeMs: Long) {
    def percentage: Float = completed.toFloat / total
    def queriesPerSecond: Float = (completed.toFloat / (reportTimeMs - startTimeMs)) * 1000f
  }

  object ProgressReport {
    def init(size: Int)(implicit clock: Clock[IO]): IO[Ref[IO, ProgressReport]] = for {
      start <- clock.monotonic(MILLISECONDS)
      ref <- Ref.of[IO, ProgressReport](ProgressReport(0, size, start, start))
    } yield ref

    def update(ref: Ref[IO, ProgressReport], n: Int = 1)(implicit clock: Clock[IO]): IO[ProgressReport] = for {
      reportTime <- clock.monotonic(MILLISECONDS)
      _ <- ref.update(pr => pr.copy(completed = pr.completed + n, reportTimeMs = reportTime))
      pr <- ref.get
    } yield pr
  }

  def sample(wordVectors: WordVectors.Indexed,
             sampleSize: Int = 1000,
             ks: Vector[Int] = defaultKs,
             report: ProgressReport => IO[Unit],
             seed: Int = 0)(implicit
             contextShift: ContextShift[IO], clock: Clock[IO]): IO[Tests] = {
    val rng = new Random(seed)
    forQueries(wordVectors,
               Vector.fill(sampleSize)(wordVectors(rng.nextInt(wordVectors.size))),
               ks,
               report)
  }

  private def getDistances(wordVectors: WordVectors.Indexed,
                           query: Array[Float],
                           ks: Vector[Int]): Query = {
    val result = nearestNeighbours(wordVectors, query, ks.max)
    val results = ks.filter(_ <= result.length).map { k =>
      QueryResult(k, result.getDistance(k - 1))
    }
    Query(query, results)
  }


  def forQueries(wordVectors: WordVectors.Indexed,
                 queries: Vector[Array[Float]],
                 ks: Vector[Int] = defaultKs,
                 report: ProgressReport => IO[Unit])(implicit
                 contextShift: ContextShift[IO], clock: Clock[IO]): IO[Tests] =
    parTraverseWithProgress(queries, report) { query =>
      IO.delay(getDistances(wordVectors, query, ks))
    }.map(Tests(wordVectors, _))

  def parTraverseWithProgress[A, B](as: Vector[A],
                                    report: ProgressReport => IO[Unit])(
                                    f: A => IO[B])(implicit
                                    contextShift: ContextShift[IO], clock: Clock[IO]): IO[Vector[B]] =
    ProgressReport.init(as.size).flatMap { ref =>
      as.parTraverse { a => 
        for {
          _ <- IO.shift
          b <- f(a)
          pr <- ProgressReport.update(ref, 1)
          _ <- report(pr)
        } yield b
      }
    }
}
