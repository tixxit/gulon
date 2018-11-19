package net.tixxit.gulon

import scala.util.Random

case class Tests(wordVectors: WordVectors.Indexed,
                 queries: Vector[Tests.Query]) {
  /**
   * Returns a map from number of neighbours queries to the recall across all
   * samples. The recall is returned as the summary stats, so that the variance
   * is available.
   */
  def recallOf(index: Index): Map[Int, SummaryStats] =
    queries.foldLeft(Map.empty[Int, SummaryStats]) {
      case (acc, Tests.Query(query, results)) =>
        val maxK = results.map(_.k).max
        // We need the exact distances from the query point to the result.
        val distances = index.query(maxK, query).map { case (word, _) =>
          val i = wordVectors.keyIndex.lookup(word).get
          MathUtils.distanceSq(query, wordVectors(i))
        }
        results.foldLeft(acc) { case (acc, Tests.QueryResult(k, maxDistanceSq)) =>
          val tp = distances.take(k).count(_ <= maxDistanceSq)
          val pointStats = SummaryStats(tp.toFloat / k)
          val stats = acc.getOrElse(k, SummaryStats.zero) ++ pointStats
          acc + (k -> stats)
        }
    }
}

object Tests {
  case class QueryResult(k: Int, maxDistanceSq: Float)
  case class Query(query: Array[Float], results: Vector[QueryResult])

  def nearestNeighbours(wordVectors: WordVectors.Indexed, query: Array[Float], k: Int): Index.Result = {
    val heap = Index.exactNearestNeighbours(wordVectors.toMatrix.data, query, k)
    Index.Result.fromHeap(wordVectors.keyIndex, heap)
  }

  val defaultKs: Vector[Int] = Vector(1, 2, 3, 5, 10, 25, 50, 100, 500, 1000)

  def sample(wordVectors: WordVectors.Indexed,
             sampleSize: Int = 1000,
             ks: Vector[Int] = defaultKs,
             seed: Int = 0) = {
    val rng = new Random(seed)
    forQueries(wordVectors,
               Vector.fill(sampleSize)(wordVectors(rng.nextInt(wordVectors.size))),
               ks)
  }

  def forQueries(wordVectors: WordVectors.Indexed,
                 queries: Vector[Array[Float]],
                 ks: Vector[Int] = defaultKs): Tests = {
    Tests(wordVectors, queries.map { query => 
      val result = nearestNeighbours(wordVectors, query, ks.max)
      val results = ks.filter(_ <= result.length).map { k =>
        QueryResult(k, result.getDistance(k))
      }
      Query(query, results)
    })
  }
}
