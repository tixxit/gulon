package net.tixxit.gulon

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class IndexSpec extends FunSuite with PropertyChecks {
  import Generators._

  case class SortedIndexWithQuery(
    index: Index.SortedIndex,
    query: Array[Float],
    k: Int)

  val genSortedIndexWithQuery = for {
    sortedIndex <- genSortedIndex
    query <- genPoint(sortedIndex.dimension)
    k <- Gen.choose(1, sortedIndex.size)
  } yield SortedIndexWithQuery(sortedIndex, query, k)

  def assertResultsMatch(actual: Seq[(String, Float)], expected: Seq[String]): Unit = {
    val secondaryOrder: Map[String, Int] = expected.zipWithIndex.toMap
    val reordered = actual
      .sortBy { case (word, distSq) =>
        (distSq, secondaryOrder.getOrElse(word, 0))
      }
      .map(_._1)
    assert(reordered == expected)
  }

  test("SortedIndex should query encoded nearest neighbours") {
    forAll(genSortedIndexWithQuery) { case SortedIndexWithQuery(index, p, k) =>
      val result = index.query(k, p)
      val decoded = index.keyIndex.keys.map { w => index.lookup(w).get }
      val p0 = if (index.metric.normalized) MathUtils.normalize(p) else p
      val indices = Index.exactNearestNeighbours(decoded, p0, k)
      val expected = indices.map(index.keyIndex(_)).toList
      assertResultsMatch(result, expected)
    }
  }

  test("protobuf round-trips") {
    forAll(genIndex) { index =>
      val result = Index.fromProtobuf(Index.toProtobuf(index))
      assert(result == index)
    }
  }
}
