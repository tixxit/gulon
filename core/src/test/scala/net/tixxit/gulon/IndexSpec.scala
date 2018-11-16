package net.tixxit.gulon

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class IndexSpec extends FunSuite with PropertyChecks {
  import Generators._
  // Make a product quantizer from a bunch of KMeans.
  // Generate some encoded vectors.
  // Compare results against decoded nearest neighbours.
  //

  case class SortedIndexWithQuery(
    index: Index.SortedIndex,
    query: Array[Float],
    k: Int)

  val genSortedIndexWithQuery = for {
    sortedIndex <- genSortedIndex
    query <- genPoint(sortedIndex.dimension)
    k <- Gen.choose(1, sortedIndex.size)
  } yield SortedIndexWithQuery(sortedIndex, query, k)

  test("SortedIndex should query encoded nearest neighbours") {
    forAll(genSortedIndexWithQuery) { case SortedIndexWithQuery(index, p, k) =>
      println(s"d=${index.dimension}, p=${p.length} k=$k")
      val result = index.query(k, p)
      val decoded = index.keyIndex.keys.map { w => index.lookup(w).get }
      val indices = Index.exactNearestNeighbours(decoded, p, k)
      val expected = indices.map(index.keyIndex(_)).toList
      assert(result.map(_._1).toList == expected)
    }
  }
}
