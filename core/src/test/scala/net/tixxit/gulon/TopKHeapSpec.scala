package net.tixxit.gulon

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}

class TopKHeapSpec extends FunSuite with PropertyChecks {
  def toVector(heap: TopKHeap): List[(Int, Float)] =
    List.fill(heap.size) {
      val k = heap.keys(0)
      val v = heap.values(0)
      heap.delete()
      k -> v
    }.reverse

  test("get first k values") {
    val genArgs = for {
      kvs <- Arbitrary.arbitrary[List[(Int, Float)]]
      k <- Gen.choose(1, math.max(2 * kvs.size, 1))
    } yield (kvs, k)

    forAll(genArgs) { case (kvs, k) =>
      val heap = TopKHeap(k)
      kvs.foreach { case (i, x) =>
        heap.update(i, x)
      }
      val actual = toVector(heap)
      val expected = kvs.sortBy(_._2).take(k)
      assert(actual == expected)
    }
  }
}
