package net.tixxit.gulon

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}

class SummaryStatsSpec extends FunSuite with PropertyChecks {
  import TestUtils.nearlyEqual

  val genSummaryStats: Gen[SummaryStats] = for {
    values <- Gen.listOf(Gen.choose(-1e4f, 1e4f))
  } yield SummaryStats(values)

  implicit val arbSummaryStats: Arbitrary[SummaryStats] =
    Arbitrary(genSummaryStats)


  test("associative") {
    forAll { (x: SummaryStats, y: SummaryStats, z: SummaryStats) =>
      val lhs = ((x ++ y) ++ z)
      val rhs = (x ++ (y ++ z))
      assert(lhs.count == rhs.count)
      assert(nearlyEqual(lhs.mean, rhs.mean))
      if (lhs.count > 0) {
        assert(nearlyEqual(lhs.variance, rhs.variance))
      }
    }
  }

  test("identity") {
    forAll { (x: SummaryStats) =>
      assert((x ++ SummaryStats.zero) == x)
      assert((SummaryStats.zero ++ x) == x)
    }
  }

  test("roughly equal to naive algorithm") {
    forAll(Gen.nonEmptyListOf(Gen.choose(-1e4f, 1e4f))) { values =>
      val stats = SummaryStats(values)
      val mean = stats.mean
      val expectedMean = values.sum / values.size
      assert(nearlyEqual(mean, expectedMean))
      val expectedVariance = values.map { x => math.pow(x - mean, 2) }.sum / values.size
      assert(nearlyEqual(stats.variance, expectedVariance.toFloat))
    }
  }
}
