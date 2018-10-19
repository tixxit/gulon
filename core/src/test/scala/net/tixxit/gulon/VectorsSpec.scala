package net.tixxit.gulon

import scala.util.Random

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class VectorsSpec extends FunSuite with PropertyChecks {
  val genMatrix: Gen[Matrix] = Gen.sized { n =>
    Gen.choose(1, math.max(n, 1)).flatMap { cols =>
      val genRow: Gen[Array[Float]] =
        Gen.listOfN(cols, arbitrary[Float]).map(_.toArray)
      Gen.nonEmptyListOf(genRow)
        .map(_.toArray)
        .map { data =>
          Matrix(data.length, cols, data)
        }
    }
  }

  val genSubVectors: Gen[Seq[Vectors]] = for {
    matrix <- genMatrix
    _ = if (matrix.cols == 0) println("wtf") else ()
    n <- Gen.choose(1, matrix.cols)
  } yield Vectors.subvectors(matrix, n)

  val genVectors: Gen[Vectors] = for {
    subVectors <- genSubVectors
    i <- Gen.choose(0, subVectors.length - 1)
  } yield subVectors(i)

  implicit val arbVectors: Arbitrary[Vectors] =
    Arbitrary(genVectors)

  test("slice()") {
    forAll { (vectors: Vectors) =>
      assert(vectors == Vectors(vectors.slice()))
    }
  }
}
