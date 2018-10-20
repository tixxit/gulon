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

  test("subvectors splits data") {
    val genArgs: Gen[(Matrix, Int)] = for {
      matrix <- genMatrix
      n <- Gen.choose(1, matrix.cols)
    } yield (matrix, n)

    forAll(genArgs) { case (matrix, n) =>
      val subvectors = Vectors.subvectors(matrix, n)
      assert(subvectors.length == n)
      val maxSize = subvectors.map(_.dimension).max
      val minSize = subvectors.map(_.dimension).min
      assert((maxSize - minSize) <= 1)
      subvectors.foreach { vectors =>
        assert(vectors.size == matrix.rows)
        val slice = Vectors(Matrix(
          vectors.size, vectors.dimension,
          matrix.data.map { row =>
            Array.range(vectors.from, vectors.until).map(row(_))
          }))
        assert(slice == vectors)
      }
    }
  }
}
