package net.tixxit.gulon

import scala.util.Random

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class ProductQuantizerSpec extends FunSuite with PropertyChecks {
  import Generators._

  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  test("(decode ∘ encode) is idempotent") {
    forAll(genProductQuantizerWithMatrix) {
      case (pq, m) =>
        val io = for {
          expected <- pq.encode(m).map(pq.decode(_))
          actual <- pq.encode(expected).map(pq.decode(_))
        } yield (expected, actual)

        val (expected, actual) = io.unsafeRunSync
        assert(TestUtils.nearlyEqualMatrices(expected, actual))
    }
  }

  test("decode selects centroids") {
    forAll(genProductQuantizer) { pq =>
      val k = pq.numClusters
      val coder = pq.coderFactory(k)
      val indices = Array.range(0, k)
      val codes = pq.quantizers.map { _ =>
        coder.buildCode(indices)
      }
      val em = EncodedMatrix(coder)(codes)
      val m = pq.decode(em)
      Vectors.subvectors(m, pq.quantizers.size)
        .zip(pq.quantizers.map(_.clusters))
        .foreach { case (vectors, kmeans) =>
          val centroids = Vectors(Matrix(k, kmeans.dimension, kmeans.centroids))
          assert(vectors == centroids)
        }
    }
  }

  test("encode selects closest encoding") {
    case class Args(pq: ProductQuantizer, p: Array[Float], em: EncodedMatrix)
    val genArgs = for {
      pq <- genProductQuantizer
      p <- genPoint(pq.dimension)
      em <- genEncodings(pq)
    } yield Args(pq, p, em)

    forAll(genArgs) { case Args(pq, p, em) =>
      val m1 = Matrix(1, p.length, Array(p))
      val p0 = pq.decode(pq.encode(m1).unsafeRunSync()).data(0)
      val d = MathUtils.distance(p, p0)
      // We expect p0 to be the closest encoding to p. We just
      // check against random encodings. This may not catch very
      // subtle errors, but it'll catch obvious issues with the
      // encoding algorithm.
      pq.decode(em).data.foreach { rand =>
        val randDist = MathUtils.distance(p, rand)
        assert(d <= randDist)
      }
    }
  }
}
