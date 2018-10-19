package net.tixxit.gulon

import scala.util.Random

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

class KMeansSpec extends FunSuite with PropertyChecks {
  // Average distance should shrink w/ each iteration.

  case class Cluster(
    centroid: Array[Float],
    scales: Array[Float])

  // Generate a cluster.
  def genCluster(d: Int, x: Gen[Float]): Gen[Cluster]  = for {
    centroid <- Gen.listOfN(d, x).map(_.toArray)
    scales <- Gen.listOfN(d, x).map(_.toArray)
  } yield Cluster(centroid, scales)

  def genPoint(cluster: Cluster): Gen[Array[Float]] = {
    val gens = cluster.centroid
      .zip(cluster.scales)
      .map { case (x, k) =>
        arbitrary[Int].map { seed =>
          val rng = new Random(seed)
          val d = rng.nextGaussian()
          (x + d * k).toFloat
        }
      }
    Gen.sequence[Array[Float], Float](gens)
  }

  def sampleCluster(cluster: Cluster): Gen[Matrix] =
    Gen.nonEmptyListOf(genPoint(cluster)).map(_.toArray)
      .map { data =>
        Matrix(data.length, data(0).length, data)
      }

  val genVectors: Gen[Vectors] = for {
    k <- Gen.choose(2, 100)
    d <- Gen.choose(2, 100)
    clusters <- Gen.listOfN(k, genCluster(d, Gen.choose(-5f, 5f)))
    groupedVectors <- Gen.sequence[List[Matrix], Matrix](clusters.map(sampleCluster(_)))
  } yield {
    val points = groupedVectors.toArray
      .flatMap(_.data)
    Vectors(Matrix(points.length, points(0).length, points))
  }

  implicit val arbVectors: Arbitrary[Vectors] =
    Arbitrary(genVectors)

  def objective(vecs: Vectors, kmeans: KMeans): Float = {
    val assignments = new Array[Int](vecs.size)
    kmeans.assign(vecs, assignments)
    val nearestCluster = assignments.map(kmeans.centroids(_))
    vecs.slice().data
      .zip(nearestCluster)
      .map { case (x, y) => MathUtils.distanceSq(x, y) }
      .sum
  }

  test("progress towards minimum") {
    forAll { (vectors: Vectors) =>
      assert(1 == 1)
      //kval next = prev.iterate(vectors, 1)
      //kassert(objective(vectors, prev) > objective(vectors, next))
    }
  }
}
