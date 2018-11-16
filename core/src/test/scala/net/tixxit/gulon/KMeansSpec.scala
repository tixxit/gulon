package net.tixxit.gulon

import scala.util.Random

import cats.effect.{ContextShift, IO}
import cats.effect.concurrent.Ref
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

  case class GeneratedVectors(
    vectors: Vectors,
    clusters: Array[Array[Float]])

  val genVectors: Gen[GeneratedVectors] = for {
    k <- Gen.choose(2, 15)
    d <- Gen.choose(2, 30)
    sz <- Gen.size
    clusterSize = math.max(sz / k, 5)
    clusters <- Gen.listOfN(k, Gen.resize(clusterSize, genCluster(d, Gen.choose(-5f, 5f))))
    groupedVectors <- Gen.sequence[List[Matrix], Matrix](clusters.map(sampleCluster(_)))
  } yield {
    val points = groupedVectors.toArray
      .flatMap(_.data)
    val vectors = Vectors(Matrix(points.length, points(0).length, points))
    GeneratedVectors(vectors, clusters.map(_.centroid).toArray)
  }

  implicit val arbGeneratedVectors: Arbitrary[GeneratedVectors] =
    Arbitrary(genVectors)

  implicit val arbVectors: Arbitrary[Vectors] =
    Arbitrary(genVectors.map(_.vectors))

  def objective(vecs: Vectors, kmeans: KMeans): Float = {
    val assignments = new Array[Int](vecs.size)
    kmeans.assign(vecs, assignments)
    val nearestCluster = assignments.map(kmeans.centroids(_))
    vecs.slice().data
      .zip(nearestCluster)
      .map { case (x, y) => MathUtils.distanceSq(x, y) }
      .sum
  }

  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

  test("computeClusters converges") {
    forAll(genVectors) { case GeneratedVectors(vectors, clusters) =>
      val k0 = KMeans.init(clusters.size, vectors)
      def report(ref: Ref[IO, Boolean])(pr: KMeans.ProgressReport): IO[Unit] =
        ref.set(pr.converged)

      val convergedIO = for {
        convergedRef <- Ref[IO].of(false)
        config = KMeans.Config(clusters.length, 100, 0, report(convergedRef)(_))
        last <- KMeans.computeClusters(vectors, config)
        converged <- convergedRef.get
      } yield converged

      assert(convergedIO.unsafeRunSync())
    }
  }

  test("iterate progresses towards minimum") {
    forAll(genVectors) { case GeneratedVectors(vectors, clusters) =>
      val k0 = KMeans.init(clusters.size, vectors)
      val o0 = objective(vectors, k0)
      val k1 = k0.iterate(vectors, 1)
      val o1 = objective(vectors, k1)
      assert(o0 >= o1)
      val k2 = k1.iterate(vectors, 3)
      val o2 = objective(vectors, k2)
      assert(o1 >= o2)
      val k3 = k2.iterate(vectors, 7)
      val o3 = objective(vectors, k3)
      assert(o2 >= o3)
      val k4 = k3.iterate(vectors, 11)
      val o4 = objective(vectors, k4)
      assert(o3 >= o4)
    }
  }

  test("does not get stuck when clusters are not distinct") {
    forAll(genVectors) { case GeneratedVectors(vectors, clusters) =>
      // All vectors assigned to cluster 0.
      val assignments = new Array[Int](vectors.size)
      val k0 = KMeans.fromAssignment(clusters.length, vectors.dimension, vectors, assignments)
      val k1 = k0.iterate(vectors, 1)
      val assignments1 = k1.assign(vectors)
      if (assignments.toVector == assignments1.toVector) {
        succeed
      } else {
        assert(objective(vectors, k0) > objective(vectors, k1))
      }
    }
  }
}
