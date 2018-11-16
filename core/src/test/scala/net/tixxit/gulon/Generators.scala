package net.tixxit.gulon

import scala.util.Random

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

object Generators {
  case class Cluster(
    centroid: Array[Float],
    scales: Array[Float])

  // Generate a cluster.
  def genCluster(d: Int, x: Gen[Float]): Gen[Cluster]  = for {
    centroid <- Gen.listOfN(d, x).map(_.toArray)
    scales <- Gen.listOfN(d, x).map(_.toArray)
  } yield Cluster(centroid, scales)

  def genPointFromCluster(cluster: Cluster): Gen[Array[Float]] = {
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
    Gen.nonEmptyListOf(genPointFromCluster(cluster)).map(_.toArray)
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

  def genPoint(dim: Int): Gen[Array[Float]] =
    Gen.listOfN(dim, Gen.choose(-1f, 1f)).map(_.toArray)

  def genKMeans(dimension: Int, k: Int): Gen[KMeans] = for {
    centroids <- Gen.listOfN(k, genPoint(dimension))
  } yield KMeans(dimension, centroids.toArray)

  def genProductQuantizer: Gen[ProductQuantizer] = for {
    // Number of quantizers
    numQuantizers <- Gen.sized(n => Gen.choose(1, math.max(1, n)))
    // Dimension of the quantizers.
    quantizerDimension <- Gen.choose(1, 4)//16)
    // Number of clusters per quantizer.
    numClusters <- Gen.choose(1, 100)//1 << 16)
    quantizers0 <- Gen.nonEmptyListOf(genKMeans(quantizerDimension, numClusters))
  } yield {
    val quantizers = quantizers0.zipWithIndex
      .map { case (kmeans, i) =>
        ProductQuantizer.Quantizer(i * quantizerDimension, kmeans)
      }
    ProductQuantizer(numClusters, quantizers.toVector)
  }

  def genMatrix(dimension: Int): Gen[Matrix] =
    Gen.nonEmptyListOf(genPoint(dimension))
      .map { points =>
        Matrix(points.size, dimension, points.toArray)
      }

  val genProductQuantizerWithMatrix: Gen[(ProductQuantizer, Matrix)] =
    for {
      pq <- genProductQuantizer
      m <- genMatrix(pq.dimension)
    } yield (pq, m)

  def genEncodings(pq: ProductQuantizer): Gen[EncodedMatrix] =
    Gen.sized { n =>
      Gen.choose(1, math.max(1, n)).flatMap { len =>
        val coder = pq.coderFactory(len)
        val genQuantizerCode: Gen[coder.Code] =
          Gen.listOfN(len, Gen.choose(0, pq.numClusters - 1))
            .map { indices => coder.buildCode(indices.toArray) }
        Gen.listOfN(pq.quantizers.size, genQuantizerCode)
          .map { codes =>
            EncodedMatrix(coder)(codes.toVector)
          }
      }
    }

  def genPQIndex: Gen[Index.PQIndex] = for {
    pq <- genProductQuantizer
    em <- genEncodings(pq)
  } yield Index.PQIndex(pq, em)

  def genSortedKeyIndexOfN(n: Int): Gen[KeyIndex.Sorted] =
    Gen.containerOfN[Set, String](n, Gen.identifier).map { keys =>
      KeyIndex.Sorted(keys.toList.sorted.toArray)
    }

  def genMetric: Gen[Metric] = Gen.oneOf(Metric.L2, Metric.Cosine)

  def genSortedIndex: Gen[Index.SortedIndex] = for {
    pqIndex <- genPQIndex
    keyIndex <- genSortedKeyIndexOfN(pqIndex.data.length)
    metric <- genMetric
  } yield Index.SortedIndex(keyIndex, pqIndex, metric)
}
