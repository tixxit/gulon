package net.tixxit.gulon

import scala.util.Random

import cats.effect.{ContextShift, IO}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Arbitrary.arbitrary

object Generators {
  implicit val contextShift: ContextShift[IO] =
    IO.contextShift(scala.concurrent.ExecutionContext.Implicits.global)

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

  val genVectors: Gen[GeneratedVectors] =
    Gen.choose(2, 30).flatMap(genVectorsOfN(_))

  def genVectorsOfN(d: Int): Gen[GeneratedVectors] = for {
    k <- Gen.choose(2, 15)
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
    numClusters <- Gen.choose(1, 100)
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

  def genSetOfN[A](n: Int, gen: Gen[A]): Gen[Set[A]] = {
    def loop(acc: Set[A]): Gen[Set[A]] =
      if (acc.size == n) Gen.const(acc)
      else gen.flatMap { a => loop(acc + a) }

    loop(Set.empty)
  }

  def genSortedKeyIndexOfN(n: Int): Gen[KeyIndex.Sorted] =
    genSetOfN[String](n, Gen.identifier).map { keys =>
      KeyIndex.Sorted(keys.toList.sorted.toArray)
    }

  def genMetric: Gen[Metric] = Gen.oneOf(Metric.L2, Metric.Cosine)

  def normalize(metric: Metric, data: Array[Array[Float]]): Array[Array[Float]] =
    if (metric.normalized) data.map(MathUtils.normalize(_))
    else data

  def makeWordVectors(metric: Metric, keys: Array[String], matrix: Matrix): WordVectors.Sorted = {
    val Matrix(rows, cols, data) = matrix
    WordVectors.Sorted(keys, Matrix(rows, cols, normalize(metric, data)))
  }

  def genSortedIndex: Gen[Index.SortedIndex] = for {
    metric <- genMetric
    pq <- genProductQuantizer
    vectors <- genMatrix(pq.dimension)
    if vectors.rows > 0
    keyIndex <- genSortedKeyIndexOfN(vectors.rows)
    wordVectors = makeWordVectors(metric, keyIndex.keys, vectors)
  } yield Index.sorted(wordVectors, pq, metric).unsafeRunSync()

  def genGroupedKeyIndexOfN(n: Int): Gen[KeyIndex.Grouped] = for {
    keys <- Gen.containerOfN[Set, String](n, Gen.identifier).map(_.toVector)
    offsets <- if (keys.length < 2) Gen.const(Set.empty[Int])
               else Gen.containerOf[Set, Int](Gen.choose(1, keys.length - 1))
  } yield {
    val orderedOffsets = offsets.toList.sorted
    val orderedKeys = (0 +: orderedOffsets :+ keys.length)
      .sliding(2)
      .flatMap { case from :: until :: Nil =>
        (from until until).map(keys(_)).sorted
      }
      .toArray
    KeyIndex.Grouped(orderedKeys, orderedOffsets.toArray)
  }

  def genGroupedIndexStrategy(size: Int, partitions: Int): Gen[Index.GroupedIndex.Strategy] =
    Gen.oneOf(
      Gen.choose(2, math.max(2, partitions)).map(Index.GroupedIndex.Strategy.LimitGroups(_)),
      Gen.choose(2, math.max(2, size)).map(Index.GroupedIndex.Strategy.LimitVectors(_)))

  def genGroupedIndex: Gen[Index.GroupedIndex] = for {
    metric <- genMetric
    pq <- genProductQuantizer
    GeneratedVectors(vectors, clusters) <- genVectorsOfN(pq.dimension)
    keyIndex <- genSortedKeyIndexOfN(vectors.size)
    wordVectors0 = makeWordVectors(metric, keyIndex.keys, vectors.matrix)
    kmeans = KMeans(vectors.dimension, normalize(metric, clusters))
    wordVectors = wordVectors0.grouped(kmeans).unsafeRunSync()
    strategy <- genGroupedIndexStrategy(vectors.size, wordVectors.keyIndex.groupOffsets.length)
  } yield Index.grouped(wordVectors,
                        pq,
                        metric,
                        strategy).unsafeRunSync()

  def genIndex: Gen[Index] =
    Gen.oneOf(genSortedIndex, genGroupedIndex)
}
