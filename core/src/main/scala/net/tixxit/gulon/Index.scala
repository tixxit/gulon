package net.tixxit.gulon

import java.io.InputStream

import cats.effect.IO

/**
 * Approximate nearest neighbour index.
 */
sealed trait Index {

  /**
   * Return the approximate `k` nearest neighbours to each query
   * point in `vectors`.
   */
  def batchQuery(k: Int, vectors: Matrix): Vector[Index.Result]

  /**
   * Return the approximate `k` nearest neighbours the query
   * `vector`.
   */
  def query(k: Int, vector: Array[Float]): Index.Result =
    batchQuery(k, Matrix(1, vector.length, Array(vector)))(0)
}

object Index {
  def apply(keyIndex: KeyIndex, vecIndex: VectorIndex): Index =
    IndexImpl(keyIndex, vecIndex)

  def read(open: IO[InputStream]): IO[Index] =
    open.bracket(is => IO.delay(protobuf.Index.parseFrom(is))) { is => IO.delay(is.close()) }
      .map(fromProtobuf(_))

  def toProtobuf(index: Index): protobuf.Index = {
    val IndexImpl(keyIndex, vecIndex) = index
    val KeyIndex.SimpleKeyIndex(words) = keyIndex
    val PQIndex(pq, data) = vecIndex
    val wordList = protobuf.WordList(words)
    val pqIndex = protobuf.PQIndex(
      ProductQuantizer.toProtobuf(pq),
      EncodedMatrix.toProtobuf(data))
    protobuf.Index(
      protobuf.Index.KeyIndex.SimpleKeyIndex(wordList),
      protobuf.Index.VectorIndex.ProductQuantizerIndex(pqIndex))
  }

  def fromProtobuf(index: protobuf.Index): Index = {
    val keyIndex = index.keyIndex match {
      case protobuf.Index.KeyIndex.SimpleKeyIndex(wordList) =>
        KeyIndex.SimpleKeyIndex(wordList.words)
      case protobuf.Index.KeyIndex.Empty =>
        throw new IllegalArgumentException("keyIndex empty")
    }
    val vecIndex = index.vectorIndex match {
      case protobuf.Index.VectorIndex.ProductQuantizerIndex(pqIndex) =>
        PQIndex(
          ProductQuantizer.fromProtobuf(pqIndex.productQuantizer),
          EncodedMatrix.fromProtobuf(pqIndex.data))
      case protobuf.Index.VectorIndex.Empty =>
        throw new IllegalArgumentException("vectorIndex empty")
    }
    IndexImpl(keyIndex, vecIndex)
  }

  trait Result extends Seq[(String, Float)] {
    def getKey(i: Int): String
    def getDistance(i: Int): Float
    def apply(i: Int): (String, Float) = getKey(i) -> getDistance(i)
    def iterator: Iterator[(String, Float)] =
      Iterator.tabulate(length)(apply)
  }

  private case class FlatResult(keys: Array[String],
                                values: Array[Float]) extends Result {
    def length: Int = keys.length
    def getKey(i: Int): String = keys(i)
    def getDistance(i: Int): Float = values(i)
  }

  /**
   * A low-level index on vectors, keyed by index.
   */
  sealed trait VectorIndex {
    def batchQuery(k: Int, vectors: Matrix): Vector[TopKHeap]
  }

  case class IndexImpl(keyIndex: KeyIndex,
                       vectorIndex: VectorIndex) extends Index {
    def batchQuery(k: Int, vectors: Matrix): Vector[Index.Result] =
      vectorIndex.batchQuery(k, vectors)
        .map { heap =>
          // We destroy the heap, but since we also created it,
          // this is fine.
          val keys = new Array[String](heap.size)
          val values = new Array[Float](heap.size)
          var i = keys.length - 1
          while (i >= 0) {
            keys(i) = keyIndex(heap.keys(0))
            values(i) = heap.values(0)
            heap.delete()
            i -= 1
          }
          FlatResult(keys, values)
        }
  }

  /**
   * A prepared query contains the precomputed distances from a set of query
   * points to the centroids in each of the quantizers in a product quantizer.
   * Computing the approximate distance from a query point to a point in the
   * index only requires summing up a handful of floats.
   *
   * The `quantizerDistances` outer array has 1 entry for each query. Each
   * query is a 2D Float array, with a row for each quantizer, and a column for
   * each centroid. So, `quantizerDistances(k)(q)(i)` corresponds to the
   * distance from query point `k` to the centroid `i` in quantizer `q`.
   */
  case class PreparedQuery(quantizerDistances: Array[Array[Array[Float]]])

  def prepareQuery(productQuantizer: ProductQuantizer, queries: Array[Array[Float]]): PreparedQuery = {
    val distances = Array.fill(queries.length)(
      Array.fill(productQuantizer.quantizers.size)(
        new Array[Float](productQuantizer.numClusters)))

    productQuantizer.quantizers
      .zipWithIndex
      .foreach { case (quantizer, j) =>
        val centroids = quantizer.clusters.centroids
        val offset = quantizer.from
        var i = 0
        while (i < centroids.length) {
          val c = centroids(i)
          var q = 0
          while (q < queries.length) {
            val query = queries(q)
            var k = 0
            var sumSq = 0f
            while (k < c.length) {
              val d = query(k + offset) - c(k)
              sumSq += d * d
              k += 1
            }
            distances(q)(j)(i) = sumSq
            q += 1
          }
          i += 1
        }
        distances
      }
    PreparedQuery(distances)
  }

  case class PQIndex(productQuantizer: ProductQuantizer,
                     data: EncodedMatrix) extends VectorIndex {
    private def distances(preparedQuery: Array[Array[Float]],
                          from: Int, len: Int): Array[Float] = {
      val ds = new Array[Float](len)
      var j = 0
      while (j < data.encodings.size) {
        val qds = preparedQuery(j)
        val code = data.encodings(j)
        var k = 0
        while (k < len) {
          val c = data.coder.getIndex(code, from + k)
          ds(k) += qds(c)
          k += 1
        }
        j += 1
      }
      ds
    }

    def batchQuery(k: Int, vectors: Matrix): Vector[TopKHeap] = {
      val preparedQuery = prepareQuery(productQuantizer, vectors.data).quantizerDistances
      val heaps = Vector.fill(preparedQuery.length)(TopKHeap(k))
      var i = 0
      val len = data.length
      while (i < len) {
        val batchSize = math.min(4096, len - i)
        var k = 0
        while (k < heaps.size) {
          // ds has all the distances for query q.
          val ds = distances(preparedQuery(k), i, batchSize)
          val heap = heaps(k)
          var j = 0
          while (j < ds.length) {
            heap.update(i + j, ds(j))
            j += 1
          }
          k += 1
        }
        i += batchSize
      }
      heaps
    }
  }
}
