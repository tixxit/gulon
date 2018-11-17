package net.tixxit.gulon

import java.io.InputStream
import java.util.Arrays

import cats.effect.{ContextShift, IO}

/**
 * Approximate nearest neighbour index.
 */
sealed trait Index {

  /** The indexed keys. */
  def keyIndex: KeyIndex

  /** The dimension of the index and query vectors. */
  def dimension: Int

  /** The number of vectors in this index. */
  def size: Int

  /**
   * Return the approximate `k` nearest neighbours to each query
   * point in `vectors`.
   */
  def batchQuery(k: Int, vectors: Matrix): Vector[Index.Result]

  /**
   * Return the approximate `k` nearest neighbours the query
   * `vector`.
   */
  def query(k: Int, vector: Array[Float]): Index.Result

  /**
   * Returns the approximate point of `word` from the index. If the word is not
   * in the index, this returns `None`.
   */
  def lookup(word: String): Option[Array[Float]]

  /**
   * Returns the k-nearest neighbours to `word` from the index. If the word is
   * not in the index, this returns `None`.
   */
  def queryByWord(k: Int, word: String): Option[Index.Result] =
    lookup(word).map(query(k, _))
}

object Index {

  /**
   * The result of a query on an ANN. These contain the words/keys of the
   * nearest neighbours, along with their approximate distance from the query
   * point. The result will be in sorted order, from closest to furthest
   * neighbour.
   */
  final class Result(keys: Array[String],
                     distances: Array[Float]) extends Seq[(String, Float)] {
    def length: Int = keys.length
    def getKey(i: Int): String = keys(i)
    def getDistance(i: Int): Float = distances(i)
    def apply(i: Int): (String, Float) = keys(i) -> distances(i)
    def iterator: Iterator[(String, Float)] =
      Iterator.tabulate(length)(apply)
    override def toString: String =
      iterator.mkString("Index.Result(", ",", ")")
    override def hashCode: Int =
      iterator.foldLeft(37)(_ * 23 + _.hashCode)
    override def equals(that: Any): Boolean = that match {
      case (that: Result) if length == that.length =>
        this.iterator.zip(that.iterator)
          .forall { case (x, y) => x == y }
      case _ => false
    }
  }

  object Result {

    /**
     * Constructs a `Result` from a `TopKHeap` heap. This destroys the heap.
     * The key index is used to lookup the string keys from the integer/index
     * stored in the heap.
     */
    def fromHeap(keyIndex: KeyIndex, heap: TopKHeap): Result = {
      val keys = new Array[String](heap.size)
      val values = new Array[Float](heap.size)
      var i = keys.length - 1
      while (i >= 0) {
        keys(i) = keyIndex(heap.keys(0))
        values(i) = heap.values(0)
        heap.delete()
        i -= 1
      }
      new Result(keys, values)
    }
  }

  /**
   * Build an approximate nearest neighbour index from a sorted set of word
   * vectors and a quantizer. This index will always do a full-scan over all
   * (encoded) vectors for queries. This is fine for small indices (~100k
   * vectors), but for larger indices, consider using a [[grouped]] index,
   * which will only search a fraction of the index per query.
   *
   * @param wordVectors the sorted word vectors to index
   * @param quantizer product quantizer to encode vectors with
   */
  def sorted(wordVectors: WordVectors.Sorted,
             quantizer: ProductQuantizer,
             metric: Metric)(implicit
             contextShift: ContextShift[IO]): IO[Index.SortedIndex] =
    quantizer.encode(wordVectors.toMatrix)
      .map { encodedData =>
        SortedIndex(KeyIndex.Sorted(wordVectors.keys), PQIndex(quantizer, encodedData), metric)
      }

  /**
   * Build an approximate nearest neighbour index from a grouped set of word
   * vectors. These word vectors are grouped by the centroid they are nearest
   * to. Rather than search the whole index, this will only search points
   * belonging to clusters that are near the query point.  The exact number of
   * clusters searched depends on the `strategy`, which can either be to search
   * a fixed number of clusters or search as many clusters needed to reach a
   * minimum number of points that must be compared.
   *
   * The [[ProductQuantizer]] used by this index is expected to be a quantizer
   * on the _residuals_ of the word vectors, after subtracting their assigned
   * centroid.
   *
   * @param wordVectors the grouped word vectors to index
   * @param quantizer product quantizer on the residual vectors
   * @param strategy the cluster search strategy
   */
  def grouped(wordVectors: WordVectors.Grouped,
              residualsQuantizer: ProductQuantizer,
              metric: Metric,
              strategy: GroupedIndex.Strategy)(implicit
              contextShift: ContextShift[IO]): IO[Index.GroupedIndex] =
    residualsQuantizer.encode(wordVectors.residuals)
      .map { encodedResiduals =>
        GroupedIndex(wordVectors.keyIndex,
                     PQIndex(residualsQuantizer, encodedResiduals),
                     metric,
                     Clustering(wordVectors.centroids),
                     strategy)
      }

  def read(open: IO[InputStream]): IO[Index] =
    open.bracket(is => IO.delay(protobuf.Index.parseFrom(is))) { is => IO.delay(is.close()) }
      .map(fromProtobuf(_))

  def toProtobuf(index: Index): protobuf.Index =
    index match {
      case SortedIndex(keyIndex, vecIndex, metric) =>
        val index = protobuf.SortedIndex(
          keyIndex.keys, PQIndex.toProtobuf(vecIndex), Metric.toProtobuf(metric))
        protobuf.Index(protobuf.Index.Implementation.Sorted(index))

      case GroupedIndex(keyIndex, vecIndex, metric, Clustering(centroids), groupedStrategy) =>
        val (strategy, limit) = groupedStrategy match {
          case GroupedIndex.Strategy.LimitGroups(n) =>
            (protobuf.GroupedIndex.Strategy.LIMIT_GROUPS, n)
          case GroupedIndex.Strategy.LimitVectors(n) =>
            (protobuf.GroupedIndex.Strategy.LIMIT_VECTORS, n)
        }
        val index = protobuf.GroupedIndex(
          keyIndex.keys,
          PQIndex.toProtobuf(vecIndex),
          Metric.toProtobuf(metric),
          centroids.map(protobuf.FloatVector(_)),
          keyIndex.groupOffsets,
          strategy,
          limit)
        protobuf.Index(protobuf.Index.Implementation.Grouped(index))
    }

  def fromProtobuf(index: protobuf.Index): Index =
    index.implementation match {
      case protobuf.Index.Implementation.Sorted(index) =>
        val keyIndex = KeyIndex.Sorted(index.sortedWords)
        val vecIndex = PQIndex.fromProtobuf(index.vectorIndex)
        SortedIndex(keyIndex, vecIndex, Metric.fromProtobuf(index.metric))

      case protobuf.Index.Implementation.Grouped(index) =>
        val keyIndex = KeyIndex.Grouped(
          index.groupedWords, index.offsets)
        val vecIndex = PQIndex.fromProtobuf(index.vectorIndex)
        val centroids = index.centroids.iterator.map(_.values).toArray
        val strategy = index.strategy match {
          case protobuf.GroupedIndex.Strategy.LIMIT_GROUPS =>
            GroupedIndex.Strategy.LimitGroups(index.limit)
          case protobuf.GroupedIndex.Strategy.LIMIT_VECTORS =>
            GroupedIndex.Strategy.LimitVectors(index.limit)
          case protobuf.GroupedIndex.Strategy.Unrecognized(_) =>
            throw new IllegalArgumentException(
              "strategy must be one of LIMIT_GROUPS or LIMIT_VECTORS")
        }
        GroupedIndex(
          keyIndex,
          vecIndex,
          Metric.fromProtobuf(index.metric),
          Clustering(centroids),
          strategy)

      case protobuf.Index.Implementation.Empty =>
        throw new IllegalArgumentException(
          "missing index implementation")
    }

  def exactNearestNeighbours(vectors: Array[Array[Float]],
                             from: Int, until: Int,
                             query: Array[Float],
                             k: Int): Array[Int] = {
    require(from <= until, s"invalid range: expected from=$from <= until=$until")
    require(until <= vectors.length, s"invalid range: expected until=$until <= vectors.length=${vectors.length}")

    val heap = TopKHeap(k)
    var i = from
    val dim = query.length
    while (i < until) {
      heap.update(i, MathUtils.distanceSq(vectors(i), query))
      i += 1
    }
    val result = new Array[Int](heap.size)
    var j = result.length - 1
    while (j >= 0) {
      result(j) = heap.delete()
      j -= 1
    }
    result
  }

  def exactNearestNeighbours(vectors: Array[Array[Float]],
                             query: Array[Float],
                             k: Int): Array[Int] =
    exactNearestNeighbours(vectors, 0, vectors.length, query, k)

  case class GroupedIndex(keyIndex: KeyIndex.Grouped,
                          vectorIndex: PQIndex,
                          metric: Metric,
                          clustering: Clustering,
                          strategy: GroupedIndex.Strategy) extends Index {
    import GroupedIndex.Strategy

    private[this] val offsets = keyIndex.groupOffsets
    private[this] val centroids = clustering.centroids

    assert(centroids.length == offsets.length + 1,
           s"${centroids.length} != ${offsets.length} + 1")

    def dimension: Int = vectorIndex.dimension
    def size: Int = keyIndex.size

    def lookup(word: String): Option[Array[Float]] =
      keyIndex.lookup(word).map { row =>
        val i = Arrays.binarySearch(offsets, row)
        val partition = if (i < 0) -i - 1 else i + 1
        val base = centroids(partition)
        val residual = vectorIndex.decode(row)
        MathUtils.add(base, residual)
      }

    def batchQuery(k: Int, vectors: Matrix): Vector[Index.Result] =
      vectors.data.iterator
        .map(query(k, _))
        .toVector

    private def getBounds(i: Int): (Int, Int) = {
      val start = if (i == 0) 0 else offsets(i - 1)
      val end = if (i == offsets.length) vectorIndex.length else offsets(i)
      (start, end)
    }

    def query(k: Int, query: Array[Float]): Index.Result = {
      val normalizedQuery =
        if (metric.normalized) MathUtils.normalize(query)
        else query
      val nn = searchSpace(normalizedQuery)
      val heap = TopKHeap(k)
      var i = 0
      while (i < nn.length) {
        val c = nn(i)
        val (from, until) = getBounds(c)
        val centroid = centroids(c)
        val residual = MathUtils.subtract(normalizedQuery, centroid)
        heap.merge(vectorIndex.query(k, residual, from, until))
        i += 1
      }
      Result.fromHeap(keyIndex, heap)
    }

    private def searchSpace(query: Array[Float]): Array[Int] =
      strategy match {
        case Strategy.LimitGroups(m) =>
          exactNearestNeighbours(centroids, query, m)
        case Strategy.LimitVectors(n) =>
          val order = exactNearestNeighbours(centroids, query, centroids.length)
          var i = 0
          var count = 0
          while (i < order.length && count < n) {
            val (from, until) = getBounds(order(i))
            count += until - from
            i += 1
          }
          Arrays.copyOf(order, i)
      }
  }

  object GroupedIndex {
    sealed abstract class Strategy extends Product with Serializable
    object Strategy {
      case class LimitGroups(count: Int) extends Strategy
      case class LimitVectors(count: Int) extends Strategy
    }
  }

  case class SortedIndex(keyIndex: KeyIndex.Sorted,
                         vectorIndex: PQIndex,
                         metric: Metric) extends Index {

    def dimension: Int = vectorIndex.dimension

    def size: Int = keyIndex.size

    def lookup(word: String): Option[Array[Float]] =
      keyIndex.lookup(word).map(vectorIndex.decode(_))

    def query(k: Int, vector: Array[Float]): Index.Result =
      batchQuery(k, Matrix(1, vector.length, Array(vector)))(0)

    private def prepare(query: Matrix): Matrix =
      if (metric.normalized) {
        Matrix(query.rows, query.cols, query.data.map { xs =>
          MathUtils.normalize(xs)
        })
      } else {
        query
      }

    def batchQuery(k: Int, vectors: Matrix): Vector[Index.Result] =
      vectorIndex
        .batchQuery(k, prepare(vectors))
        .map(Result.fromHeap(keyIndex, _))
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
                     data: EncodedMatrix) {
    def dimension: Int = productQuantizer.dimension
    def length: Int = data.length

    def decode(row: Int): Array[Float] =
      productQuantizer.decode(data(row))

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

    def query(k: Int, query: Array[Float], from: Int, until: Int): TopKHeap =
      batchQuery(k, Matrix(1, query.length, Array(query)), from, until)(0)

    def batchQuery(k: Int, vectors: Matrix): Vector[TopKHeap] =
      batchQuery(k, vectors, 0, length)

    def batchQuery(k: Int, vectors: Matrix, from: Int, until: Int): Vector[TopKHeap] = {
      require(from <= until, "expected: from <= until")
      require(from >= 0 && until <= length, "expected: from >= 0 && until <= length")
      val preparedQuery = prepareQuery(productQuantizer, vectors.data).quantizerDistances
      val heaps = Vector.fill(preparedQuery.length)(TopKHeap(k))
      var i = from
      while (i < until) {
        val batchSize = math.min(4096, until - i)
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

  object PQIndex {
    def toProtobuf(index: PQIndex): protobuf.PQIndex =
      protobuf.PQIndex(
        ProductQuantizer.toProtobuf(index.productQuantizer),
        EncodedMatrix.toProtobuf(index.data))

    def fromProtobuf(index: protobuf.PQIndex): PQIndex =
      PQIndex(
        ProductQuantizer.fromProtobuf(index.productQuantizer),
        EncodedMatrix.fromProtobuf(index.data))
  }
}
