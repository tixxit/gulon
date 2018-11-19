package net.tixxit.gulon

import java.io.{BufferedReader, File, FileReader, Reader}
import java.lang.Float.parseFloat
import java.lang.ref.WeakReference
import java.util.Arrays
import scala.collection.mutable.{ArrayBuffer, ArrayBuilder}

import cats.Monad
import cats.effect.{ContextShift, IO}
import cats.implicits._

sealed trait WordVectors {
  import WordVectors.{Grouped, Sorted, Unindexed}

  def word(i: Int): String
  def apply(i: Int): Array[Float]

  def toMatrix: Matrix

  def size: Int
  def dimension: Int

  def grouped(clustering: KMeans)(implicit contextShift: ContextShift[IO]): IO[WordVectors.Grouped] = {
    require(clustering.k > 0, "must have at least 1 cluster")

    clustering.parAssign(Vectors(toMatrix)).map { assignments =>
      val indices = Array.range(0, size)
        .sortBy(word(_))
        .sortBy(assignments(_))
      val groupedKeys = new Array[String](indices.length)
      val groupedVecs = new Array[Array[Float]](indices.length)
      val offsetsBldr = ArrayBuilder.make[Int]()
      // Not all centroids may have points assigned to them.
      val centroidsBldr = ArrayBuilder.make[Array[Float]]()
      if (indices.length > 0) {
        var i = 0
        var prev = assignments(0)
        centroidsBldr += clustering.centroids(prev)
        while (i < indices.length) {
          val j = indices(i)
          val a = assignments(j)
          groupedKeys(i) = word(j)
          groupedVecs(i) = apply(j)
          if (prev != a) {
            offsetsBldr += i
            prev = a
            centroidsBldr += clustering.centroids(prev)
          }
          i += 1
        }
      }
      Grouped(groupedKeys,
              Matrix(size, dimension, groupedVecs),
              centroidsBldr.result(),
              offsetsBldr.result())
    }
  }

  def sorted: WordVectors.Sorted = this match {
    case indexed @ Sorted(_, _) =>
      indexed
    case _ =>
      val indices = Array.range(0, size).sortBy(word(_))
      val sortedKeys = indices.map(word(_))
      val sortedData = indices.map(apply(_))
      Sorted(sortedKeys, Matrix(size, dimension, sortedData))
  }
}

object WordVectors {
  case class Unindexed(
    keys: Vector[String],
    toMatrix: Matrix
  ) extends WordVectors {
    def dimension: Int = toMatrix.cols
    def size: Int = toMatrix.rows
    def word(i: Int): String = keys(i)
    def apply(i: Int): Array[Float] = toMatrix.data(i)
  }

  case class Sorted(
    keys: Array[String],
    toMatrix: Matrix
  ) extends WordVectors {
    def dimension: Int = toMatrix.cols
    def size: Int = toMatrix.rows
    val keyIndex: KeyIndex.Sorted =
      KeyIndex.Sorted(keys)
    def word(i: Int): String = keys(i)
    def apply(i: Int): Array[Float] = toMatrix.data(i)
  }

  case class Grouped(
    keys: Array[String],
    toMatrix: Matrix,
    centroids: Array[Array[Float]],
    offsets: Array[Int]
  ) extends WordVectors {
    def dimension: Int = toMatrix.cols
    def size: Int = toMatrix.rows
    val keyIndex: KeyIndex.Grouped =
      KeyIndex.Grouped(keys, offsets)
    def word(i: Int): String = keys(i)
    def apply(i: Int): Array[Float] = toMatrix.data(i)
    def clusterOf(i: Int): Int = {
      val k0 = Arrays.binarySearch(offsets, i)
      if (k0 < 0) -k0 - 1 else (k0 + 1)
    }

    private[this] var residualsRef: WeakReference[Matrix] =
      new WeakReference(null)

    def residuals: Matrix = synchronized {
      var ret = residualsRef.get
      if (ret == null) {
        val residuals = new Array[Array[Float]](size)
        val data = toMatrix.data
        var i = 0
        var k = -1
        var nextOffset = 0
        while (i < residuals.length) {
          while (i >= nextOffset) {
            k += 1
            nextOffset = if (k < offsets.length) offsets(k) else residuals.length
          }
          residuals(i) = MathUtils.subtract(data(i), centroids(k))
          i += 1
        }
        ret = Matrix(size, dimension, residuals)
        residualsRef = new WeakReference(ret)
      }
      ret
    }
  }

  private def readHeader(reader: Reader): (Int, Int) = {
    val bldr = new java.lang.StringBuilder()
    var c = reader.read()
    while (c != -1 && c != '\n') {
      bldr.append(c.toChar)
      c = reader.read()
    }
    val line = bldr.toString
    val Array(size, dim) = line.split(" ")
    (size.toInt, dim.toInt)
  }

  private def readFast(reader: Reader, dimension: Int)(f: (String, Array[Float]) => Unit): Boolean = {
    val bldr = new java.lang.StringBuilder()
    val vec = new Array[Float](dimension)
    var c = reader.read()
    var i = 0
    var k = 0
    // a 0 1 2
    while (c != -1 && c != '\n') {
      bldr.append(c.toChar)
      if (c == ' ') {
        vec(k) = i
        k += 1
      }
      c = reader.read()
      i += 1
    }
    val line = bldr.toString
    val word = line.substring(0, vec(0).toInt)
    i = 0
    while (i < vec.length) {
      val start = vec(i).toInt + 1
      val scalar =
        if ((i + 1) == vec.length) line.substring(start)
        else line.substring(start, vec(i + 1).toInt)
      vec(i) = parseFloat(scalar)
      i += 1
    }
    f(word, vec)
    c != -1
  }

  case class ProgressReport(dimension: Int, linesRead: Int, linesTotal: Int, charsPerWord: Float) {
    def percentageRead: Float = linesRead.toFloat / linesTotal
    def sizeEstimate: Long = {
      val words = (2 * charsPerWord * linesRead).toLong
      val vecs = (4 * dimension * linesRead).toLong
      words + vecs
    }
  }

  type Reporter = ProgressReport => IO[Unit]
  val emptyReporter: Reporter = _ => IO.pure(())

  private[this] val chunkSize: Int = 10000

  def readWord2Vec(reader: Reader,
                   normalize: Boolean = false,
                   report: Reporter = emptyReporter): IO[Unindexed] =
    IO.suspend {
      val vecs = new ArrayBuffer[Array[Float]]()
      val words = Vector.newBuilder[String]

      var chars: Long = 0
      val addLine: (String, Array[Float]) => Unit =
        if (normalize) {
          { (word, vec) =>
            chars += word.length
            words += word
            vecs += MathUtils.normalize(vec)
          }
        } else {
          { (word, vec) =>
            chars += word.length
            words += word
            vecs += vec
          }
        }
      val (size, dimension) = readHeader(reader)

      def readChunk(len: Int): IO[Int] = IO.delay {
        var i = 0
        while (i < len) {
          readFast(reader, dimension)(addLine)
          i += 1
        }
        len
      }

      Monad[IO].tailRecM(0) {
        case i if i < size =>
          val len = math.min(size - i, chunkSize)
          for {
            _ <- readChunk(len)
            n = i + len
            _ <- report(ProgressReport(dimension, n, size, chars.toFloat / n))
          } yield Left(n)
        case _ =>
          report(ProgressReport(dimension, size, size, chars.toFloat / size))
            .as(Right(Unindexed(words.result(), Matrix(size, dimension, vecs.toArray))))
      }
    }

  def readWord2VecFile(file: File,
                       normalize: Boolean = false,
                       report: Reporter = emptyReporter): IO[Unindexed] =
    IO.delay(new BufferedReader(new FileReader(file)))
      .bracket(readWord2Vec(_, normalize, report))(reader => IO.delay(reader.close()))

  def readWord2VecPath(path: String,
                       normalize: Boolean = false,
                       report: Reporter = emptyReporter): IO[Unindexed] =
    readWord2VecFile(new File(path), normalize, report)
}
