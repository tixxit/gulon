package net.tixxit.gulon

import java.io.{BufferedReader, File, FileReader, Reader}
import java.lang.Float.parseFloat
import scala.collection.mutable.ArrayBuffer

import cats.Monad
import cats.effect.IO
import cats.implicits._

sealed trait WordVectors {
  import WordVectors.{Indexed, Unindexed}

  def word(i: Int): String
  def vectors: Matrix

  def size: Int = vectors.rows

  def indexed: WordVectors.Indexed = this match {
    case Unindexed(keys, _) =>
      val indices = Array.range(0, size).sortBy(keys(_))
      val sortedKeys = indices.map(keys(_))
      val keyIndex = KeyIndex.unsafeSortedWordList(sortedKeys)
      val sortedData = indices.map(vectors.data(_))
      Indexed(keyIndex, vectors.copy(data = sortedData))
    case indexed @ Indexed(_, _) =>
      indexed
  }
}

object WordVectors {
  case class Unindexed(
    keys: Vector[String],
    vectors: Matrix
  ) extends WordVectors {
    def word(i: Int): String = keys(i)
  }

  case class Indexed(
    keys: KeyIndex,
    vectors: Matrix
  ) extends WordVectors {
    def word(i: Int): String = keys(i)
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

  def readWord2Vec(reader: Reader, report: Reporter = emptyReporter): IO[Unindexed] =
    IO.suspend {
      val vecs = new ArrayBuffer[Array[Float]]()
      val words = Vector.newBuilder[String]

      var chars: Long = 0
      val addLine: (String, Array[Float]) => Unit = { (word, vec) =>
        chars += word.length
        words += word
        vecs += vec
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

  def readWord2VecFile(file: File, report: Reporter = emptyReporter): IO[Unindexed] =
    IO.delay(new BufferedReader(new FileReader(file)))
      .bracket(readWord2Vec(_, report))(reader => IO.delay(reader.close()))

  def readWord2VecPath(path: String, report: Reporter = emptyReporter): IO[Unindexed] =
    readWord2VecFile(new File(path), report)

  // Goa: 50MB ann for 1M word vectors - 50 bytes!

  // Step 1:
  //  Make a compressed index -> String lookup.
  //   - build(TraversableOnce[CharSequence]) -> StringIndex
  //   - unsafeLookup(index, buf): Unit
  //   - Store trie with bonsai + String
  //   - Lookup: rank(id), traverse upwards, append char if

  // Idea?
  //  - use 64-bit hash to do word -> code lookups
  //  - store words in a trie structure
  //  - match codes with index into tail of trie structure
  //  - reconstruct words to present results
  //  - can use left-child right-sibling binary tree encoding + bonsai
  // Size?
  //  - N * (8 + 4 + m) + ???
}
