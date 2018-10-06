package net.tixxit.gulon

import java.io.{BufferedReader, File, FileReader, Reader}
import java.lang.Float.parseFloat
import scala.collection.mutable.ArrayBuffer

trait WordVectors {
  def word(i: Int): String
  def vectors: Matrix
  def size: Int = vectors.rows
}

object WordVectors {
  case class FlatWordVectors(
    keys: Array[String],
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

  def readWord2Vec(reader: Reader): FlatWordVectors = {
    val vecs = new ArrayBuffer[Array[Float]]()
    val words = new ArrayBuffer[String]()

    var chars: Long = 0
    val addLine: (String, Array[Float]) => Unit = { (word, vec) =>
      chars += word.length
      words += word
      vecs += vec
    }
    val (size, dimension) = readHeader(reader)
    var i = 0
    while (i < size) {
      readFast(reader, dimension)(addLine)
      if (i % 10000 == 0) {
        val len = if (i == 0) "?" else ((chars + i - 1) / i).toString
        println(f"${(i * 100f) / size.toFloat}%.2f%% : $len mean length")
      }
      i += 1
    }
    FlatWordVectors(words.toArray, Matrix(size, dimension, vecs.toArray))
  }

  def readWord2VecFile(file: File): FlatWordVectors = {
    val reader = new BufferedReader(new FileReader(file))
    try {
      readWord2Vec(reader)
    } finally {
      reader.close()
    }
  }

  def readWord2VecPath(path: String): FlatWordVectors =
    readWord2VecFile(new File(path))

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
