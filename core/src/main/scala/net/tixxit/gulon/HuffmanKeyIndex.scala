package net.tixxit.gulon

import scala.collection.mutable.PriorityQueue
import java.io.ByteArrayOutputStream

case class HuffmanKeyIndex(
  dictionary: HuffmanKeyIndex.Dictionary,
  data: Array[Byte]//,
  //mask: IndexedBitSet
) extends KeyIndex {
  def length: Int = ???
  def apply(i: Int): String = ???
  def iterator: Iterator[String] = ???
}

object HuffmanKeyIndex {
  def calcSize(source: Iterable[String]): Int = {
    // TODO:
    // Count vectorize & norm the chars.
    // Run k-means with k=256 on the resulting vectors.
    // Make 256 dictionaries from assignments.
    // Use 256 dictionaries.
    // Use first byte of every word to index dictionary.
    // A lot of wasted space to index dictionaries.
    val counts = charCounts(source)
    val dictionary = Dictionary.fromCounts(counts)
    val lookup = buildLookup(dictionary)
    val writer = new BitWriter()
    source.foreach { s =>
      val bytes = s.getBytes("utf-8")
      var i = 0
      while (i < bytes.length) {
        writer.write(lookup(bytes(i) & 0xFF))
        i += 1
      }
      writer.write(lookup(256))
      writer.realign()
    }
    val bytes = writer.result()
    bytes.length + 257 * 2
  }

  def apply(source: Iterable[String]): HuffmanKeyIndex = {
    val len = calcSize(source)
    println(s"Huffman size: ${len}")
    ???
  }

  def try2(source: Iterable[String]): HuffmanKeyIndex = {
    val bytes = source.toList.sorted.grouped(10000)
      .map { group =>
        calcSize(group)
      }
      .sum
    println(s"chunked: ${bytes}")
    ???
  }

  private case class Code(code: Int, len: Int)

  private class BitWriter() {
    private[this] val baos = new ByteArrayOutputStream()
    private[this] var byte: Int = 0
    private[this] var pos: Int = 0
    private[this] var bitOffset: Int = 0

    def write(code: Code): Unit =
      write(code.code, code.len)

    def write(code: Int, len: Int): Unit = {
      var k = 0
      var c = code
      while (k < len) {
        if (bitOffset == 8) {
          realign()
        }

        byte |= ((c & 1) << bitOffset)
        c = c >>> 1
        bitOffset += 1
        k += 1
      }
    }

    def realign(): Unit = {
      baos.write(byte)
      byte = 0
      pos += 1
      bitOffset = 0
    }

    def result(): Array[Byte] = {
      if (bitOffset != 0)
        realign()
      baos.toByteArray()
    }
  }

  sealed trait Dictionary {
    def count: Long
  }
  private case class Split(zero: Dictionary, one: Dictionary, count: Long) extends Dictionary
  private case class Leaf(char: Int, count: Long) extends Dictionary

  private object Dictionary {
    implicit val dictionaryOrdering: Ordering[Dictionary] =
      Ordering.by(_.count)

    def fromCounts(freqs: Array[Long]): Dictionary = {
      val dicts = PriorityQueue(freqs.zipWithIndex.collect {
        case (count, char) if count > 0 =>
          Leaf(char, count): Dictionary
      }: _*)(dictionaryOrdering.reverse)
      while (dicts.size > 1) {
        val lhs = dicts.dequeue()
        val rhs = dicts.dequeue()
        dicts.enqueue(Split(lhs, rhs, lhs.count + rhs.count))
      }
      dicts.dequeue()
    }
  }

  private def buildLookup(dict: Dictionary): Array[Code] = {
    val codes: Array[Code] = new Array[Code](257)
    def build(dict: Dictionary, code: Int, len: Int): Unit = dict match {
      case Split(zero, one, _) =>
        build(zero, code, len + 1)
        build(one, code | (1 << len), len + 1)
      case Leaf(i, _) =>
        codes(i) = Code(code, len)
    }
    build(dict, 0, 0)
    codes
  }

  private def charCounts(source: Iterable[String]): Array[Long] = {
    val freqs: Array[Long] = new Array[Long](257)
    source.foreach { s =>
      val bytes = s.getBytes("utf-8")
      var i = 0
      while (i < bytes.length) {
        freqs(bytes(i) & 0xFF) += 1
        i += 1
      }
      freqs(256) += 1
    }
    freqs
  }
}
