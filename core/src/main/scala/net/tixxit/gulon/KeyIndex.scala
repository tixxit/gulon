package net.tixxit.gulon

/**
 * An index into a large set of string keys.
 */
sealed trait KeyIndex extends Seq[String] {
  def lookup(key: String): Option[Int]
}

object KeyIndex {

  /** Requires words to be sorted. */
  def unsafeSortedWordList(words: Array[String]): KeyIndex =
    SortedWordList(words)

  case class SortedWordList private (index: Array[String]) extends KeyIndex {
    def length: Int = index.length
    def apply(i: Int): String = index(i)
    def iterator: Iterator[String] = index.iterator
    def lookup(key: String): Option[Int] = {
      val i = java.util.Arrays.binarySearch(index.asInstanceOf[Array[java.lang.Object]], key)
      if (i >= 0) Some(i)
      else None
    }
  }
}
