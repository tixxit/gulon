package net.tixxit.gulon

/**
 * An index into a large set of string keys.
 */
sealed trait KeyIndex extends Seq[String]

object KeyIndex {
  def apply(index: Vector[String]): KeyIndex =
    SimpleKeyIndex(index)

  case class SimpleKeyIndex(index: Vector[String]) extends KeyIndex {
    def length: Int = index.size
    def apply(i: Int): String = index(i)
    def iterator: Iterator[String] = index.iterator
  }
}
