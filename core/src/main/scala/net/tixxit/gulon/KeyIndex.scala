package net.tixxit.gulon

import java.lang.{Object => JObject}
import java.util.Arrays

/**
 * An index into a large set of string keys.
 */
sealed trait KeyIndex extends Seq[String] {
  def lookup(key: String): Option[Int]
}

object KeyIndex {
  case class Sorted(keys: Array[String]) extends KeyIndex {
    def length: Int = keys.length
    def apply(i: Int): String = keys(i)
    def iterator: Iterator[String] = keys.iterator
    def lookup(key: String): Option[Int] = {
      val i = Arrays.binarySearch(keys.asInstanceOf[Array[JObject]], key)
      if (i >= 0) Some(i)
      else None
    }
    override def equals(that: Any): Boolean = that match {
      case (that: Sorted) => ArrayUtils.shallowEquals(keys, that.keys)
      case _ => false
    }
    override def hashCode: Int = (getClass, ArrayUtils.shallowHashCode(keys)).hashCode
  }

  case class Grouped(
    keys: Array[String],
    groupOffsets: Array[Int]
  ) extends KeyIndex {
    def length: Int = keys.length
    def apply(i: Int): String = keys(i)
    def iterator: Iterator[String] = keys.iterator
    def lookup(key: String): Option[Int] = {
      var i = 0
      var from = 0
      val offsets = groupOffsets
      while (i <= offsets.length) {
        val to = if (i < offsets.length) offsets(i)
                 else keys.length
        val result = Arrays.binarySearch(
          keys.asInstanceOf[Array[JObject]], from, to, key)
        if (result >= 0) {
          return Some(result)
        }
        from = to
        i += 1
      }
      None
    }
    override def equals(that: Any): Boolean = that match {
      case (that: Grouped) =>
        ArrayUtils.shallowEquals(keys, that.keys) &&
          Arrays.equals(groupOffsets, that.groupOffsets)
      case _ => false
    }
    override def hashCode: Int =
      (getClass, ArrayUtils.shallowHashCode(keys), Arrays.hashCode(groupOffsets)).hashCode
  }
}
