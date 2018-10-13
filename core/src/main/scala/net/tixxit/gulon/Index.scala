package net.tixxit.gulon

trait KeyIndex extends Seq[String]

object KeyIndex {
  def apply(index: Vector[String]): KeyIndex =
    SimpleKeyIndex(index)

  case class SimpleKeyIndex(index: Vector[String]) extends KeyIndex {
    def length: Int = index.size
    def apply(i: Int): String = index(i)
    def iterator: Iterator[String] = index.iterator
  }
}

trait Index {
  def query(vectors: Vectors): List[Index.Result]
}

object Index {
  case class Result(key: String, approximateDistance: Float)
}
