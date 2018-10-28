package net.tixxit.gulon

final class TopKHeap(
  val keys: Array[Int],
  val values: Array[Float]) {
  var size: Int = 0

  private def parent(i: Int): Int = (i - 1) / 2
  private def leftChild(i: Int): Int = 2 * i + 1
  private def rightChild(i: Int): Int = 2 * i + 2

  private def swap(i: Int, j: Int): Unit = {
    val tmpKey = keys(i)
    val tmpValue = values(i)
    keys(i) = keys(j)
    values(i) = values(j)
    keys(j) = tmpKey
    values(j) = tmpValue
  }

  private def percolateUp(i: Int): Unit =
    if (i > 0) {
      val p = parent(i)
      if (values(i) > values(p)) {
        swap(i, p)
        percolateUp(p)
      }
    }

  private def percolateDown(i: Int): Unit = {
    var top = i
    val lc = leftChild(i)
    val rc = rightChild(i)
    if (lc < size && values(top) < values(lc))
      top = lc
    if (rc < size && values(top) < values(rc))
      top = rc
    if (top != i) {
      swap(i, top)
      percolateDown(top)
    }
  }

  def isEmpty: Boolean = size == 0

  def delete(): Unit = {
    if (size <= 0) {
      throw new IllegalStateException("heap is empty")
    }
    size -= 1
    keys(0) = keys(size)
    values(0) = values(size)
    percolateDown(0)
  }

  def update(k: Int, v: Float): Unit = {
    if ((size == keys.length) && (values(0) > v)) {
      delete()
    }
    if (size < keys.length) {
      keys(size) = k
      values(size) = v
      percolateUp(size)
      size += 1
    }
  }
}

object TopKHeap {
  def apply(k: Int): TopKHeap = new TopKHeap(new Array[Int](k), new Array[Float](k))
}
