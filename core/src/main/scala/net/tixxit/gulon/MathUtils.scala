package net.tixxit.gulon

case class SummaryStats(count: Int, mean: Float, s: Float) {
  def variance: Float = s / count
  def stdDev: Float = math.sqrt(variance).toFloat

  def ++(that: SummaryStats): SummaryStats =
    if (that.count == 0) {
      this
    } else if (this.count == 0) {
      that
    } else {
      val n = this.count + that.count
      val d = this.mean - that.mean
      SummaryStats(n,
                   this.mean + (that.count.toFloat / n) * (that.mean - this.mean),
                   this.s + that.s + d * d * this.count * that.count / n)
    }

  override def toString: String =
    s"SummaryStats(mean = $mean, stdDev = $stdDev)"
}

object SummaryStats {
  def apply(xs: TraversableOnce[Float]): SummaryStats = {
    val bldr = newBuilder()
    xs.foreach(bldr.update(_))
    bldr.result()
  }

  def zero: SummaryStats = SummaryStats(0, 0f, 0f)

  final class SummaryStatsBuilder() {
    private[this] var m = 0f
    private[this] var s = 0f
    private[this] var n = 0

    def update(x: Float): Unit = {
      n += 1
      val m0 = m
      m = m0 + (x - m0) / n
      s = s + (x - m0) * (x - m)
    }

    def result(): SummaryStats =
      SummaryStats(n, m, s)
  }

  def newBuilder(): SummaryStatsBuilder = new SummaryStatsBuilder()
}

object MathUtils {
  final def add(x: Array[Float], y: Array[Float]): Array[Float] = {
    val z = new Array[Float](x.length)
    val len = x.length
    var i = 0
    while (i < len) {
      z(i) = x(i) + y(i)
      i += 1
    }
    z
  }

  final def subtract(x: Array[Float], y: Array[Float]): Array[Float] = {
    val z = new Array[Float](x.length)
    val len = x.length
    var i = 0
    while (i < len) {
      z(i) = x(i) - y(i)
      i += 1
    }
    z
  }

  final def distanceSq(x: Array[Float], y: Array[Float]): Float = {
    var sumSq = 0f
    var i = 0
    val len = x.length
    while (i < len) {
      val dx = y(i) - x(i)
      sumSq += dx * dx
      i += 1
    }
    sumSq
  }

  final def distance(x: Array[Float], y: Array[Float]): Float = {
    math.sqrt(distanceSq(x, y)).toFloat
  }
}
