package net.tixxit.gulon

sealed abstract class Metric(
  val quantizedResiduals: Boolean,
  val normalized: Boolean
) extends Product with Serializable

object Metric {
  final case object L2 extends Metric(true, false)
  final case object Cosine extends Metric(false, true)

  final def fromProtobuf(m: protobuf.Metric): Metric =
    m match {
      case protobuf.Metric.L2 => L2
      case protobuf.Metric.COSINE => Cosine
      case protobuf.Metric.Unrecognized(i) =>
        throw new IllegalArgumentException(s"unrecognized metric: $i")
    }

  final def toProtobuf(m: Metric): protobuf.Metric =
    m match {
      case L2 => protobuf.Metric.L2
      case Cosine => protobuf.Metric.COSINE
    }
}
