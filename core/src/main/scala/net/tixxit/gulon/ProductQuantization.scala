package net.tixxit.gulon

trait ProductQuantization

object ProductQuantization {
  case class Config(
    numSubvectors: Int,
    numClusters: Int,
    maxIterations: Int
  )

  // 
  def apply(vectors: Matrix, config: Config): ProductQuantization = {
    val size = vectors.rows
    val dimension = vectors.cols
    ???
  }
}
