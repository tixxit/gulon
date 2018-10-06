package net.tixxit.gulon

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.annotations.Scope
import org.openjdk.jmh.annotations.State

import scala.util.Random

object KMeansBench {
  val dimension = 100
  val size = 10000
  val k = 25

  @State(Scope.Benchmark)
  class KMeansState {
    val rng = new Random(0)
    val data = Array.tabulate(size, dimension) { (i, j) =>
      rng.nextFloat
    }
    var vectors = Vectors(Matrix(size, dimension, data))
    val kmeans = KMeans.init(k, vectors)
  }
}

class KMeansBench {
  import KMeansBench._

  @Benchmark
  def benchIter(state: KMeansState): Unit = {
    state.kmeans.iterate(state.vectors, 1)
  }
}
