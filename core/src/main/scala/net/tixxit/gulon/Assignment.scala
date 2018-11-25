package net.tixxit.gulon

import java.util.Arrays

final class Assignment private (val assignments: Array[Int]) {
  def size: Int = assignments.length

  override def hashCode: Int =
    (getClass, ArrayUtils.deepHashCode(assignments)).hashCode

  override def equals(that: Any): Boolean = that match {
    case (that: Assignment) =>
      Arrays.equals(assignments, that.assignments)
    case _ => false
  }
}

object Assignment {
  def apply(assignments: Array[Int]): Assignment =
    new Assignment(assignments)

  def unapply(a: Assignment): Some[Array[Int]] = Some(a.assignments)
}
