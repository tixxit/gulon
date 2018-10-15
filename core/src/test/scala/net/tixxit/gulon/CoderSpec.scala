package net.tixxit.gulon

import org.scalatest.FunSuite
import org.scalatest.prop.PropertyChecks
import org.scalacheck.Gen

class CoderSpec extends FunSuite with PropertyChecks {
  case class CoderWithValues(coder: Coder, values: List[Int])

  val genCoderWithValues: Gen[CoderWithValues] = for {
    width <- Gen.choose(1, 16)
    maxValue <- Gen.choose(1 << width - 1, 1 << width - 1)
    values <- Gen.nonEmptyListOf(Gen.choose(0, maxValue))
  } yield CoderWithValues(Coder(width, values.length), values)


  test("coder round-trips indices") {
    forAll(genCoderWithValues) { case CoderWithValues(coder, values) =>
      val code = coder.buildCode(values.toArray)
      val actual = (0 until values.length).map(coder.getIndex(code, _))
      assert(actual == values)
    }
  }

  test("factoryFor returns factories for width 1 -> 16") {
    (1 to 16).foreach { w =>
      assert(Coder.factoryFor(w).isDefined)
    }
  }

  test("produces small code") {
    val indices = Array(1, 1, 1, 1, 1)
    Coder.supportedWidths.foreach { w =>
      val expectedSize = (indices.length * w + 7) / 8
      val coder = Coder(w, indices.length)
      val code = coder.buildCode(indices)
      val bytes = coder.unwrapCode(code)
      assert(bytes.length == expectedSize)
    }
  }
}
