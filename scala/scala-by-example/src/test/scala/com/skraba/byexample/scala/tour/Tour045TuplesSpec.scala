package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour045TuplesSpec extends AnyFunSpecLike with Matchers {

  describe("Tuples") {

    it("can be simple pairs.") {
      // Create with parenthesis
      val tuple: (Int, String) = (1, "one")
      tuple shouldBe (1, "one")
      tuple._1 shouldBe 1
      tuple._2 shouldBe "one"
      tuple shouldBe a[(Int, String)]

      // Or as an instance.  This is a case class.
      val tuple2 = Tuple2(2, "two")

      // And have this extra method.
      tuple.swap shouldBe ("one", 1)

      // Taken apart with pattern matching.
      val (id: Int, name: String) = tuple
      id shouldBe 1
      name shouldBe "one"

      // Used in pattern matching.
      val xs = Seq(tuple, tuple2, 3 -> "three", (4, "four"))
      for ((xsId, xsName) <- xs) {
        xsId shouldBe <=(4)
        xsName shouldBe xsName.toLowerCase
      }
    }

    it("are products and case classes.") {
      // Create with parenthesis
      val tuple = (1, "one")

      tuple.productArity shouldBe 2
      tuple.productElement(0) shouldBe 1
      tuple.productElement(1) shouldBe "one"
      tuple.productIterator.toSeq shouldBe Seq(1, "one")
    }
  }
}
