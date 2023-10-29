package com.skraba.byexample.scala.collections
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.Sorting

/** Collections, [[Sorting]] and [[Ordering]]
  *
  * @see
  *   [[https://www.scala-lang.org/api/2.12.6/scala/math/Ordering.html]]
  */
class SortingOrderingSpec extends AnyFunSpecLike with Matchers {

  case class ABC(a: String, b: Int, c: Int*)
  val xs: Array[ABC] = Array(ABC("a", 5, 2), ABC("c", 3), ABC("b", 1, 3, 2, 1))

  describe("Sorting.quickSort") {
    it("can be used to sort common arrays") {
      val xs = Array(3, 2, 1)
      Sorting.quickSort(xs)
      // The array is modified in place
      xs shouldBe Array(1, 2, 3)
    }
  }

  describe("Sorting.stableSort") {

    it("can be used to sort common arrays") {
      val xs = Array(3, 2, 1)
      Sorting.stableSort(xs)
      // The array is modified in place
      xs shouldBe Array(1, 2, 3)
    }

    it("can be used to sort immutable sequences") {
      val xs = Seq(3, 2, 1)
      // The sequence is not modified, but the return value is.
      Sorting.stableSort(xs) shouldBe Seq(1, 2, 3)
      xs shouldBe Seq(3, 2, 1)
    }

    it("can be used to sort tuples") {
      // The tuples are sorted using the first to last elements in order.
      Sorting.stableSort(Seq((3, 0), (2, 99), (1, 1), (1, 0))) shouldBe Seq(
        (1, 0),
        (1, 1),
        (2, 99),
        (3, 0)
      )
    }
  }

  describe("Ordering") {

    it("can be customized for tuples") {
      val pairs = Array(("a", 5, 2), ("c", 3, 1), ("b", 1, 3))

      // sort by 2nd element
      Sorting.quickSort(pairs)(Ordering.by[(String, Int, Int), Int](_._2))
      pairs shouldBe Seq(("b", 1, 3), ("c", 3, 1), ("a", 5, 2))

      // sort by the 3rd element, then 1st
      Sorting.quickSort(pairs)(Ordering[(Int, String)].on(x => (x._3, x._1)))
      pairs shouldBe Seq(("c", 3, 1), ("a", 5, 2), ("b", 1, 3))
    }

    it("can be customized for case classes") {
      // Sort by B
      Sorting.quickSort(xs)(_.b compare _.b)
      xs shouldBe Array(ABC("b", 1, 3, 2, 1), ABC("c", 3), ABC("a", 5, 2))
      // Sort by the length of c
      Sorting.quickSort(xs)(_.c.length compare _.c.length)
      xs shouldBe Array(ABC("c", 3), ABC("a", 5, 2), ABC("b", 1, 3, 2, 1))

      Sorting.stableSort(xs, (x: ABC, y: ABC) => x.a < y.a)
      xs shouldBe Array(ABC("a", 5, 2), ABC("b", 1, 3, 2, 1), ABC("c", 3))
    }
  }
}
