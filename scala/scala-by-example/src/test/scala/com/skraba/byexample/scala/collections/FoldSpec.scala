package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Patterns for folding with scala collections. */
class FoldSpec extends AnyFunSpecLike with Matchers {

  describe("Folding") {

    it("can be used to group elements in a list") {

      // The input elements.  We want to cut them into lists where subsequent elements
      // never have more than 5 between them.
      val in = List(1, 2, 3, 9, 20, 25, 30, 35, 40, 45)

      // Since the accumulator is Nil, we need to specify its type in the fold signature.
      // We're folding starting from the right.
      val out: List[List[Int]] = in.foldRight[List[List[Int]]](Nil) {
        // x is the new element
        // y and ys are part of the accumulator being constructed.
        case (x, y :: ys) if y.headOption.exists(_ - x <= 5) => (x :: y) :: ys
        case (x, ys)                                         => (x :: Nil) :: ys
      }

      out should have size 3
      out.head shouldBe List(1, 2, 3)
      out(1) shouldBe List(9)
      out(2) shouldBe List(20, 25, 30, 35, 40, 45)
    }
  }
}
