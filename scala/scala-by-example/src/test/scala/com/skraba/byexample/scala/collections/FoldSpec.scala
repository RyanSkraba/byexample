package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Patterns for folding with scala collections. */
class FoldSpec extends AnyFunSpecLike with Matchers {

  describe("Folding") {

    it("can be used to split a list at between two elements") {
      // When comparing two consecutive elements determines whether the list should be split

      // Since the accumulator is Nil, we need to specify its type in the fold signature.
      // We're folding starting from the right.
      def splitAtGap(gap: Int, in: List[Int]): List[List[Int]] =
        in.foldRight[List[List[Int]]](Nil) {
          // x is the new element
          // y and ys are part of the accumulator being constructed.
          case (x, y :: ys) if y.headOption.exists(_ - x <= gap) =>
            (x :: y) :: ys
          case (x, ys) => (x :: Nil) :: ys
        }

      // The input elements.  We want to cut them into lists where subsequent elements
      // never have more than 5 between them.
      val out = splitAtGap(5, List(1, 2, 3, 9, 20, 25, 30, 35, 40, 45))

      out should have size 3
      out.head shouldBe List(1, 2, 3)
      out(1) shouldBe List(9)
      out(2) shouldBe List(20, 25, 30, 35, 40, 45)
    }

    it("can be used merge intervals") {
      // Merges intervals of integers where (i1,i2) is inclusive.
      def merge(sortedByFirst: Seq[(Int, Int)]): Seq[(Int, Int)] =
        sortedByFirst.foldRight(List.empty[(Int, Int)]) {
          case ((i1, i2), Nil) => (i1, i2) :: Nil
          case ((i1, i2), (acc1, acc2) :: rest) if i2 >= (acc1 - 1) =>
            (i1 min acc1, i2 max acc2) :: rest
          case ((i1, i2), acc) => (i1, i2) :: acc
        }

      for (
        x <- Seq(List.empty, List((-101, -100)));
        y: List[(Int, Int)] <- Seq(List.empty, List((100, 101)))
      ) {
        // All of these overlap
        merge(x ++ List((0, 1)) ++ y) shouldBe x ++ List((0, 1)) ++ y
        merge(x ++ List((0, 1), (0, 4)) ++ y) shouldBe x ++ List((0, 4)) ++ y
        merge(x ++ List((0, 5), (3, 4)) ++ y) shouldBe x ++ List((0, 5)) ++ y
        merge(x ++ List((0, 5), (3, 5)) ++ y) shouldBe x ++ List((0, 5)) ++ y
        merge(x ++ List((0, 5), (3, 6)) ++ y) shouldBe x ++ List((0, 6)) ++ y
        // Although this doesn´t overlap, these integer ranges can be merged
        merge(x ++ List((0, 1), (2, 4)) ++ y) shouldBe x ++ List((0, 4)) ++ y
        // This doesn´t overlap
        merge(x ++ List((0, 1), (3, 4)) ++ y) shouldBe x ++ List(
          (0, 1),
          (3, 4)
        ) ++ y
      }
    }
  }
}
