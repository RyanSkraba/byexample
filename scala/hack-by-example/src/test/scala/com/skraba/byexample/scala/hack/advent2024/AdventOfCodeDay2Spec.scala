package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 2 Solutions in scala=
  *
  * Input: A list of lists of space-separated integers, where each line is a report.
  *
  * Part 1: How many reports are safe? A report is safe if the difference between each pair of numbers is between 1 and
  * 3, and monotonically increasing OR decreasing.
  *
  * Part 2: How many reports are safe if you can remove at most one number?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/2]]
  */
class AdventOfCodeDay2Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String): Array[Int] = in.trim.split("\\s+").map(_.toInt)

    def part1(in: String*): Long = in.map(parse).count { isSafe }

    def isSafe(report: Array[Int]): Boolean = {
      val dir = (report.head - report(1)).sign
      val deltas = report.sliding(2).map { case Array(a, b) => dir * (a - b) }.toSeq
      deltas.min >= 1 && deltas.max <= 3 && deltas.forall(_ > 0)
    }

    def part2(in: String*): Long = in.map(parse).count { isSafe2 }

    def isSafe2(report: Array[Int]): Boolean =
      isSafe(report) || report.indices.map(report.patch(_, Nil, 1)).exists(isSafe)
  }

  import Solution._

  describe("Example case") {
    val input =
      """7 6 4 2 1
        |1 2 7 8 9
        |9 7 6 2 1
        |1 3 2 4 5
        |8 6 4 4 1
        |1 3 6 7 9
        |""".trim.stripMargin.split("\n")

    it("should calculate safe reports for part 1") {
      isSafe(Array(7, 6, 4, 2, 1)) shouldBe true
      isSafe(Array(1, 2, 7, 8, 9)) shouldBe false
      isSafe(Array(9, 7, 6, 2, 1)) shouldBe false
      isSafe(Array(1, 3, 2, 4, 5)) shouldBe false
      isSafe(Array(8, 6, 4, 4, 1)) shouldBe false
      isSafe(Array(1, 3, 6, 7, 9)) shouldBe true
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 2
    }

    it("should calculate safe reports for part 2") {
      isSafe2(Array(7, 6, 4, 2, 1)) shouldBe true
      isSafe2(Array(1, 2, 7, 8, 9)) shouldBe false
      isSafe2(Array(9, 7, 6, 2, 1)) shouldBe false
      isSafe2(Array(1, 3, 2, 4, 5)) shouldBe true
      isSafe2(Array(8, 6, 4, 4, 1)) shouldBe true
      isSafe2(Array(1, 3, 6, 7, 9)) shouldBe true
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 4
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day2Input.txt")
    lazy val answer1 = decryptLong("Ljb9bqy1aiKgdgVbZbXffA==")
    lazy val answer2 = decryptLong("FVT+A7ta99wjMfGZk+7QwQ==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
