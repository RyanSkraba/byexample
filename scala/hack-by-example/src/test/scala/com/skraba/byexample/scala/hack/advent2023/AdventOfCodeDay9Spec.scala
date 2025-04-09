package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 9 Solutions in scala=
  *
  * Input: A list of number sequences, one line per sequence. The delta between each number is a constant *OR* the delta
  * between each delta is a constant or the delta between _those_ deltas are constants or...
  *
  * Part 1: Find the next number in each sequence, and sum them together.
  *
  * Part 2: Find the previous number in each sequence, and sum them together.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/9]]
  */
class AdventOfCodeDay9Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {
    def part1extrapolate(in: Long*): Long = {
      val y = LazyList.iterate(in) {
        _.sliding(2).toSeq.map { xs => xs(1) - xs.head }
      }
      val nn = y.takeWhile(_.exists(_ != 0))
      nn.map { _.last }.sum
    }

    def part1(in: String*): Long = {
      in.map(_.split("\\s+").toSeq.map(_.toLong)).map(part1extrapolate(_: _*)).sum
    }

    def part2(in: String*): Long = {
      in.map(_.split("\\s+").toSeq.reverse.map(_.toLong))
        .map(part1extrapolate(_: _*))
        .sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """0 3 6 9 12 15
        |1 3 6 10 15 21
        |10 13 16 21 30 45
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1extrapolate(1, 1, 1) shouldBe 1
      part1extrapolate(1, 2, 3, 4, 5) shouldBe 6
      part1(input: _*) shouldBe 114
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 2
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day9Input.txt")
    lazy val answer1 = decryptLong("/3DZIAsPsnsRIjv9NxbfHg==")
    lazy val answer2 = decryptLong("sPtmgR95P6JS2265Rc863A==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
