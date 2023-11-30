package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 23 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/23]]
  */
class AdventOfCodeDay23Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class ABC(a: Long) {}

    def parse(in: String): Option[ABC] = None

    def part1(in: String*): Long = 100

    def part2(in: String*): Long = 200
  }

  import Solution._

  describe("Example case") {
    val input =
      """
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 100
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 200
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day23Input.txt")
    lazy val answer1 = decryptLong("tTNGygZ0+O4PEH+5IiCrBw==")
    lazy val answer2 = decryptLong("U9BZNCixKWAgOXNrGyDe5A==")
    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
