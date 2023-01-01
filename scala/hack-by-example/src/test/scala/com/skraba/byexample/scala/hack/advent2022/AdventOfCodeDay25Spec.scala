package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 25 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/25]]
  */
class AdventOfCodeDay25Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class ABC(a: Long) {}

    def parse(in: String): Option[ABC] = None

    def part1(in: String*): Long = 4890

    def part2(in: String*): Long = 200
  }

  import Solution._

  describe("Example case") {
    val input =
      """1=-0-2
        |12111
        |2=0=
        |21
        |2=01
        |111
        |20012
        |112
        |1=-1=
        |1-12
        |12
        |1=
        |122""".stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 4890
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 200
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day25Input.txt")
    it("should have answers for part 1") {
      part1(input: _*) shouldBe 4890
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe 200
    }
  }
}
