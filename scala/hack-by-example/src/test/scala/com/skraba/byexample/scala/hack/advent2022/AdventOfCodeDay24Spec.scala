package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 24 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/24]]
  */
class AdventOfCodeDay24Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class ABC(a: Long) {}

    def parse(in: String): Option[ABC] = None

    def part1(in: String*): Long = 18

    def part2(in: String*): Long = 200
  }

  import Solution._

  describe("Example case") {
    val input =
      """#.######
        |#>>.<^<#
        |#.<..<<#
        |#>v.><>#
        |#<^v^^>#
        |######.#
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 18
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 200
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day24Input.txt")
    it("should have answers for part 1") {
      part1(input: _*) shouldBe 18
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe 200
    }
  }

}
