package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 16 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from https://adventofcode.com/2022/day/16
  */
class AdventOfCodeDay16Spec
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
      |""".stripMargin.split("\n")

    it("should match the puzzle description") {
      part1(input: _*) shouldBe 100
      part2(input: _*) shouldBe 200
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day16Input.txt")
    it("should have answers") {
      part1(input: _*) shouldBe 100
      part2(input: _*) shouldBe 200
    }
  }
}
