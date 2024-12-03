package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 3 Solutions in scala=
  *
  * Input: A data string to scan for multiplication.
  *
  * Part 1: Find all occurrences of mul(NNN,MMM) and calculate the sum of products of NNN and MMM.
  *
  * Part 2: Same as part one, but stop counting when we encounter don't() and start again when we encounter do().
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/3]]
  */
class AdventOfCodeDay3Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    val MulRegex = """mul\((\d+),(\d+)\)""".r

    def part1(in: String): Long = MulRegex.findAllMatchIn(in).map(m => m.group(1).toLong * m.group(2).toLong).sum

    def part2(in: String): Long = s"do()$in".split("don't\\(\\)").flatMap(_.split("do\\(\\)").drop(1)).map(part1).sum
  }

  import Solution._

  describe("Example case") {
    val input = """xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"""
    val input2 = """"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))""""

    it("should match the puzzle description for part 1") {
      part1(input) shouldBe 161
    }

    it("should match the puzzle description for part 2") {
      part2(input2) shouldBe 48
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day3Input.txt")
    lazy val answer1 = decryptLong("6j0GkPY9o3pFKY0ioAjmqw==")
    lazy val answer2 = decryptLong("wcCTziwGzeA7H0WGM2+/6g==")

    it("should have answers for part 1") {
      part1(input.mkString("\n")) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input.mkString("\n")) shouldBe answer2
    }
  }
}
