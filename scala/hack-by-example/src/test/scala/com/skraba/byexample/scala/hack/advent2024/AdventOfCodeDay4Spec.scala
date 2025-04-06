package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 4 Solutions in scala=
  *
  * Input: A word search puzzle.
  *
  * Part 1: The number of XMAS you can find in the puzzle going in any direction, including reverse and diagonals.
  *
  * Part 2: The number of 2 diagonal MAS that intersect at the A.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/4]]
  */
class AdventOfCodeDay4Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** A 2D grid of characters with a width and height.
      *
      * @param full
      *   A string corresponding to all the grid lines concatenated.
      * @param w
      *   The width of the grid.
      * @param invalid
      *   When fetching a character outside the grid, return this value.
      */
    case class Plan(full: String, w: Int, invalid: Char = '.') {
      lazy val h: Int = full.length / w;
      lazy val display: String = full.grouped(w).mkString("\n")
      def apply(x: Int, y: Int): Char = if (x >= 0 && x < w && y >= 0 && y < h) full(y * w + x) else invalid
      def findAll(c: Char): Seq[(Int, Int)] = for (x <- 0 until w; y <- 0 until h if apply(x, y) == c) yield (x, y)
    }

    def part1(in: String*): Long = {
      val plan = Plan(in.mkString, in.head.length)
      // Find all 'X' and then look in all directions for 'M', 'A', 'S'
      val xmas =
        for {
          (x, y) <- plan.findAll('X'); dx <- -1 to 1; dy <- -1 to 1 if (dx, dy) != (0, 0);
          if plan(x + dx, y + dy) == 'M' && plan(x + 2 * dx, y + 2 * dy) == 'A' && plan(x + 3 * dx, y + 3 * dy) == 'S'
        } yield (x, y, dx, dy)
      xmas.size
    }

    def part2(in: String*): Long = {
      val plan = Plan(in.mkString, in.head.length)
      // Find all 'A' and then get the values in the 2 diagonals
      val mas =
        for ((x, y) <- plan.findAll('A'))
          yield (
            (x, y),
            Set(plan(x - 1, y - 1), plan(x + 1, y + 1)),
            Set(plan(x - 1, y + 1), plan(x + 1, y - 1))
          )
      // Only retain the coordinates where the diagonals have both 'M' and 'S'
      val x_mas = mas.filter(_._2('M')).filter(_._2('S')).filter(_._3('M')).filter(_._3('S'))
      x_mas.size
    }
  }

  import Solution._

  describe("Example case") {
    val inputMin =
      """..X...
        |.SAMX.
        |.A..A.
        |XMAS.S
        |.X....
        |""".trimSplit

    val input =
      """MMMSXXMASM
        |MSAMXMSMSA
        |AMXSXMAAMM
        |MSAMASMSMX
        |XMASAMXAMM
        |XXAMMXXAMA
        |SMSMSASXSS
        |SAXAMASAAA
        |MAMMMXMMMM
        |MXMXAXMASX
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(inputMin: _*) shouldBe 4
      part1(input: _*) shouldBe 18
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 9
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day4Input.txt")
    lazy val answer1 = decryptLong("fKFNrjFJP1VC8VmPkFPLnQ==")
    lazy val answer2 = decryptLong("dBSiG6lubHB1UY9Ab+PkxQ==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
