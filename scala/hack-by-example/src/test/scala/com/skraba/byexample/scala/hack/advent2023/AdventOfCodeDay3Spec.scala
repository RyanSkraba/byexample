package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 3 Solutions in scala=
  *
  * Input: A parts map consisting of periods, numbers and symbols
  *
  * Part 1: The sum of all the numbers in the map that border at least one symbol (including diagonals).
  *
  * Part 2: For all "gears"" that are asterisks in the map that border exactly two numbers (including diagonal), the sum
  * of the product of those two numbers.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/3]]
  */
class AdventOfCodeDay3Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def part1(input: String*): Long = {
      // The width of the plan including a border of periods
      val dx = input.headOption.map(_.length).getOrElse(0) + 2
      // The full plan as a single string, including a full border of periods
      val full = input.mkString("." * (dx + 1), "..", "." * (dx + 1))

      // Iterate over an accumulator of the remaining string to process, and the total of the parts found.
      val xs = LazyList.iterate((full.dropWhile(!_.isDigit), 0L)) { acc =>
        // Extract the number from the head of the string
        val (num, rest) = acc._1.span(_.isDigit)
        if (num.nonEmpty) {
          val pos = full.length - acc._1.length
          val above = full.slice(pos - 1 - dx, pos + num.length + 1 - dx)
          val below = full.slice(pos - 1 + dx, pos + num.length + 1 + dx)
          val beside = "" + full(pos - 1) + full(pos + num.length)
          val border = above + below + beside
          if (border.exists(_ != '.'))
            (rest, acc._2 + num.toLong)
          else
            (rest.dropWhile(!_.isDigit), acc._2)
        } else
          (rest.dropWhile(!_.isDigit), acc._2)

      }

      xs.dropWhile(_._1.nonEmpty).headOption.map(_._2).getOrElse(0L)
    }

    def part2(input: String*): Long = {
      // The width of the plan including a border of periods
      val dx = input.headOption.map(_.length).getOrElse(0) + 2
      // The full plan as a single string, including a full border of periods
      val full = input.mkString("." * (dx + 1), "..", "." * (dx + 1))

      // Helper to find the unique numbers surrounding a symbol at a given position
      def borderDigits(pos: Int) =
        full.slice(pos - 1, pos + 2).map(_.isDigit) match {
          case Seq(false, false, false) => Seq.empty
          case Seq(_, true, _)          => Seq(pos)
          case Seq(true, false, true)   => Seq(pos - 1, pos + 1)
          case Seq(true, false, false)  => Seq(pos - 1)
          case Seq(false, false, true)  => Seq(pos + 1)
        }

      // Helper to find a full number at the given position
      def numberAt(pos: Int): Long =
        (full.slice(0, pos).reverse.takeWhile(_.isDigit).reverse + full
          .slice(pos, full.length)
          .takeWhile(_.isDigit)).toLong

      // Now find all the asterisks in the plan
      val asterisks =
        for (
          y <- 1 until input.length + 1;
          x <- 0 until dx if full(y * dx + x) == '*'
        )
          yield y * dx + x

      // For each asterisk, find the surrounding digit positions
      val sides: Seq[Seq[Int]] = asterisks.map { pos =>
        borderDigits(pos - dx) ++ borderDigits(pos) ++ borderDigits(pos + dx)
      }

      // For each asterisk with exactly two numbers, find and sum their products
      sides.map {
        case Seq(left, right) => numberAt(left) * numberAt(right)
        case _                => 0
      }.sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """467..114..
        |...*......
        |..35..633.
        |......#...
        |617*......
        |.....+.58.
        |..592.....
        |......755.
        |...$.*....
        |.664.598..""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 4361
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 467835
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day3Input.txt")
    lazy val answer1 = decryptLong("QixHXp1dFg6QWstPc7uuSQ==")
    lazy val answer2 = decryptLong("xT6GBQSY5luJjSCuHKPRFA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
