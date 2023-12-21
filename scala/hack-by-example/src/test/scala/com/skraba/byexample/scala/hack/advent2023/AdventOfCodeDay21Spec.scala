package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2023 Day 21 Solutions in scala=
  *
  * Input: A map of garden plots, showing available space and rocks.
  *
  * Part 1: Given that the gardener wants to step exactly 64 spaces from the
  * starting point S. How many spaces are exactly this distance?
  *
  * Part 2: The map is actually infinitely repeating, and the gardener wants to
  * step exactly 26501365 spaces. How many spaces are exactly this distance.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/21]]
  */
class AdventOfCodeDay21Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** Reusable North, South, East, West */
    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }

    case class Plan(input: Seq[String], border: String = "#") {

      /** The width of the full plan */
      lazy val dx: Int = input.headOption.map(_.length).getOrElse(0) + 2

      /** The height of the full plan */
      lazy val dy: Int = input.length + 2

      /** The full plan as a single string, including a full border. */
      lazy val full: String =
        input.mkString(border * (dx + 1), border * 2, border * (dx + 1))

      lazy val display: String = full.grouped(dx).mkString("\n")
    }

    def part1(d: Int, in: String*): Set[Int] = {
      val plan = Plan(in)

      val pos = Set(plan.full.indexOf('S'))
      LazyList
        .iterate(pos)(
          _.flatMap(pos =>
            Set(pos + 1, pos - 1, pos + plan.dx, pos - plan.dx)
              .filter(plan.full(_) != '#')
          )
        )(d)
    }

    def part2(d: Int, in: String*): Long =
      if (d < 100) part1(d, in: _*).size else 200
  }

  import Solution._

  describe("Example case") {
    val input =
      """...........
        |.....###.#.
        |.###.##..#.
        |..#.#...#..
        |....#.#....
        |.##..S####.
        |.##..#...#.
        |.......##..
        |.##.#.####.
        |.##..##.##.
        |...........
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(6, input: _*).size shouldBe 16
    }

    ignore("should match the puzzle description for part 2") {
      part2(6, input: _*) shouldBe 16
      part2(10, input: _*) shouldBe 50
      part2(50, input: _*) shouldBe 1594
      part2(100, input: _*) shouldBe 6536
      part2(500, input: _*) shouldBe 167004
      part2(1000, input: _*) shouldBe 668697
      part2(5000, input: _*) shouldBe 16733044
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day21Input.txt")
    lazy val answer1 = decryptLong("MUZ3tq06UzkU/J+IryXpQw==")
    lazy val answer2 = decryptLong("U9BZNCixKWAgOXNrGyDe5A==")

    it("should have answers for part 1") {
      part1(64, input: _*).size shouldBe answer1
    }

    it("should have answers for part 1 using part2") {
      part2(64, input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(26501365, input: _*) shouldBe answer2
    }
  }
}
