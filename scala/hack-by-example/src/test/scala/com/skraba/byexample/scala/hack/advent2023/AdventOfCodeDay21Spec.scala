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
  * Part 1: Given that the gardener wants to step exactly 64 spaces from the starting point S. How many spaces are
  * exactly this distance?
  *
  * Part 2: The map is actually infinitely repeating, and the gardener wants to step exactly 26501365 spaces. How many
  * spaces are exactly this distance.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/21]]
  */
class AdventOfCodeDay21Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

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

    def part2(d: Int, in: String*): Long = {
      val dx: Int = in.headOption.map(_.length).getOrElse(0)
      val full: String = in.mkString
      val s0 = full.indexOf('S')

      // Inspecting the input manually:

      // The input is square and has an empty border all the way around it, and
      // there is an empty path from the start to the border in all directions.
      // We are guaranteed that the each border position is reachable in at
      // most the taxicab distance.

      // The start position is in the exact center, and there is an even number
      // of rows.

      // There is a clear diamond shaped path around the start position.  We
      // can expect the growing "area" covered by the steps to be diamond
      // shaped.  The input looks pretty sparse, we might make an exact diamond
      // by the time we cover enough steps to reach the border?

      // And again when we pass through another dx steps?

      // Using a chessboard argument and considering S to be RED, all odd steps
      // are on white and all even steps are on black.  Once any tile is lit,
      // it will alternate between on and off.

      // Check square input, and the central start position
      val start = s0 % dx -> s0 / dx
      if (full.length != dx * dx && dx % 2 != 1) return -1
      if (start._1 != start._2 && start._1 != dx / 2) return -1

      // Memo to memorize distances from the center
      var memoMaxStep = 0L
      val memo = mutable.Map(start -> 0L)
      def memoDistances(step: Long): Long = {
        // If we're going farther than we've gone before, calculate the distances
        if (step > memoMaxStep) {

          def isRock(pos: Tuple2[Int, Int]): Boolean =
            '#' == full(
              math.floorMod(pos._2, dx) * dx + math.floorMod(pos._1, dx)
            )

          val bfs: mutable.Queue[((Int, Int), Long)] =
            mutable.Queue.from(memo.filter(_._2 == memoMaxStep))
          while (bfs.nonEmpty) {
            val ((x, y), dist) = bfs.dequeue()
            val next = Set(
              (x + 1, y),
              (x - 1, y),
              (x, y + 1),
              (x, y - 1)
            ).filterNot(memo.contains)
              .filterNot(isRock)
              .map(p => (p, dist + 1L))
            memo.addAll(next)
            bfs.enqueueAll(next.filter(_._2 < step))
          }
          memoMaxStep = step
        }

        // The number of positions that can be reached from the given step, taking
        // into account whether it is on a RED or BLACK square
        memo.filter(_._2 <= step).count(_._2 % 2 == step % 2)
      }

      if (d < 10000) memoDistances(d)

      // 65 steps will reach the border

      // 131 steps plus the length should cover another tile in each direction

      // Aha, the input covers 202300 widths and 65 extra steps
      val (tiles, remainder) = (d / dx, d % dx)

      // OK at this point, I looked at other solutions to see it's a quadratic
      // function to be fit.  Oh well, I can do that.

      val p2 = memoDistances(2 * dx + remainder)
      val p1 = memoDistances(1 * dx + remainder)
      val p0 = memoDistances(remainder)

      // p0 = 0a + 0b + c
      // p1 =  a +  b + c
      // p2 = 4a + 2b + c

      // Calculating the coefficients
      val c = p0
      val a = (p2 + c) / 2 - p1
      val b = p1 - a - c

      // And applying to the number of tiles
      a * tiles * tiles + b * tiles + c
    }
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
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(6, input: _*).size shouldBe 16
    }

    ignore("should match the puzzle description for part 2") {
      // These are true, but my part2 solution depends on characteristics
      // that aren't in the example puzzle
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
    lazy val answer2 = decryptLong("n3NDxRwNP1J7B88/pHjRqg==")

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
