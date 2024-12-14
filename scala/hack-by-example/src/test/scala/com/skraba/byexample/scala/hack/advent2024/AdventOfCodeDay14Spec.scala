package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.math.floorMod
import scala.util.matching.Regex

/** =Advent of Code 2024 Day 14 Solutions in scala=
  *
  * Input: Robots with a starting position and velocity moving in a grid, where boundaries teleport them to the other
  * side.
  *
  * Part 1: Find the position of each robot after 100 moves, and count the number of robots in each quadrant (ignoring
  * the robots in the middle grid lines). Return the product of the number of robots in each quadrant.
  *
  * Part 2: Find a special pattern in the robots positions after a number of moves.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/14]]
  */
class AdventOfCodeDay14Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    val RobotRe = "p=(.+),(.+) v=(.+),(.+)".r: Regex

    case class Robot(x: Int, y: Int, vx: Int, vy: Int)

    /** To examine the map. */
    def toDisplay(dx: Int, dy: Int, rs: Robot*): String = {
      rs.foldLeft(Seq.fill(dy)("." * dx)) { case (plan, r) =>
        plan.updated(r.y, plan(r.y).updated(r.x, '#'))
      }.mkString("\n")
    }

    def parse(in: String): Option[Robot] = RobotRe
      .findFirstMatchIn(in)
      .map(m => Robot(m.group(1).toInt, m.group(2).toInt, m.group(3).toInt, m.group(4).toInt))

    def part1(dx: Int, dy: Int, in: String*): Long = {
      val robots = in.flatMap(parse)
      val (nw, ne, sw, se) = robots.foldLeft((0L, 0L, 0L, 0L)) { case ((nw, ne, sw, se), r) =>
        (floorMod(r.x + r.vx * 100, dx), floorMod(r.y + r.vy * 100, dy)) match {
          case (x, y) if x < dx / 2 && y < dy / 2               => (nw + 1, ne, sw, se)
          case (x, y) if x >= (dx / 2) + 1 && y < dy / 2        => (nw, ne + 1, sw, se)
          case (x, y) if x < dx / 2 && y >= (dy / 2) + 1        => (nw, ne, sw + 1, se)
          case (x, y) if x >= (dx / 2) + 1 && y >= (dy / 2) + 1 => (nw, ne, sw, se + 1)
          case _                                                => (nw, ne, sw, se)
        }
      }
      nw * ne * sw * se
    }

    def part2(dx: Int, dy: Int, in: String*): Long = {
      val robots = in.flatMap(parse)
      LazyList
        .iterate((0L, robots)) { case (i, rs) =>
          (i + 1, rs.map(r => r.copy(x = floorMod(r.x + r.vx, dx), y = floorMod(r.y + r.vy, dy))))
        }
        .filter(_._2.map(r => r.x -> r.y).distinct.size == robots.size)
        // .map(i => i._1 -> toDisplay(i._2:_*)) Check here to find which floor plan has a pattern.
        .head // It turns out to be the first one where the robots are non-overlapping!
        ._1
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """p=0,4 v=3,-3
        |p=6,3 v=-1,-3
        |p=10,3 v=-1,2
        |p=2,0 v=2,-1
        |p=0,0 v=1,3
        |p=3,0 v=-2,-2
        |p=7,6 v=-1,-3
        |p=3,0 v=-1,-2
        |p=9,3 v=2,3
        |p=7,3 v=-1,2
        |p=2,4 v=2,-3
        |p=9,5 v=-3,-3
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(11, 7, input: _*) shouldBe 12
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day14Input.txt")
    lazy val answer1 = decryptLong("uyBZIM7DHMlWsAwhQ5hHqQ==")
    lazy val answer2 = decryptLong("gS0SBc0MmCfio6M+AT/t2Q==")

    it("should have answers for part 1") {
      part1(101, 103, input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(101, 103, input: _*) shouldBe answer2
    }
  }
}
