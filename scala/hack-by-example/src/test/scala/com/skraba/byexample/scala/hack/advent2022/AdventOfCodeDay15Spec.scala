package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.util.matching.Regex

/** =Advent of Code 2022 Day 15 Solutions in scala=
  *
  * Input: Sensor readings from within the cavern finding the closest beacon
  *
  * Part 1: The number of places in a given row that we can guarantee do not
  * have a beacon
  *
  * Part 2: Given some bounds, the first empty place found
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/15]]
  */
class AdventOfCodeDay15Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    val Scan =
      "Sensor at x=(.+), y=(.+): closest beacon is at x=(.+), y=(.+)".r: Regex

    def analyseRow(
        row: Int,
        in: Seq[String],
        lower: Int = Int.MinValue,
        upper: Int = Int.MaxValue
    ): (List[(Int, Int)], Set[Int]) = {

      // scanning through all of the sensors, find the intervals that the sensor contributes to the row, as well as any beacons in this row
      val sensors: Seq[(Int, Int, Option[Int])] = in.toStream
        // parse the input to the sensor and beacon coordinates
        .map { case Scan(sx, sy, bx, by) =>
          (sx.toInt, sy.toInt, bx.toInt, by.toInt)
        }
        // find the manhattan distance between the beacon and throw away the sensors that can't contribute to this row
        .map { case (sx, sy, bx, by) =>
          (sx, sy, bx, by, (sx - bx).abs + (sy - by).abs)
        }
        .filter { case (_, sy, _, _, dist) => (row - sy).abs <= dist }
        // find the length of the interval that the sensor covers in this row
        .map { case (sx, sy, bx, by, dist) =>
          (sx, bx, by, dist - (row - sy).abs)
        }
        // instead of the signal location, use the interval bounds that the signal covers and filter out the ones that are out of bounds
        .map { case (sx, bx, by, dx) =>
          (sx - dx, sx + dx, bx, by)
        }
        .filter { case (i1, i2, bx, by) => i1 <= upper && i2 >= lower }
        .map { case (i1, i2, bx, by) =>
          (i1 max lower, i2 min upper, if (by == row) Some(bx) else None)
        }

      // merge the intervals
      val intervals = sensors.sortBy(_._1).foldLeft(List.empty[(Int, Int)]) {
        case (Nil, (i1, i2, _)) => (i1, i2) :: Nil
        case ((acc1, acc2) :: rest, (i1, i2, _)) if i1 <= (acc2 + 1) =>
          (acc1, acc2 max i2) :: rest
        case (acc, (i1, i2, _)) => (i1, i2) :: acc
      }

      val beacons = sensors.flatMap(_._3).toSet

      (intervals, beacons)
    }

    def part1(row: Int, in: String*): Long = {
      val (intervals, beacons) = analyseRow(row, in)
      intervals.foldLeft(0L) { case (acc, (i1, i2)) =>
        acc + i2 - i1 + 1
      } - beacons.size
    }

    def part2(upper: Int, in: String*): Long = {
      val findInInterval = (0 to upper).toStream.map { row =>
        analyseRow(row, in, 0, upper)._1 match {
          case Seq(one, _)                => (row, one._1 - 1)
          case Seq((0, x2)) if x2 < upper => (row, upper)
          case Seq((x1, 0)) if x1 > 0     => (row, 0)
          case _                          => (row, -1)
        }
      }

      val found = findInInterval.filter(_._2 >= 0).head
      found._2 * 4000000L + found._1
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Sensor at x=2, y=18: closest beacon is at x=-2, y=15
        |Sensor at x=9, y=16: closest beacon is at x=10, y=16
        |Sensor at x=13, y=2: closest beacon is at x=15, y=3
        |Sensor at x=12, y=14: closest beacon is at x=10, y=16
        |Sensor at x=10, y=20: closest beacon is at x=10, y=16
        |Sensor at x=14, y=17: closest beacon is at x=10, y=16
        |Sensor at x=8, y=7: closest beacon is at x=2, y=10
        |Sensor at x=2, y=0: closest beacon is at x=2, y=10
        |Sensor at x=0, y=11: closest beacon is at x=2, y=10
        |Sensor at x=20, y=14: closest beacon is at x=25, y=17
        |Sensor at x=17, y=20: closest beacon is at x=21, y=22
        |Sensor at x=16, y=7: closest beacon is at x=15, y=3
        |Sensor at x=14, y=3: closest beacon is at x=15, y=3
        |Sensor at x=20, y=1: closest beacon is at x=15, y=3
        |""".stripMargin.split("\n")

    it("should solve simple input") {
      val in = Seq(
        "Sensor at x=0, y=0: closest beacon is at x=2, y=2",
        "Sensor at x=10, y=10: closest beacon is at x=8, y=8"
      )
      part1(-5, in: _*) shouldBe 0
      part1(-4, in: _*) shouldBe 1
      part1(-3, in: _*) shouldBe 3
      part1(-2, in: _*) shouldBe 5
      part1(-1, in: _*) shouldBe 7
      part1(0, in: _*) shouldBe 9
      part1(1, in: _*) shouldBe 7
      part1(2, in: _*) shouldBe 4
      part1(3, in: _*) shouldBe 3
      part1(4, in: _*) shouldBe 1
      part1(5, in: _*) shouldBe 0
      part1(10, in: _*) shouldBe 9
    }

    it("should match the puzzle description") {
      part1(10, input: _*) shouldBe 26
      part2(20, input: _*) shouldBe 56000011
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day15Input.txt")
    it("should have answers (150s", Slow) {
      // About 2m30 seconds
      part1(2000000, input: _*) shouldBe 4748135
      part2(4000000, input: _*) shouldBe 13743542639657L
    }
  }
}
