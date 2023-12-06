package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 6 Solutions in scala=
  *
  * Input: For a series of races, the maximum time allocated to each race, as
  * well as the record distance to beat for each race
  *
  * Part 1: For each toy boat, we can increase the speed by 1ms/mm by pushing
  * the button for 1ms. We have less time to race, but the boat goes faster. For
  * each race, find all of the possible button press times that would beat the
  * existing record, and multiply these together.
  *
  * Part 2: Consider only one race by removing the spaces between the given race
  * times and distances to make big, big numbers, and find the possible button
  * press times that would beat the record.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/6]]
  */
class AdventOfCodeDay6Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** The quadratic formula. */
    def quadratic(a: Long, b: Long, c: Long): (Double, Double) = {
      val dsc = math.sqrt(b * b - 4 * a * c)
      val root1 = (-b - dsc) / (2 * a)
      val root2 = (-b + dsc) / (2 * a)
      (math.min(root1, root2), math.max(root1, root2))
    }

    def part1(in: String*): Long = {
      // Parse the input lines to get a list of the races as a sequence of Long
      // where the first number is the time allowed for the race, and the second
      // is the distance to beat.
      val races = in
        .map(_.split("\\s+").drop(1).map(_.toLong))
        .grouped(2)
        .flatMap(x => x.head.zip(x(1)))
        .toSeq

      // if T is the time allocated to the race, we can travel:
      // distance(x) = (T - x) * x
      // where x is the time we pushed the button
      // if we want the distance to be > L, all of the solutions between the roots of
      // (T - x) * x = L + 1
      // Or  -x2 - T * x - L - 1 == 0
      val roots = races
        .map(r => quadratic(-1, r._1, -r._2 - 1))
        .map(r => math.floor(r._2) - math.ceil(r._1) + 1)

      roots.product.toLong
    }

    def part2(in: String*): Long = {
      // Identical to above except for the parsing
      val race: Seq[Long] = in
        .map(_.split("\\s+").drop(1).mkString.toLong)
      val r = quadratic(-1, race.head, -race(1) - 1)
      (math.floor(r._2) - math.ceil(r._1) + 1).toLong
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Time:      7  15   30
        |Distance:  9  40  200
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 288
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 71503
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day6Input.txt")
    lazy val answer1 = decryptLong("KE1Gzoswci0Sc20o6EJWhw==")
    lazy val answer2 = decryptLong("fbUo0zmiYiTVih9z9I2GrA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
