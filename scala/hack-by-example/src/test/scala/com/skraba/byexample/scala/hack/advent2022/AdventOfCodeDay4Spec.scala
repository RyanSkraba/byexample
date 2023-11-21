package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 4 Solutions in scala=
  *
  * Input: A list of lines in the form 1-2,3-4 where the first range are the
  * areas for the first elf to clean and the second is the areas for the second
  * elf to clean.
  *
  * Part 1: Count the number of elves where one of the elves entirely overlaps
  * the other's cleaning areas.
  *
  * Part 1: Count the number of elves where one of the elves partially overlaps
  * the other's cleaning areas.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/4]]
  */
class AdventOfCodeDay4Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** Two elves and their ranges. */
    case class ElfPair(aMin: Long, aMax: Long, bMin: Long, bMax: Long) {

      /** Does one of the ranges entirely overlap the other? */
      def overlapsEntirely: Boolean = (aMin - bMin) * (bMax - aMax) >= 0

      /** Does the ranges partially overlap? */
      def overlaps: Boolean =
        aMin >= bMin && aMin <= bMax || bMin >= aMin && bMin <= aMax
    }

    def parse(in: String): Option[ElfPair] =
      in.split("[,-]") match {
        case Array(a, b, c, d) =>
          Some(ElfPair(a.toLong, b.toLong, c.toLong, d.toLong))
        case _ => None
      }

    def part1(in: String*): Long =
      in.flatMap(parse).map(_.overlapsEntirely).count(identity)

    def part2(in: String*): Long =
      in.flatMap(parse).map(_.overlaps).count(identity)
  }

  import Solution._

  describe("Example case") {
    val input =
      """2-4,6-8,
        |2-3,4-5,
        |5-7,7-9,
        |2-8,3-7,
        |6-6,4-6,
        |2-6,4-8
        |""".stripMargin.split("\n")

    it("should match the puzzle description") {
      part1(input: _*) shouldBe 2
      part1(input :+ "": _*) shouldBe 2
      part2(input: _*) shouldBe 4
      part2(input :+ "": _*) shouldBe 4
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day4Input.txt")
    it("should have answers") {
      part1(input: _*) shouldBe 471
      part2(input: _*) shouldBe 888
    }
  }
}
