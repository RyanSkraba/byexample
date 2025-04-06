package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2024 Day 19 Solutions in scala=
  *
  * Input: A list of valid towels and a list of designs.
  *
  * Part 1: For the given designs, count how many can be split into valid towels. This is the same problem as splitting
  * smushed together words when there is a limited dictionary.
  *
  * Part 2: For each design, count the total number of ways they could be split into valid towels, and return the sum.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/19]]
  */
class AdventOfCodeDay19Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String*): (Set[String], Seq[String]) = in.span(_.nonEmpty) match {
      case (towels, designs) => (towels.head.split(", ").toSet, designs.tail)
    }

    def part1(in: String*): Long = {
      val (towels, designs) = parse(in: _*)
      // Memo of whether a substring can be split into towels or not.
      lazy val split: String => Boolean = new mutable.HashMap[String, Boolean]() {
        override def apply(design: String): Boolean = getOrElseUpdate(
          design,
          if (design.isEmpty) true
          else towels.filter(design.startsWith).map(_.length).map(design.substring).exists(split)
        )
      }
      designs.map(split).count(identity)
    }

    def part1optimized(in: String*): Long = {
      val (towels, designs) = parse(in: _*)
      val maxTowel = towels.map(_.length).max

      // This is roughly the same as the memoized version, but using an iterative approach.
      def split(design: String): Boolean = {
        // The accumulator is the memo, and stores the [start, end) indices for each valid towel found so far.
        val splittables = design.indices.map(_ + 1).foldLeft(Seq(Int.MinValue -> 0)) { case (acc, end) =>
          acc
            .find { case (_, i) => end - i <= maxTowel && towels(design.substring(i, end)) }
            .map(_._2 -> end)
            .map(_ +: acc)
            .getOrElse(acc)
        }
        // The design can be split if the last valid towel goes all the way to the end of the design.
        splittables.headOption.map(_._2).contains(design.length)
      }

      designs.map(split).count(identity)
    }

    def part2(in: String*): Long = {
      val (towels, designs) = parse(in: _*) match { case (t, d) => (t.toSeq, d) }
      // Memo of how many towels a substring can be split into
      lazy val split: String => Long = new mutable.HashMap[String, Long]() {
        override def apply(design: String): Long = getOrElseUpdate(
          design,
          if (design.isEmpty) 1L
          else towels.filter(design.startsWith).map(_.length).map(design.substring).map(split).sum
        )
      }
      designs.map(split).sum
    }

  }

  import Solution._

  describe("Example case") {
    val input =
      """r, wr, b, g, bwu, rb, gb, br
        |
        |brwrr
        |bggr
        |gbbr
        |rrbgbr
        |ubwu
        |bwurrg
        |brgr
        |bbrgwb
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 6
    }

    it("should match the puzzle description for part 1 optimized") {
      part1optimized(input: _*) shouldBe 6
    }

    it("should match the puzzle description for part 2") {
      part2("r, wr, b, g, bwu, rb, gb, br", "", "brwrr") shouldBe 2
      part2(input: _*) shouldBe 16
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day19Input.txt")
    lazy val answer1 = decryptLong("34UWR2zoNSFWhgClOYn6ow==")
    lazy val answer2 = decryptLong("LhJV2FxS76MbaYI3Rp2FbA==")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should match the puzzle description for part 1 optimized") {
      part1optimized(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
