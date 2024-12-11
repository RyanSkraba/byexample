package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 11 Solutions in scala=
  *
  * Input: A series of numbers (written on stones). Every time you blink, the number changes: if it is zero, it becomes
  * 1, otherwise if it has an even number of digits, it splits exactly into two numbers, otherwise it multiplies by
  * 2024.
  *
  * Part 1: After 25 blinks, how many stones do you have?
  *
  * Part 2: After 75 blinks, how many stones do you have?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/11]]
  */
class AdventOfCodeDay11Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String): Map[Long, Long] = in.split("\\s+").map(_.toLong -> 1L).toMap

    def blink(in: Map[Long, Long]): Map[Long, Long] = {
      in.toSeq
        .flatMap {
          case (0, count) => Seq(1L -> count)
          case (n, count) if (n.toString.length % 2 == 0) =>
            n.toString.splitAt(n.toString.length / 2) match {
              case (n1, n2) => Seq(n1.toLong -> count, n2.toLong -> count)
            }
          case (n, count) => Seq(n * 2024L -> count)
        }
        .groupMapReduce(_._1)(_._2)(_ + _)
    }

    def part1(in: String*): Long = LazyList.iterate(parse(in.head)) { blink }.drop(25).head.values.sum

    def part2(in: String*): Long = LazyList.iterate(parse(in.head)) { blink }.drop(75).head.values.sum
  }

  import Solution._

  describe("Example case") {
    val input = "125 17"

    it("should match the puzzle description for part 1") {
      part1(input) shouldBe 55312
    }

    it("should match the puzzle description for part 2") {
      part2(input) shouldBe 65601038650482L
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day11Input.txt").mkString("")
    lazy val answer1 = decryptLong("i1CZ9HOq154JpruumX+xNQ==")
    lazy val answer2 = decryptLong("eEXY2XYSbeZuNtK/0xHLWQ==")

    it("should have answers for part 1") {
      part1(input) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input) shouldBe answer2
    }
  }
}
