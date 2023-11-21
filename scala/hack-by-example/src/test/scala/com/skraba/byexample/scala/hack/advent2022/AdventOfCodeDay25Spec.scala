package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 25 Solutions in scala=
  *
  * Input: A list of SNAFU formatted numbers.
  *
  * Part 1: The sum of the SNAFU numbers in decimal.
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/25]]
  */
class AdventOfCodeDay25Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    def unSnafu(in: String): Long = in.foldLeft(0L) {
      case (acc, '2') => acc * 5 + 2
      case (acc, '1') => acc * 5 + 1
      case (acc, '0') => acc * 5
      case (acc, '-') => acc * 5 - 1
      case (acc, '=') => acc * 5 - 2
    }

    def snafu(in: Long): String = Stream
      .iterate(("", in)) {
        case (acc, remainder) if remainder % 5 == 2 =>
          ("2" + acc, remainder / 5)
        case (acc, remainder) if remainder % 5 == 1 =>
          ("1" + acc, remainder / 5)
        case (acc, remainder) if remainder % 5 == 0 =>
          ("0" + acc, remainder / 5)
        case (acc, remainder) if remainder % 5 == 3 =>
          ("=" + acc, remainder / 5 + 1)
        case (acc, remainder) if remainder % 5 == 4 =>
          ("-" + acc, remainder / 5 + 1)
      }
      .dropWhile(_._2 != 0)
      .head
      ._1

    def part1(in: String*): String = snafu(in.map(unSnafu).sum)

    def part2(in: String*): Long = 200
  }

  import Solution._

  describe("Example case") {
    val input =
      """1=-0-2
        |12111
        |2=0=
        |21
        |2=01
        |111
        |20012
        |112
        |1=-1=
        |1-12
        |12
        |1=
        |122""".stripMargin.split("\n")

    val inputDecimal =
      Seq(1747, 906, 198, 11, 201, 31, 1257, 32, 353, 107, 7, 3, 37)

    for (i <- input.indices) {
      it(s"should unsnafu ${input(i)}") {
        unSnafu(input(i)) shouldBe inputDecimal(i)
      }
      it(s"should snafu ${inputDecimal(i)}") {
        snafu(inputDecimal(i)) shouldBe input(i)
      }
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe "2=-1=0"
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day25Input.txt")
    it("should have answers for part 1") {
      part1(input: _*) shouldBe "2=--=0000-1-0-=1=0=2"
    }
  }
}
