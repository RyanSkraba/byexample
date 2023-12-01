package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 1 Solutions in scala=
  *
  * Input: List of alphanumeric strings that correspond to amended calibration
  * notes.
  *
  * Part 1: Every line has a two number that corresponds to the first digit in
  * the line and the last digit in the line. Find the sum of these numbers.
  *
  * Part 2: Find the sum if the first and last digit can also be their text
  * representation
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/1]]
  */
class AdventOfCodeDay1Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** Spelling out the digits in text from zero to nine. */
    val Digits: Seq[String] = Seq(
      "zero",
      "one",
      "two",
      "three",
      "four",
      "five",
      "six",
      "seven",
      "eight",
      "nine"
    )

    def extractCalibration(in: String*): Long =
      in
        .filter(_.nonEmpty)
        .map(str => (str.find(_.isDigit), str.findLast(_.isDigit)))
        .map(dgt =>
          10 * dgt._1.map(_ - '0').getOrElse(0) + dgt._2
            .map(_ - '0')
            .getOrElse(0)
        )
        .sum

    def extractCalibrationPart2(in: String*): Long = {
      val dgts: Map[String, Int] =
        Digits.zipWithIndex.toMap ++ "0123456789".zipWithIndex
          .map(dgt => dgt._1.toString -> dgt._2)
          .toMap

      val re1 = dgts.keys.mkString("(", "|", ")").r
      val re2 = dgts.keys.map(_.reverse).mkString("(", "|", ")").r

      val dgt1 = in.flatMap(re1.findFirstIn(_))
      val dgt2 = in.flatMap(str => re2.findFirstIn(str.reverse).map(_.reverse))

      dgt1.map(dgts(_) * 10).sum + dgt2.map(dgts(_)).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """1abc2
        |pqr3stu8vwx
        |a1b2c3d4e5f
        |treb7uchet
        |""".trim.stripMargin.split("\n")

    val input2 =
      """two1nine
        |eightwothree
        |abcone2threexyz
        |xtwone3four
        |4nineeightseven2
        |zoneight234
        |7pqrstsixteen
        |""".stripMargin.split("\n")

    it("should match the puzzle description") {
      extractCalibration(input: _*) shouldBe 142
      extractCalibrationPart2(input2: _*) shouldBe 281
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day1Input.txt")
    lazy val answer1 = decryptLong("YVEtS+jl23wF3KgtkMs8Ng==")
    lazy val answer2 = decryptLong("PO+p7ZYpVbeepHb2rRnC/g==")

    it("should have answers for part 1") {
      extractCalibration(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      extractCalibrationPart2(input: _*) shouldBe answer2
    }
  }
}
