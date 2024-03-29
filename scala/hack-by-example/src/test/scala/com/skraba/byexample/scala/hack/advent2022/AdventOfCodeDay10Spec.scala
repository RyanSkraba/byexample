package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 10 Solutions in scala=
  *
  * Input: The assembly text being sent to the radio, which is a 1-cycle NOOP or a 2-cycle ADDX that adds to the single
  * X register in the radio CPU.
  *
  * Part 1: A signal strength which is the value of the X register times the cycle number at certain points.
  *
  * Part 2: An image to be displayed on the CPU.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/10]]
  */
class AdventOfCodeDay10Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def radio(in: String*): Seq[Int] = {
      in.foldLeft(Seq(1)) {
        case (acc, code) if code.startsWith("addx ") =>
          acc ++ Seq(acc.last, acc.last + code.substring(5).toInt)
        case (acc, _) => acc :+ acc.last
      }
    }

    def part1(in: String*): Long =
      radio(in: _*)
        .grouped(40)
        .take(6)
        .map(_(19))
        .zipWithIndex
        .map(t => t._1 * (t._2 * 40 + 20))
        .sum

    def part2(register: Seq[Int]): String = {
      register.zipWithIndex
        .take(240)
        .map(t => (t._1, t._2 % 40 + 1))
        .map(t => if (t._2 >= t._1 && t._2 <= t._1 + 2) '#' else '.')
        .grouped(40)
        .map(_.mkString)
        .mkString("\n")
    }
  }

  import Solution._

  describe("Example case") {
    val inputTiny =
      """noop
        |addx 3
        |addx -5
        |""".stripMargin.split("\n").filter(_.nonEmpty)
    lazy val input = puzzleInput("Day10InputSmall.txt").filter(_.nonEmpty)

    it("should match the puzzle description") {
      radio(inputTiny: _*) shouldBe Seq(1, 1, 1, 4, 4, -1)
    }

    it("should match the puzzle example input") {
      val register = radio(input: _*)
      register(19) shouldBe 21
      register(59) shouldBe 19
      register(99) shouldBe 18
      register(139) shouldBe 21
      register(179) shouldBe 16
      register(219) shouldBe 18
      part1(input: _*) shouldBe 13140

      part2(register) shouldBe
        """##..##..##..##..##..##..##..##..##..##..
          |###...###...###...###...###...###...###.
          |####....####....####....####....####....
          |#####.....#####.....#####.....#####.....
          |######......######......######......####
          |#######.......#######.......#######.....
          |""".stripMargin.trim
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day10Input.txt").filter(_.nonEmpty)
    it("should have answers") {
      part1(input: _*) shouldBe decryptLong("2SQ5A9SPW8A3tmAqZAL4mQ==")
      val code = part2(radio(input: _*))
      code shouldBe decrypt(
        "CBuYmAlDkestd/UvVR3qJz93i2QJ+DU6jpOHpQ6rQ3yQvUW9uCO/wtaOq+xf8aLE/7xl" +
          "bah6M8jfldz4ze/fgKiDcEtKAAlGm8vD5MWNotc7s2cGQQ8a6p53aayyWwc7uirDqI" +
          "A4b376TsmZeL8O+KJ7QLFmYY4TyvJQ4gkN3z7zzw90k/p9ivwVpYl+a9ttJQsCnMeC" +
          "xgL8I0pCsZ9Gczk41fzrF1G0ONvseA3bR2Xk7c1UY3/1o8xumt1+w9p3mDYpQQqq8G" +
          "bcLq7DKkeh+xW1zxWBLH86LcRzT+JgmV7WwqaJOhMj7sHNsU5d8IQtTqDJZexJiwud" +
          "n8Sbi1/J9Q=="
      )
    }
  }
}
