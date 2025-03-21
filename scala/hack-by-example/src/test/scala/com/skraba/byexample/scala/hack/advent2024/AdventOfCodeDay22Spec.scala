package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 22 Solutions in scala=
  *
  * Input: A list of seeds for the monkey's random number generator with rules to find subsequent numbers.
  *
  * Part 1: Find the 2000th generated number.
  *
  * Part 2: Find a sequence of 4 deltas (differences between 5 consecutive numbers) that you can give to your monkey
  * agent to sell on the 4th number. Maximizing the
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/22]]
  */
class AdventOfCodeDay22Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def nextSecret(in: Long): Long = {
      val in1 = ((in << 6) ^ in) & 0xffffff
      val in2 = ((in1 >> 5) ^ in1) & 0xffffff
      ((in2 << 11) ^ in2) & 0xffffff
    }

    def secretAt(in: Long, n: Int): Long = LazyList.iterate(in)(nextSecret).drop(n).head

    def part1(in: String*): Long = in.map(_.toLong).map(secretAt(_, 2000)).sum

    def part2(in: String*): Long = {
      in.map(_.toLong) // The seed
        .map(LazyList.iterate(_)(nextSecret)) // The list of secrets
        .map(_.map(_ % 10))
        .map(_.sliding(2).take(1999))
        .map(_.map { case Seq(a, b) => b - a -> b }) // The deltas of the first 2000 secrets
        .map(_.sliding(4)) // The deltas of the first 2000 secrets grouped by
        .map(_.map { case Seq(d0 -> _, d1 -> _, d2 -> _, d3 -> v) => (d0, d1, d2, d3) -> v })
        .flatMap(_.toSeq.reverse.toMap.toSeq)
        .groupMapReduce(_._1)(_._2)(_ + _)
        .maxBy(_._2)
        ._2
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """1
        |10
        |100
        |2024
        |""".trim.stripMargin.split("\n")

    it("should calculate secrets correctly") {

      LazyList.iterate(123L)(nextSecret).take(11) shouldBe
        Seq(123, 15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432, 5908254)

      secretAt(1, 2000) shouldBe 8685429
      secretAt(10, 2000) shouldBe 4700978
      secretAt(100, 2000) shouldBe 15273692
      secretAt(2024, 2000) shouldBe 8667524
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 37327623
    }

    it("should match the puzzle description for part 2") {
      part2("123") shouldBe 9
      part2(input: _*) shouldBe 24
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day22Input.txt")
    lazy val answer1 = decryptLong("7jdAPbNjB1Cmq2TPJGAyWQ==")
    lazy val answer2 = decryptLong("wds1JaNwqmj9WNB7VVEsIA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
