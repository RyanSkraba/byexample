package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** =Advent of Code 2024 Day 7 Solutions in scala=
  *
  * Input: A list of calibration test values separated by a colon from a list of numbers. By applying an operation to
  * these lists from left to right (NOT order of operations), determine whether we can obtain the test value.
  *
  * Part 1: If addition and multiplication are the only operations, find the rows that can be solved and return the sum
  * of their test values.
  *
  * Part 2: If addition, multiplication and concatenation are the possible operations, find the rows that can be solved
  * and return the sum of their test values.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/7]]
  */
class AdventOfCodeDay7Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String): (Long, Seq[Long]) = in.split(':') match {
      case Array(k, v) => (k.trim.toLong, v.trim.split(' ').map(_.toLong).toSeq)
    }

    def part1(in: String*): Long = {
      def solve(testValue: Long, xs: Seq[Long]): Boolean =
        if (xs.length == 1) xs.contains(testValue)
        else
          (testValue % xs.last == 0 && solve(testValue / xs.last, xs.init)) ||
          (testValue >= xs.last && solve(testValue - xs.last, xs.init))
      in.map(parse).filter { case (k, v) => solve(k, v) }.map(_._1).sum
    }

    def part2(in: String*): Long = {
      // Checking all the possible answers using a depth-first search, via lazy lists.
      def possible(xs: Seq[Long]): LazyList[Long] =
        if (xs.length == 1) LazyList(xs.head)
        else {
          val x1 = xs.head
          val x2 = xs(1)
          possible((x1 + x2) +: xs.drop(2)) ++ possible((x1 * x2) +: xs.drop(2)) ++ possible(
            (x1.toString + x2.toString).toLong +: xs.drop(2)
          )
        }
      in.map(parse).filter { case (result, xs) => possible(xs).contains(result) }.map(_._1).sum
    }

    def part2recursive(in: String*): Long = {
      def solve(testValue: Long, xs: Seq[Long]): Boolean =
        if (xs.length == 1) xs.contains(testValue)
        else
          (testValue % xs.last == 0 && solve(testValue / xs.last, xs.init)) ||
          (testValue >= xs.last && solve(testValue - xs.last, xs.init)) ||
          (testValue != xs.last &&
            testValue.toString.endsWith(xs.last.toString) &&
            solve(testValue.toString.dropRight(xs.last.toString.length).toLong, xs.init))
      in.map(parse).filter { case (k, v) => solve(k, v) }.map(_._1).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """190: 10 19
        |3267: 81 40 27
        |83: 17 5
        |156: 15 6
        |7290: 6 8 6 15
        |161011: 16 10 13
        |192: 17 8 14
        |21037: 9 7 18 13
        |292: 11 6 16 20
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 3749
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 11387
      part2recursive(input: _*) shouldBe 11387
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day7Input.txt")
    lazy val answer1 = decryptLong("WbrKjSThwDfv3cCbpLDaZg==")
    lazy val answer2 = decryptLong("n6oo2DHWY/odOIddzzTssw==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2 (7 seconds)", Slow) {
      part2(input: _*) shouldBe answer2
    }

    it("should have answers for part 2b") {
      part2recursive(input: _*) shouldBe answer2
    }
  }
}
