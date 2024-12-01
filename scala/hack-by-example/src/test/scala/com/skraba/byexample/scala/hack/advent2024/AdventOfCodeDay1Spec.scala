package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 1 Solutions in scala=
  *
  * Input: Two columns of numbers, where each line has a pair separated by spaces.
  *
  * Part 1: Sorting each column, find the sum of the differences between each pair.
  *
  * Part 2: Find a "similarity score": for every number in the first column multiply it by the number of times it
  * appears in the second column.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/1]]
  */
class AdventOfCodeDay1Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String): Option[(Long, Long)] = in.trim.split("\\s+") match {
      case Array(a, b, _*) => Some((a.toLong, b.toLong))
    }

    def part1(in: String*): Long = {
      val input = in.flatMap(parse)
      input.map(_._1).sorted.zip(input.map(_._2).sorted).map { case (a: Long, b: Long) => Math.abs(a - b) }.sum
    }

    def part2(in: String*): Long = {
      val input = in.flatMap(parse)
      similarity(0, input.map(_._1).sorted, input.map(_._2).sorted)
    }

    def similarity(count: Long, a: Seq[Long], b: Seq[Long]): Long = {
      // Drop elements from the sorted lists until the head is the same.
      if (a.isEmpty || b.isEmpty) return count
      if (a.head < b.head) return similarity(count, a.tail, b)
      if (a.head > b.head) return similarity(count, a, b.tail)
      // The top element is the same, count the number of times it exists in both lists
      val (aTop, aRest) = a.span(_ == a.head)
      val (bTop, bRest) = b.span(_ == a.head)
      similarity(count + a.head * aTop.length * bTop.length, aRest, bRest)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """|3   4
        |4   3
        |2   5
        |1   3
        |3   9
        |3   3
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 11
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 31
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day1Input.txt")
    lazy val answer1 = decryptLong("WXoklhpW8/wmzQfDhR3ucw==")
    lazy val answer2 = decryptLong("OkbnklQHY7HnsHWK/NmSyA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
