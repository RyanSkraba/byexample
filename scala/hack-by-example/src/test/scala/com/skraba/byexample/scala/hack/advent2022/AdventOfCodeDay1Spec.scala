package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 1 Solutions in scala=
  *
  * Input: List of numbers that correspond to the calories of the snacks that elves are carrying. Consecutive lines
  * belong to the same elf, a blank line is the next elf.
  *
  * Part 1: Find the maximum total number of calories carried by an elf.
  *
  * Part 2: Find the sum of the top three elves (by total number of calories).
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/1]]
  */
class AdventOfCodeDay1Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def elfWithMostCalories(in: String*): Long = {
      val acc = in.foldLeft((0L, 0L))((acc, str) =>
        str match {
          case n if str.nonEmpty => (acc._1, acc._2 + n.toLong)
          case _                 => (Math.max(acc._1, acc._2), 0)
        }
      )
      Math.max(acc._1, acc._2)
    }

    def threeElvesWithMostCalories(in: String*): Long = {
      val acc = in.foldLeft[(List[Long], Long)](Nil, 0L)((acc, str) =>
        str match {
          case n if str.nonEmpty => (acc._1, acc._2 + n.toLong)
          case _                 => (acc._1 :+ acc._2, 0)
        }
      )
      (acc._1 :+ acc._2).sorted.reverse.take(3).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """1000
        |2000
        |3000
        |
        |4000
        |
        |5000
        |6000
        |
        |7000
        |8000
        |9000
        |
        |10000""".trimSplit

    it("should match the puzzle description") {
      elfWithMostCalories(input: _*) shouldBe 24000
      threeElvesWithMostCalories(input: _*) shouldBe 45000
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day1Input.txt")
    lazy val answer1 = decryptLong("FTYpuJrtzel2PvCVTEIppw==")
    lazy val answer2 = decryptLong("RelObyfSNaKcVxgSuHYRUg==")

    it("should have answers for part 1") {
      elfWithMostCalories(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      threeElvesWithMostCalories(input: _*) shouldBe answer2
    }
  }
}
