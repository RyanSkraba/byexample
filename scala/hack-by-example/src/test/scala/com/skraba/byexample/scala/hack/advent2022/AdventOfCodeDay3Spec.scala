package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 3 Solutions in scala=
  *
  * Input: Lines of rucksack items, each case-sensitive letter corresponds to an item in the rucksack. The first half of
  * the line is in the first compartment of the backpack, and the second half is in the second compartment.
  *
  * The priority of an item is 'a'-'z' 1-26 and 'A'-'Z' 27-52
  *
  * Part 1: Find the duplicate item in the first and second compartment for each backpack and sum the priorities.
  *
  * Part 2: For each group of three elves, find the common item in their three backpacks and sum the priorities.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/3]]
  */
class AdventOfCodeDay3Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {
    def priority(c: Char): Long = if (c >= 'a') c - 'a' + 1 else c - 'A' + 27

    def findDup(in: String): Option[Char] = {
      val c1 = in.slice(0, in.length / 2).toSet
      in.substring(in.length / 2).find(c1.contains)
    }

    def priorities(in: String*): Long = in.flatMap(findDup).map(priority).sum

    def badge(in: String*): Iterator[Char] = in
      .grouped(3)
      .map(elves => elves.tail.foldLeft(elves.head)((acc, elf) => acc.intersect(elf)))
      .flatMap(_.headOption)

    def badgePriorities(in: String*): Long = badge(in: _*).map(priority).sum
  }

  import Solution._

  describe("Example case") {
    val input =
      """vJrwpWtwJgWrhcsFMMfFFhFp
        |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
        |PmmdzqPrVvPwwTWBwg
        |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
        |ttgJtRGJQctTZtZT
        |CrZsJsPPZsGzwwsLwLmpwMDw""".trimSplit

    it("should match the puzzle description") {
      findDup(input(0)) shouldBe Some('p')
      findDup(input(1)) shouldBe Some('L')
      findDup(input(2)) shouldBe Some('P')
      findDup(input(3)) shouldBe Some('v')
      findDup(input(4)) shouldBe Some('t')
      findDup(input(5)) shouldBe Some('s')
      priorities(input: _*) shouldBe 157
      badge(input: _*).toSeq shouldBe Seq('r', 'Z')
      badgePriorities(input: _*) shouldBe 70
      // Check for trailing lines
      badge(input :+ "": _*).toSeq shouldBe Seq('r', 'Z')
      badgePriorities(input :+ "": _*) shouldBe 70
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day3Input.txt")
    lazy val answer1 = decryptLong("zOWo23t75p2Y4PYlepCTtA==")
    lazy val answer2 = decryptLong("WSPQaXm5LkMz/vyB6UDUzw==")

    it("should have answers for part 1") {
      priorities(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      badgePriorities(input: _*) shouldBe answer2
    }
  }
}
