package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 5 Solutions in scala=
  *
  * Input: A list of page ordering rules where the tuple means that the first page number must occur before the second
  * page number (at any point, not necessarily immediately), followed by a list of page number sequences.
  *
  * Part 1: For all the sequences that are in order according to the rules, sum the middle page numbers.
  *
  * Part 2: For all the sequences that are not in order according to the rules, find a GOOD order and sum the middle
  * page numbers.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/5]]
  */
class AdventOfCodeDay5Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String*): (Set[(Long, Long)], Seq[Seq[Long]]) = {
      val (rulesPart, updatesPart) = in.span(_.trim.nonEmpty)
      val rules = rulesPart.map(_.split('|').map(_.toLong)).map { case Array(a, b) => (a, b) }.toSet
      val updates = updatesPart.tail.map(_.split(',').map(_.toLong)).map(_.toSeq)
      (rules, updates)
    }

    def hasViolation(rules: Set[(Long, Long)], update: Seq[Long]): Boolean = {
      for (i <- update.indices; j <- i + 1 until update.length)
        if (rules.contains((update(j), update(i)))) return true
      false
    }

    def part1(in: String*): Long = {
      val (rules, updates) = parse(in: _*)
      val ok = updates.filterNot(hasViolation(rules, _))
      ok.map(update => update(update.length / 2)).sum
    }

    def part2(in: String*): Long = {
      val (lt, updates) = parse(in: _*)
      val nok = updates.filter(hasViolation(lt, _))
      nok.map(_.sortWith(lt.apply(_, _))).map(update => update(update.length / 2)).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """47|53
        |97|13
        |97|61
        |97|47
        |75|29
        |61|13
        |75|53
        |29|13
        |97|29
        |53|29
        |61|53
        |97|53
        |61|29
        |47|13
        |75|47
        |97|75
        |47|61
        |75|61
        |47|29
        |75|13
        |53|13
        |
        |75,47,61,53,29
        |97,61,53,29,13
        |75,29,13
        |75,97,47,61,53
        |61,13,29
        |97,13,75,29,47
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 143
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 123
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day5Input.txt")
    lazy val answer1 = decryptLong("A+PjU5PiGTYUIPvczt6muA==")
    lazy val answer2 = decryptLong("rE5rFrykFbp7gVOScKRuFw==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
