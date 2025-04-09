package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** =Advent of Code 2022 Day 5 Solutions in scala=
  *
  * Input: A text file where the first lines describe how crates are stacked in the original configuration and the
  * following lines are a plan to move them (N crates from SRC stack to DST stack). Each crate is a letter.
  *
  * Part 1: A string containing the top crate letter from each stack, if moving N crates moves them one at a time.
  *
  * Part 2: A string containing the top crate letter from each stack, if moving N crates moves them all at once (keeping
  * the top at the top).
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/5]]
  */
class AdventOfCodeDay5Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    val Mover: Regex = """move (\d+) from (\d+) to (\d+)""".r

    /** @return The start state of the crate stacks from the input. */
    def parseCrates(in: String*): Seq[String] = {
      val crates: List[String] = in.takeWhile(_.nonEmpty).toList
      crates
        .map(_.padTo(crates.last.length + 1, ' '))
        .transpose
        .map(_.filter(_.isLetter).mkString)
        .filter(_.nonEmpty)
    }

    /** @return The crate movement instructions from the input */
    def parseMoves(in: String*): Seq[(Int, Int, Int)] = {
      in.collect { case Mover(n, src, dst) => (n.toInt, src.toInt - 1, dst.toInt - 1) }
    }

    def part1(crates: Seq[String], moves: Seq[(Int, Int, Int)]): Seq[String] = {
      moves.foldLeft(crates) { case (c, (n, src, dst)) =>
        c.updated(src, c(src).drop(n))
          .updated(dst, c(src).take(n).reverse + c(dst))
      }
    }

    def part2(crates: Seq[String], moves: Seq[(Int, Int, Int)]): Seq[String] = {
      moves.foldLeft(crates) { case (c, (n, src, dst)) =>
        c.updated(src, c(src).drop(n))
          .updated(dst, c(src).take(n) + c(dst))
      }
    }

    /** @return Every top item on each stack as a String. */
    def topCrates(crates: Seq[String]): String =
      crates.map(_.head).mkString
  }

  import Solution._

  describe("Example case") {
    val input =
      """    [D]
        |[N] [C]
        |[Z] [M] [P]
        | 1   2   3
        |
        |move 1 from 2 to 1
        |move 3 from 1 to 3
        |move 2 from 2 to 1
        |move 1 from 1 to 2
        |""".stripMargin.split("\n").toSeq

    it("should match the puzzle description") {
      val crates = parseCrates(input: _*)
      crates shouldBe Seq("NZ", "DCM", "P")
      val moves = parseMoves(input: _*)
      moves should have size 4
      moves.head shouldBe (1, 1, 0)
      moves(1) shouldBe (3, 0, 2)
      moves(2) shouldBe (2, 1, 0)
      moves(3) shouldBe Tuple3(1, 0, 1)
      val newCrates = part1(crates, moves)
      newCrates shouldBe Seq("C", "M", "ZNDP")
      topCrates(newCrates) shouldBe "CMZ"
      val newCrates2 = part2(crates, moves)
      newCrates2 shouldBe Seq("M", "C", "DNZP")
      topCrates(newCrates2) shouldBe "MCD"
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day5Input.txt")
    it("should have answers") {
      val crates = parseCrates(input: _*)
      crates.mkString(",") shouldBe decrypt(
        "vPQOujFIkxxh9/5ADbAm6/49KEva6I+u7ZxbrRodqh7cZouYfV2Lw1dreSYnVqUnoH5WDDzs/lpq3czjkkLcV6g9OALaDYL2V8G8vBhq+qw="
      )

      val moves = parseMoves(input: _*)
      moves should have size decryptLong("NLfxxhehN14Tbgc7kiV/Eg==")
      moves.head shouldBe (2, 3, 1)
      moves.last shouldBe (1, 7, 5)

      val newCrates = part1(crates, moves)
      newCrates.mkString(",") shouldBe decrypt(
        "2+ScvkQDIOVhlHb473W1EqZb94ia8LlQC5eHgkdIeh5Am7pnnQS3H+o5PWfWXtTTi+gV8gWXL/8Eq/yD+LE1xKg9OALaDYL2V8G8vBhq+qw="
      )
      topCrates(newCrates) shouldBe decrypt("yJ63bO0goBLZSY6eubQ0Bw==")

      val newCrates2 = part2(crates, moves)
      newCrates2.mkString(",") shouldBe decrypt(
        "Et5+FYu5bHmHXR3vb1fz34KrPD/rp2EOwpTGiZl66majJ0eQnUZMRq569guK365XJ93cc/n9ldVVk8CrYkA3I6g9OALaDYL2V8G8vBhq+qw="
      )
      topCrates(newCrates2) shouldBe decrypt("6RX8dcRlvo+UnaeE1gHrnQ==")
    }
  }
}
