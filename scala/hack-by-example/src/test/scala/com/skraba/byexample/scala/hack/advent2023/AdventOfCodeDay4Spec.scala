package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 4 Solutions in scala=
  *
  * Input: A list of scratch tickets, each line showing a set of drawn numbers and the actual numbers. A match occurs
  * when the same number appears on both sides of the bar.
  *
  * Part 1: A ticket is a winner if there is at least one match, which is worth 1 point, and every subsequent match on
  * the same line doubles the points (four matches gives 8 points). Find the sum of all the winning tickets.
  *
  * Part 2: A winning ticket doesn't' give you points, but actually more tickets: 4 matches on ticket gives you a copy
  * of N+1, N+2, and N+3. Find how many tickets you have once you
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/4]]
  */
class AdventOfCodeDay4Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** @param in
      *   One input line
      * @return
      *   A sequence containing two sets: drawn numbers and the actual numbers
      */
    def parse(in: String): Seq[Set[String]] =
      in.split("[:|]").toSeq.drop(1).map(_.trim.split("\\s+").toSet)

    def part1(in: String*): Long = in
      .map(parse)
      // then the matching numbers
      .map(n => n.head.intersect(n(1)).size)
      // only keep the winners
      .filter(_ > 0)
      // raise to the correct power of two and return the sum
      .map(_ - 1)
      .map(math.pow(2, _))
      .sum
      .toLong

    def part2a(in: String*): Long = {
      in.map(parse) // then the matching numbers
        .map(n => n.head.intersect(n(1)).size)
        // iterate over the list to accumulate the  total number of tickets won so far
        // the first value is the total
        // the second value is a list of past winners that contribute a free copy of the current ticket
        .foldLeft[(Long, Seq[Int])]((0L, Nil)) { case ((total, bonus), matches) =>
          // OK, not gonna lie, I got this to to work first and now I can't figure out why it works.
          val nextBonus = bonus.map(_ - 1).filter(_ > 0)
          if (matches > 0) {
            (
              total + 1 + (1 + bonus.size) * matches,
              nextBonus ++ Seq.fill(1 + bonus.size)(matches)
            )
          } else {
            (total + 1, nextBonus)
          }
        }
        ._1
    }

    def part2b(in: String*): Long = {
      in.map(parse) // then the matching numbers
        .map(n => n.head.intersect(n(1)).size)
        // iterate over the list to accumulate the  total number of tickets won so far:
        // The accumulator is (1) the total number of tickets so far and (2) a list of the next free tickets
        // that have been won.
        .foldLeft[(Long, Seq[Int])]((0L, Nil)) { case ((total, winnings), matches) =>
          // The total copies of this ticket that we possess: the original plus all of the ones we have won.
          val copies = 1 + winnings.headOption.getOrElse(0)
          // Calculate the bonus for the subsequent tickets: if this is a winner, add a free ticket for
          // every ticket we have to the next N tickets.'
          val nextBonus = winnings
            .drop(1)
            .padTo(matches, 0)
            .take(matches)
            .map(_ + copies) ++ winnings.drop(1 + matches)
          (total + copies, nextBonus)
        }
        ._1
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
        |Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
        |Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
        |Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
        |Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
        |Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 13
    }

    it("should match the puzzle description for part 2 (first try)") {
      part2a(input: _*) shouldBe 30
    }

    it("should match the puzzle description for part 2 (second try)") {
      part2b(input: _*) shouldBe 30
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day4Input.txt")
    lazy val answer1 = decryptLong("JLTsmXEpbPjfqojgtlN6zg==")
    lazy val answer2 = decryptLong("CBGoEJivAu8oQbYZJTfJ5g==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2 (first try)") {
      part2a(input: _*) shouldBe answer2
    }

    it("should have answers for part 2 (second try)") {
      part2b(input: _*) shouldBe answer2
    }
  }
}
