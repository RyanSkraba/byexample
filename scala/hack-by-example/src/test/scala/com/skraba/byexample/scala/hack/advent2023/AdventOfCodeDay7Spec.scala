package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 7 Solutions in scala=
  *
  * Input: A list of poker hands with bids. There are some special rules for ordering the poker hands.
  *
  * Part 1: When ordering the poker hands from weakest to strongest, the sum of rank*bid.
  *
  * Part 2: Same as Part 1 except that Jacks are counted as Jokers.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/7]]
  */
class AdventOfCodeDay7Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Hand(cards: String, bid: Long) {

      /** Group the cards to find the type. */
      lazy val p1Groups: Seq[Int] = cards
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .values
        .toSeq
        .sorted(Ordering.Int.reverse)

      /** Map the type to a number for ordering. */
      lazy val p1Type: Int = p1Groups match {
        case Seq(5)       => 6 // Five of a kind
        case Seq(4, 1)    => 5 // Four of a kind
        case Seq(3, 2)    => 4 // Full house
        case Seq(3, 1, 1) => 3 // Three of a kind
        case Seq(2, 2, 1) => 2 // Two pair
        case Seq(2, _*)   => 1 // Pair
        case _            => 0 // Pair
      }

      /** Map the cards to a sortable string so the stronger values come later.
        */
      lazy val p1Rank: String = cards
        .map("23456789TJQKA".indexOf(_).toChar)
        .map(_ + 'a' - '0')
        .map(_.toChar)
        .mkString

      /** Group the cards to find the type, ignoring jokers. */
      lazy val p2Groups: Seq[Int] = cards
        .filter(_ != 'J')
        .groupMapReduce(identity)(_ => 1)(_ + _)
        .values
        .toSeq
        .sorted(Ordering.Int.reverse)

      /** The number of jokers available. */
      lazy val jokers: Int = cards.count(_ == 'J')

      lazy val p2Type: Int = p2Groups match {
        case _ if jokers == 5                  => 6
        case Seq(top, _*) if top + jokers == 5 => 6 // Five of a kind
        case Seq(top, _*) if top + jokers == 4 => 5 // Four of a kind
        case Seq(2, 2) if jokers == 1          => 4 // Full house
        case Seq(3, 2)                         => 4 // Full house
        case Seq(top, _*) if top + jokers == 3 => 3 // Three of a kind
        case Seq(2, 2, 1)                      => 2 // Two pair
        case Seq(top, _*) if top + jokers == 2 => 1 // Pair
        case _                                 => 0 // Pair
      }

      /** Map the cards to a sortable string so the stronger values come later.
        */
      lazy val p2Rank: String = cards
        .map("J23456789TQKA".indexOf(_).toChar)
        .map(_ + 'a' - '0')
        .map(_.toChar)
        .mkString
    }

    class Ordering[Hand]

    def part1(in: String*): Long = {
      val hands: Seq[Hand] = in.map { line =>
        val h = line.split("\\s+")
        Hand(h.head, h(1).toLong)
      }
      val ranked = hands.sorted(
        Ordering.by[Hand, (Int, String)](h => (h.p1Type, h.p1Rank))
      )
      ranked.zipWithIndex.map(rh => rh._1.bid * (1 + rh._2)).sum
    }

    def part2(in: String*): Long = {
      val hands: Seq[Hand] = in.map { line =>
        val h = line.split("\\s+")
        Hand(h.head, h(1).toLong)
      }
      val ranked = hands.sorted(
        Ordering.by[Hand, (Int, String)](h => (h.p2Type, h.p2Rank))
      )
      ranked.zipWithIndex.map(rh => rh._1.bid * (1 + rh._2)).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """32T3K 765
        |T55J5 684
        |KK677 28
        |KTJJT 220
        |QQQJA 483
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 6440
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 5905
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day7Input.txt")
    lazy val answer1 = decryptLong("Sf3lVwWtjHKg1UekRV/UGg==")
    lazy val answer2 = decryptLong("H6H7Ql91QEV8rc2Mt3JNig==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
