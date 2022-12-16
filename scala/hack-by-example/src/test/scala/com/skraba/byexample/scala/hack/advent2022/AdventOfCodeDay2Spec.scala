package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 2 Solutions in scala=
  *
  * Input: "Strategy guide" where each line's first column is A, B, C and the
  * second column is X, Y, Z, for playing Rock, Paper, Scissors. A, B, C
  * corresponds to the opponent playing Rock, Paper or Scissors respectively
  *
  * For each match, playing Rock, Paper or Scissors gives you 1, 2 or 3 points
  * (respectively) PLUS Winning gives you 6 points, and a draw is 3 points. No
  * points for losing.
  *
  * Part 1: If X, Y, Z corresponds to me playing Rock, Paper or Scissors
  * respectively, find the total number of points.
  *
  * Part 2: If X, Y, Z corresponds to me losing, drawing or winning
  * respectively, find the total number of points.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/2]]
  */
class AdventOfCodeDay2Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    def naiveStrategyGuide1(in: String*): Long = {
      in.foldLeft(0L)((acc, str) =>
        str.split(" ") match {
          case Array("A", "X") => acc + 1 + 3 // Rock vs Rock, Draw
          case Array("A", "Y") => acc + 2 + 6 // Rock vs Paper, Win
          case Array("A", "Z") => acc + 3 + 0 // Rock vs Scissors, Lose
          case Array("B", "X") => acc + 1 + 0 // Paper vs Rock, Lose
          case Array("B", "Y") => acc + 2 + 3 // Paper vs Paper, Draw
          case Array("B", "Z") => acc + 3 + 6 // Paper vs Scissors, Win
          case Array("C", "X") => acc + 1 + 6 // Scissors vs Rock, Win
          case Array("C", "Y") => acc + 2 + 0 // Scissors vs Paper, Lose
          case Array("C", "Z") => acc + 3 + 3 // Scissors vs Scissors, Draw
        }
      )
    }

    def naiveStrategyGuide2(in: String*): Long = {
      in.foldLeft(0L)((acc, str) =>
        str.split(" ") match {
          case Array("A", "X") => acc + 0 + 3 // Lose to Rock, Play Scissors
          case Array("A", "Y") => acc + 3 + 1 // Draw to Rock, Play Rock
          case Array("A", "Z") => acc + 6 + 2 // Win Rock, Play Paper
          case Array("B", "X") => acc + 0 + 1 // Lose to Paper, Play Rock
          case Array("B", "Y") => acc + 3 + 2 // Draw to Paper, Play Paper
          case Array("B", "Z") => acc + 6 + 3 // Win Paper, Play Scissors
          case Array("C", "X") => acc + 0 + 2 // Lose to Scissors, Play Paper
          case Array("C", "Y") => acc + 3 + 3 // Draw to Scissors, Play Scissors
          case Array("C", "Z") => acc + 6 + 1 // Win Scissors, Play Rock
        }
      )
    }

    def strategyGuide1(in: String*): Long = {
      in.map(x => {
        val them = x(0) - 'A'
        val me = x(2) - 'X'
        (me + 1) + ((4 + me - them) % 3) * 3
      }).sum
    }

    def strategyGuide2(in: String*): Long = {
      in.map(x => {
        val them = x(0) - 'A'
        val ldw = x(2) - 'X'
        ldw * 3 + (2 + ldw + them) % 3 + 1
      }).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """A Y
        |B X
        |C Z""".stripMargin.split("\n")

    it("should match the puzzle description") {
      naiveStrategyGuide1(input: _*) shouldBe 15
      naiveStrategyGuide2(input: _*) shouldBe 12
      strategyGuide1(input: _*) shouldBe 15
      strategyGuide2(input: _*) shouldBe 12
    }

    it("should match the naive and other solution") {
      for (them <- Seq("A", "B", "C"))
        for (me <- Seq("X", "Y", "Y")) {
          naiveStrategyGuide1(s"$them $me") shouldBe strategyGuide1(
            s"$them $me"
          )
          naiveStrategyGuide2(s"$them $me") shouldBe strategyGuide2(
            s"$them $me"
          )
        }
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day2Input.txt")
    it("should have answers") {
      Solution.naiveStrategyGuide1(input: _*) shouldBe 14069
      Solution.naiveStrategyGuide2(input: _*) shouldBe 12411
      Solution.strategyGuide1(input: _*) shouldBe 14069
      Solution.strategyGuide2(input: _*) shouldBe 12411
    }
  }
}
