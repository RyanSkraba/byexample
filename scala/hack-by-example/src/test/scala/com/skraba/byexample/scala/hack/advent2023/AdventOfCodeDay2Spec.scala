package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 2 Solutions in scala=
  *
  * Input: A list of strings, where each line corresponds to a game being played
  * taking coloured cubes out of out of a bag. Each game has an ID.
  *
  * Part 1: Given a certain maximum number of coloured cubes, find the sum of
  * the game IDs in the list that are possible.
  *
  * Part 2: For each game, find the product of the minimum number of each
  * coloured cube required to play that game, and sum this power over the entire
  * list.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/2]]
  */
class AdventOfCodeDay2Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Game(id: Long, bs: Seq[Sample]) {
      lazy val power: Long =
        bs.map(_.red).max * bs.map(_.green).max * bs.map(_.blue).max
    }

    object Game {
      def apply(in: String): Game =
        in.dropWhile(!_.isDigit).span(_.isDigit) match {
          case (id, rest) =>
            Game(
              id.toLong,
              rest
                .dropWhile(!_.isDigit)
                .split(';')
                .map(_.split(','))
                .map(Sample(_))
            )
        }

      def parse(in: String*): Seq[Game] = in.filter(_.nonEmpty).map(Game.apply)
    }

    case class Sample(red: Long = 0, green: Long = 0, blue: Long = 0)

    object Sample {
      def apply(in: Seq[String]): Sample =
        in.map(_.trim.split(' '))
          .foldLeft(Sample())((acc, countColour) =>
            countColour match {
              case Array(count, "red")   => acc.copy(red = count.toLong)
              case Array(count, "green") => acc.copy(green = count.toLong)
              case Array(count, "blue")  => acc.copy(blue = count.toLong)
              case _                     => acc
            }
          )
    }
  }

  import Solution._

  def extractGamesWithMax(maxR: Long, maxG: Long, maxB: Long, in: Game*): Long =
    in.filter(
      !_.bs.exists(b => b.red > maxR || b.green > maxG || b.blue > maxB)
    ).map(_.id)
      .sum

  def extractPower(in: Game*): Long =
    in.map(_.power).sum

  describe("Example case") {
    val input =
      """Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        |Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        |Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        |Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        |Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description") {
      Game(input.head) shouldBe Game(
        1,
        Seq(
          Sample(blue = 3, red = 4),
          Sample(red = 1, green = 2, blue = 6),
          Sample(green = 2)
        )
      )
      val games: Seq[Game] = Game.parse(input: _*)
      extractGamesWithMax(12, 13, 14, games: _*) shouldBe 8
      extractPower(games: _*) shouldBe 2286

    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day2Input.txt")
    lazy val games: Seq[Game] = Game.parse(input: _*)
    lazy val answer1 = decryptLong("heNEtKBib+to9TK7YnwNZQ==")
    lazy val answer2 = decryptLong("AxCsgqZR+rkhpND0BLl/Cg==")

    it("should have answers for part 1") {
      extractGamesWithMax(12, 13, 14, games: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      extractPower(games: _*) shouldBe answer2
    }
  }
}
