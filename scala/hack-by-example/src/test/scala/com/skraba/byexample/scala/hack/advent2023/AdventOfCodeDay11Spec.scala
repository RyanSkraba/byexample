package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 11 Solutions in scala=
  *
  * Input: A map of galaxies in the sky.
  *
  * Part 1: Given a rate of expansion where all of the empty rows and columns have doubled in size, find the sum of the
  * taxi distances between each galaxy pair.
  *
  * Part 2: Given a rate of expansion where all of the empty rows and columns are a million times in size, find the sum
  * of the taxi distances between each galaxy pair.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/11]]
  */
class AdventOfCodeDay11Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def part1(in: String*): Long = part2(2, in: _*)

    def part2(age: Long, in: String*): Long = {
      // The positions of all the galaxies
      val glx =
        for (y <- in.indices; x <- in(y).indices if in(y)(x) == '#')
          yield (x, y)

      // Find all of the rows and columns that have galaxies
      val xs = glx.map(_._1).toSet
      val ys = glx.map(_._2).toSet

      // For each column, find if it has no galaxies, and transform that into
      // the count of empty columns to its left (expanded to the age of the
      // universe).  Same with rows.
      val noXs = (0 to xs.max)
        .map(xs(_))
        .scanLeft(0L) { (total, exists) =>
          if (exists) total else total + age - 1
        }
        .drop(1)
      val noYs = (0 to ys.max)
        .map(ys(_))
        .scanLeft(0L) { (total, exists) =>
          if (exists) total else total + age - 1
        }
        .drop(1)

      // We can then calculate the manhattan distance between any two galaxies by
      // adding the empty rows, columns.
      val distances =
        for (i1 <- glx.indices; i2 <- i1 + 1 until glx.size) yield {
          val ((x1, y1), (x2, y2)) = (glx(i1), glx(i2))
          math.abs(noXs(x1) + x1 - noXs(x2) - x2) + math.abs(
            noYs(y1) + y1 - noYs(y2) - y2
          )
        }

      distances.sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """...#......
        |.......#..
        |#.........
        |..........
        |......#...
        |.#........
        |.........#
        |..........
        |.......#..
        |#...#.....
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 374
    }

    it("should match the puzzle description for part 2") {
      part2(10, input: _*) shouldBe 1030
      part2(100, input: _*) shouldBe 8410
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day11Input.txt")
    lazy val answer1 = decryptLong("LBMs8fFs30lQX2fhFWXtGw==")
    lazy val answer2 = decryptLong("zjRaas4g+CQfTc1I/nrxbA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(1000000, input: _*) shouldBe answer2
    }
  }
}
