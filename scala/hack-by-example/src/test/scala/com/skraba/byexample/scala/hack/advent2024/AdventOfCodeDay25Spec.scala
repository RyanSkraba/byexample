package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 25 Solutions in scala=
  *
  * Input: A 5x7 grids, creating the shape of either a key ({{{#}}} pointing up from the bottom) or a lock (growing down
  * from the top). A key always has the bottom row filled and the top row empty and the lock is the opposite.
  *
  * Part 1: Find how many sets of locks and keys that have shapes that can be overlayed without any filled blocks (#)
  * overlapping.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/25]]
  */
class AdventOfCodeDay25Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def part1(in: String*): Long = {

      // Read the keys and locks from the input, transposing so locks shapes are on the left and keys on the right
      val shapes = in.grouped(8).map(_.filter(_.nonEmpty).transpose.map(_.mkString)).toSeq
      val dx = in.head.length

      // Make each shape a list of Int counting the filled blocks (#) per row
      val (locks, keys) = shapes.map(s => (s.head.head == '#') -> s.map(_.count(_ == '#') - 1)).partition(_._1) match {
        case (l, k) => l.map(_._2) -> k.map(_._2)
      }

      (for (
        k <- keys; l <- locks;
        x = k zip l if x.forall { case (ks, ls) => ks + ls <= 5 }
      ) yield true).size
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """#####
        |.####
        |.####
        |.####
        |.#.#.
        |.#...
        |.....
        |
        |#####
        |##.##
        |.#.##
        |...##
        |...#.
        |...#.
        |.....
        |
        |.....
        |#....
        |#....
        |#...#
        |#.#.#
        |#.###
        |#####
        |
        |.....
        |.....
        |#.#..
        |###..
        |###.#
        |###.#
        |#####
        |
        |.....
        |.....
        |.....
        |#....
        |#.#..
        |#.#.#
        |#####
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 3
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day25Input.txt")
    lazy val answer1 = decryptLong("uKD4IvI/ImPxjKXdYgZCzA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }
  }
}
