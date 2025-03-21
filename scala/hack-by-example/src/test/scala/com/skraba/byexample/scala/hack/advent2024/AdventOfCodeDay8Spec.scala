package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 8 Solutions in scala=
  *
  * Input: A map of antennas and their positions on a grid. Each antenna has a character identifier.
  *
  * Part 1: For all antennas with the same identifier, find the number of antinodes which are defined as the points on
  * the same line between two antennas, such that each antenna has the same distance between it and one antinode. Count
  * the number of unique antinodes in the grid.
  *
  * Part 2: For all antennas with the same identifier, find the number of antinodes which are defined as the points on
  * the same line between two antennas, such that each antenna or antinode are the same distance apart.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/8]]
  */
class AdventOfCodeDay8Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Pos(x: Long, y: Long) {
      def move(dx: Long, dy: Long): Pos = Pos(x + dx, y + dy)
      def inBounds(w: Long, h: Long): Boolean = x >= 0 && y >= 0 && x < w && y < h
    }

    def parse(in: String*): Map[Char, Seq[Pos]] =
      (for (y <- in.indices; x <- in(y).indices if in(y)(x) != '.')
        yield in.apply(y)(x) -> Pos(x.toLong, y.toLong))
        .groupMap(_._1)(_._2)

    def part1(in: String*): Long = {
      val antinodes = parse(in: _*).flatMap { case (_, xs) =>
        for (i <- xs.indices; j <- i + 1 until xs.length; x1 = xs(i); x2 = xs(j)) yield {
          val (dx, dy) = (x2.x - x1.x, x2.y - x1.y)
          Seq(x1.move(-dx, -dy), x2.move(dx, dy))
        }
      }
      antinodes.flatten.filter(_.inBounds(in.head.length, in.length)).toSet.size
    }

    def part2(in: String*): Long = {
      val antinodes = parse(in: _*).flatMap { case (_, xs) =>
        for (i <- xs.indices; j <- i + 1 until xs.length; x1 = xs(i); x2 = xs(j)) yield {
          val (dx, dy) = (x2.x - x1.x, x2.y - x1.y)
          LazyList.from(0).map(m => x1.move(-m * dx, -m * dy)).takeWhile(_.inBounds(in.head.length, in.length)) ++
            LazyList.from(0).map(m => x2.move(m * dx, m * dy)).takeWhile(_.inBounds(in.head.length, in.length))
        }
      }
      antinodes.flatten.toSet.size
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """............
        |........0...
        |.....0......
        |.......0....
        |....0.......
        |......A.....
        |............
        |............
        |........A...
        |.........A..
        |............
        |............
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 14
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 34
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day8Input.txt")
    lazy val answer1 = decryptLong("Scyue8ceCKzPChFnHYXyRw==")
    lazy val answer2 = decryptLong("R3zuO2nx5k/pn+fgCFX9vQ==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
