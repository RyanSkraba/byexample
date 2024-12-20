package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.collection.mutable

/** =Advent of Code 2024 Day 20 Solutions in scala=
  *
  * Input: A maze with walls defined by '#', and a single non-branching path starting from 'S' and ending at 'E'. Since
  * there is only one path, there's a fixed time to finish the maze where one step costs one picosecond.
  *
  * Part 1: You can turn off the walls for two picoseconds (meaning that you can pass through at most two walls) to take
  * a shortcut. A unique shortcut is defined from one point on the original path to another point on the original path.
  * How many shortcuts can save you at least 100 picoseconds?
  *
  * Part 2: You can turn off the walls for twenty picoseconds. How many shortcuts can save you 100 picoseconds now?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/20]]
  */
class AdventOfCodeDay20Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def part1(in: String*): Long = solve(3, 100, in: _*).map(_._2).sum

    def part2(in: String*): Long = solve(21, 100, in: _*).map(_._2).sum

    /** Find the different ways you can cheat to save time in the maze.
      *
      * @param cheat
      *   The number of picoseconds the walls are turned off plus ONE. This is the maximum distance you can travel from
      *   one valid step on the input path to another valid step.
      * @param in
      *   The maze as a sequence of strings.
      * @param min
      *   The minimum time you have to save off the original route to be counted.
      * @return
      *   A list of tuples corresponding to how much time could be saved due to shortcuts, in the form (saved -> count)
      *   where {{count}} is the number of unique shortcuts that shave {{saved}} picoseconds off the time.
      */
    def solve(cheat: Int, min: Int, in: String*): Seq[(Int, Int)] = {
      val (dx, dy) = (in.head.length, in.length)
      val plan: String = in.mkString
      val start = plan.indexOf('S')

      // The original path in reverse order.
      val path: Seq[Int] = LazyList
        .iterate(List(start)) { path =>
          Seq(1, dx, -1, -dx)
            .map(_ + path.head)
            .filterNot(path.length > 1 && path(1) == _) // Don't go back
            .find(plan(_) != '#')
            .head :: path
        }
        .find(path => plan(path.head) == 'E')
        .get

      // Map from a position in plan to the distance from the end on the original path.
      val distance = path.zipWithIndex.toMap

      // Given any position, find any cheat to another position that cause the distance to decrease.
      def cheatsFrom(pos: Int): Seq[Int] = {
        val d0 = distance(pos)
        for (
          xTaxi <- -cheat to cheat;
          x = pos % dx + xTaxi if x >= 1 && x < (dx - 1);
          yTaxi <- -cheat to cheat;
          y = pos / dx + yTaxi if y >= 1 && y < (dy - 1);
          taxi = xTaxi.abs + yTaxi.abs if taxi < cheat;
          newPos = y * dx + x if distance.contains(newPos);
          saved = d0 - distance(newPos) - taxi if saved >= min
        ) yield saved
      }

      // Flatten the results into a count of how many shortcuts can save how much time.
      path.flatMap(cheatsFrom).groupMapReduce(identity)(_ => 1)(_ + _).toSeq
    }
  }

  import Solution._

  describe("Example case") {

    val input =
      """###############
        |#...#...#.....#
        |#.#.#.#.#.###.#
        |#S#...#.#.#...#
        |#######.#.#.###
        |#######.#.#...#
        |#######.#.###.#
        |###..E#...#...#
        |###.#######.###
        |#...###...#...#
        |#.#####.#.###.#
        |#.#...#.#.#...#
        |#.#.#.#.#.#.###
        |#...#...#...###
        |###############
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      val cheats = solve(3, 1, input: _*)
      cheats.sorted should contain theSameElementsAs
        Seq((2, 14), (4, 14), (6, 2), (8, 4), (10, 2), (12, 3), (20, 1), (36, 1), (38, 1), (40, 1), (64, 1))
    }

    it("should match the puzzle description for part 2") {
      val cheats = solve(21, 1, input: _*)
      cheats.sorted should contain allOf
        ((50, 32), (52, 31), (54, 29), (56, 39), (58, 25), (60, 23), (62, 20), (64, 19), (66, 12), (68, 14), (70, 12),
        (72, 22), (74, 4), (76, 3))
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day20Input.txt")
    lazy val answer1 = decryptLong("oyB8B22n0/IeBp9K/tBX3w==")
    lazy val answer2 = decryptLong("k5zhUo4QM+6+DZw0yo4ADw==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2 (3s)", Slow) {
      part2(input: _*) shouldBe answer2
    }
  }
}
