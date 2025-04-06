package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2023 Day 14 Solutions in scala=
  *
  * Input: A map of a platform of boulders, where # are unmovable boulders and O are moveable boulders.
  *
  * Part 1: Roll each of the moveable boulders as north as possible, until it's blocked by an unmoveable border, the map
  * edge or another blocked boulder. Then calculate the "load" compared to the north: every moveable boulder counts its
  * distance from the south edge (where the southernmost boulders count 1).
  *
  * Part 2: A cycle shifts all boulders North, then West, then South, then East. Calculate the load after 1 billion
  * cycles.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/14]]
  */
class AdventOfCodeDay14Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def rotateCcw(in: Seq[String]): Seq[String] =
      in.map(_.reverse).transpose.map(_.mkString)

    def rotateCw(in: Seq[String]): Seq[String] =
      in.transpose.map(_.reverse).map(_.mkString)

    def shiftLeft(in: Seq[String]): Seq[String] =
      in.map(row =>
        row
          .split('#')
          .map(x => ("O" * x.count(_ == 'O')).padTo(x.length, '.'))
          .mkString("#")
          .padTo(row.length, '#')
      )

    def calcNLeft(in: Seq[String]): Long =
      in.flatMap(_.reverse.zipWithIndex).filter(_._1 == 'O').map(_._2 + 1L).sum

    def cycle(in: Seq[String]): Seq[String] = {
      LazyList
        .iterate(in) { plan => rotateCw(shiftLeft(plan)) }
        .drop(4)
        .head
    }

    def part1(in: String*): Long = {
      // Consider north to be on the left for easier manipulation
      val nLeft = rotateCcw(in)
      calcNLeft(shiftLeft(nLeft))
    }

    def part2(cycles: Long, in: String*): Long = {
      // Consider north to be on the left for easier manipulation
      val nLeft = rotateCcw(in)

      // Iterate until we find a state we've already seen
      val cache = mutable.Set[Seq[String]]()
      val repeat = LazyList.iterate(nLeft)(cycle).dropWhile(cache.add)

      // The length of the cycle is found by counting until the head is found again
      val repeat1 = repeat.head
      val repeatLen = 1 + repeat.tail.takeWhile(_ != repeat1).size
      // And the cache includes the first cycle, so repeat0 is the number of element before the cycle starts
      val repeat0 = cache.size - repeatLen

      // After skipping the number of repeated cycles, we still need to iterate a few times
      val remaining = (cycles - repeat0) % repeatLen

      // Find a cycle
      val end = repeat.drop(remaining.toInt).head
      calcNLeft(end)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """O....#....
        |O.OO#....#
        |.....##...
        |OO.#O....O
        |.O.....O#.
        |O.#..O.#.#
        |..O..#O..O
        |.......O..
        |#....###..
        |#OO..#....
        |""".trimSplit

    it("should manipulate the map") {
      val nLeft = rotateCcw(input)
      nLeft.mkString("\n") shouldBe
        """.#.O.#O...
          |....#.....
          |....O#.O#.
          |..#...O.#.
          |#.#..O#.##
          |.#.O......
          |.O.#......
          |.O...#O..O
          |...OO....O
          |OO.O.O..##""".stripMargin
      val shifted = shiftLeft(nLeft)
      shifted.mkString("\n") shouldBe
        """.#O..#O...
          |....#.....
          |O....#O.#.
          |..#O....#.
          |#.#O..#.##
          |.#O.......
          |O..#......
          |O....#OO..
          |OOO.......
          |OOOO....##""".stripMargin
      rotateCw(shifted).mkString("\n") shouldBe
        """OOOO.#.O..
          |OO..#....#
          |OO..O##..O
          |O..#.OO...
          |........#.
          |..#....#.#
          |..O..#.O.O
          |..O.......
          |#....###..
          |#....#....""".stripMargin
    }

    it("should cycle correctly") {
      val cycle1 = cycle(rotateCcw(input))
      rotateCw(cycle1).mkString("\n") shouldBe
        """.....#....
          |....#...O#
          |...OO##...
          |.OO#......
          |.....OOO#.
          |.O#...O#.#
          |....O#....
          |......OOOO
          |#...O###..
          |#..OO#....""".stripMargin

      val cycle2 = cycle(cycle1)
      rotateCw(cycle2).mkString("\n") shouldBe
        """.....#....
          |....#...O#
          |.....##...
          |..O#......
          |.....OOO#.
          |.O#...O#.#
          |....O#...O
          |.......OOO
          |#..OO###..
          |#.OOO#...O""".stripMargin

      val cycle3 = cycle(cycle2)
      rotateCw(cycle3).mkString("\n") shouldBe
        """.....#....
          |....#...O#
          |.....##...
          |..O#......
          |.....OOO#.
          |.O#...O#.#
          |....O#...O
          |.......OOO
          |#...O###.O
          |#.OOO#...O""".stripMargin
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 136
    }

    it("should match the puzzle description for part 2") {
      part2(1000000000, input: _*) shouldBe 64
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day14Input.txt")
    lazy val answer1 = decryptLong("V4O7+jFd+JhKSl+ytMD+YA==")
    lazy val answer2 = decryptLong("4OEmRUprz/ebqgxUdl2DoQ==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(1000000000, input: _*) shouldBe answer2
    }
  }
}
