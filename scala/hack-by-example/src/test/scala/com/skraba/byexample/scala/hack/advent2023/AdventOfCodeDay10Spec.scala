package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 10 Solutions in scala=
  *
  * Input: A map containing a maze of pipes, with 90 degree bends where - and |
  * are horizontal and vertical characters, while F 7 L and J are bends. There
  * is one big loop starting at a position marked S.
  *
  * Part 1: Starting at S and moving in both directions in the loop, find the
  * steps to the farthest point in the loop (counting distance along the loop).
  *
  * Part 2: Find all of the points inside the loop.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/10]]
  */
class AdventOfCodeDay10Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    def findMeetingPaths(input: String*): List[List[Int]] = {
      // The width of the plan including a border of periods
      val dx = input.headOption.map(_.length).getOrElse(0) + 2
      // The full plan as a single string, including a full border of periods
      val full = input.mkString("." * (dx + 1), "..", "." * (dx + 1))

      // The starting position and the next two positions
      val start = full.indexOf("S")
      val next = move(start, dx, full)

      // Iterate and collect the paths through the pipes
      val paths = LazyList.iterate(next.map(List(_, start))) { acc =>
        acc.map(path => {
          val headPos = path.head
          val prevPos = path(1)
          move(headPos, dx, full).filter(_ != prevPos) ++ path
        })
      }

      // Stop iterating when the paths finally meet
      paths.dropWhile { xs => xs.head.head != xs(1).head }.head
    }

    /** Given the starting position in the maze, find the next two possible
      * positions.
      * @param in
      *   The position we are currently at.
      * @param dx
      *   The width of the maze, used to separate the string into a 2D array.
      * @param maze
      *   The string describing the maze.
      * @return
      *   The next two positions in the maze permitted from that spot.
      */
    def move(in: Int, dx: Int, maze: String): List[Int] = (maze(in) match {
      case '-' => List(-1, +1) // left right
      case '|' => List(-dx, +dx) // down up
      case 'J' => List(-dx, -1)
      case 'F' => List(1, +dx)
      case '7' => List(-1, +dx)
      case 'L' => List(-dx, +1)
      case 'S' =>
        // If the starting position is S, then collect the neighbours that can move there.
        List(-1, +1, dx, -dx).filter(x => move(x + in, dx, maze).contains(in))
      case _ => Nil
    }).map(_ + in)

    def part1(input: String*): Long =
      findMeetingPaths(input: _*).head.length - 1

    def part2(input: String*): Long = {
      // The width of the plan including a border of periods
      val dx = input.headOption.map(_.length).getOrElse(0) + 2
      // The full plan as a single string, including a full border of periods
      val full = input.mkString("." * (dx + 1), "..", "." * (dx + 1))

      // The starting position and the next two positions
      val start = full.indexOf("S")
      val next = move(start, dx, full)

      // The positions in the maze that are actually part of the loop.
      val onPath: Set[Int] = findMeetingPaths(input: _*).flatten.toSet

      // Update the maze by replacing the S with it's actual pipe shape
      val maze = Seq('-', '|', 'L', 'F', 'J')
        .map(full.updated(start, _))
        .filter(move(start, dx, _) == next)

      // And clean the maze by removing all the pipes that aren't on the path
      val clean = maze.head.zipWithIndex
        .map(x => if (onPath(x._2)) x._1 else '.')
        .mkString

      // For each row, we can count the pipes that CROSS the row (starting from the bottom going to the top) to determine whether we are inside or outside.
      clean
        .split("(\\||L-*7|F-*J)")
        .grouped(2)
        .toSeq
        .flatMap(_.tail)
        .map(_.count(_ == '.'))
        .sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """-L|F7
        |7S-7|
        |L|7||
        |-L-J|
        |L|-JF
        |""".trim.stripMargin.split("\n")

    val input2 =
      """7-F7-
        |.FJ|7
        |SJLL7
        ||F--J
        |LJ.LJ
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 4
      part1(input2: _*) shouldBe 8
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 1
      part2(input2: _*) shouldBe 1
    }
  }

  describe("🔑 Solution 🔑") {
    val input = puzzleInput("Day10Input.txt")
    lazy val answer1 = decryptLong("tMh3A4fEoiUgve/H9PqFTQ==")
    lazy val answer2 = decryptLong("NLfxxhehN14Tbgc7kiV/Eg==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
