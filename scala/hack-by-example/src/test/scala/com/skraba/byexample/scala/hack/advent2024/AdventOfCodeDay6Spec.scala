package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** =Advent of Code 2024 Day 6 Solutions in scala=
  *
  * Input: A 2D map with obstacles marked by ```#``` and the starting position of the guard marked by ```^```. The guard
  * moves in a straight line, and turns right when there is an obstacle.
  *
  * Part 1: How many squares are visited by the guard before they leave the map?
  *
  * Part 2: In how many empty positions can you add a single obstacle to prevent the guard from leaving the map?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/6]]
  */
class AdventOfCodeDay6Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }
    import Dir._

    /** Inspired from [[com.skraba.byexample.scala.hack.advent2023.AdventOfCodeDay16Spec.Solution.Plan]] */
    case class Plan(full: String, dx: Int) {

      /** The height of the full plan */
      lazy val dy: Int = full.length / dx

      /** If there is a border, this is the character it uses. */
      lazy val border: Char = full.head

      lazy val display: String = full.grouped(dx).mkString("\n")

      /** A cursor moving through the plan. */
      case class Cursor(pos: Int, dir: Dir) {

        /** The character at the cursor. */
        lazy val at: Char = full(pos)

        // Moves the cursor in the specified direction
        lazy val e: Cursor = Cursor(pos + 1, East)
        lazy val s: Cursor = Cursor(pos + dx, South)
        lazy val w: Cursor = Cursor(pos - 1, West)
        lazy val n: Cursor = Cursor(pos - dx, North)
        lazy val straight: Cursor = mv(dir)

        // Rotates the cursor clockwise or counter-clockwise
        lazy val rotCw: Cursor = copy(dir = Dir((dir.id + 1) % Dir.maxId))
        lazy val rotCcw: Cursor = copy(dir = Dir((dir.id + Dir.maxId - 1) % Dir.maxId))

        def mv(to: Dir): Cursor = to match {
          case East  => e
          case South => s
          case West  => w
          case North => n
        }
      }

      object Cursor {
        def apply(x: Int, y: Int, dir: Dir): Cursor = Cursor((y + 1) * dx + x + 1, dir)
      }

      /** Traces the movements of the guard through the plan. The first set is the current position of the guard or
        * empty if they've left, and the second set is the positions they've already occupied.
        */
      case class Guard(current: Set[Cursor], visited: Set[Cursor] = Set.empty) {
        def nextMove(in: Cursor): Set[Cursor] = in.straight.at match {
          case c if c == border => Set.empty
          case '#'              => Set(in.rotCw)
          case _                => Set(in.straight)
        }

        // Find the next position of the guard, and add the current position to the visited set.
        lazy val next: Guard = Guard(current.flatMap(nextMove), visited ++ current)

        def exited(): Boolean = current.isEmpty
        def inLoop(): Boolean = current.exists(visited.apply)
      }
    }

    object Plan {
      def apply(in: Seq[String], border: Char = '+'): Plan = {
        val dx: Int = in.headOption.map(_.length).getOrElse(0) + 2
        Plan(full = in.mkString(border.toString * (dx + 1), border.toString * 2, border.toString * (dx + 1)), dx)
      }
    }

    def part1(in: String*): Long = {
      val plan = Plan(in)
      val moves = LazyList.iterate(plan.Guard(Set(plan.Cursor(plan.full.indexOf('^'), North)))) { acc => acc.next }
      moves.dropWhile(!_.exited()).head.visited.map(_.pos).size
    }

    def part2(in: String*): Long = {
      val plan = Plan(in)
      val start = plan.full.indexOf('^')

      // Find the original path of the guard, so we can place a new obstacle in it.
      val originalPath = {
        val moves = LazyList.iterate(plan.Guard(Set(plan.Cursor(start, North)))) { acc => acc.next }
        moves.dropWhile(!_.exited()).head.visited.map(_.pos)
      }

      // All the finished possible paths for the guard with the new obstacle.
      val finished = for (i <- originalPath if plan.full(i) == '.') yield {
        val blocked = plan.copy(full = plan.full.updated(i, '#'))
        val moves = LazyList.iterate(blocked.Guard(Set(blocked.Cursor(start, North)))) { acc => acc.next }
        moves.dropWhile(g => !g.exited() && !g.inLoop()).head
      }

      finished.count(_.inLoop())
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """....#.....
        |.........#
        |..........
        |..#.......
        |.......#..
        |..........
        |.#..^.....
        |........#.
        |#.........
        |......#...
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 41
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 6
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day6Input.txt")
    lazy val answer1 = decryptLong("5ieGhY2c9+I52yafnQ6e7w==")
    lazy val answer2 = decryptLong("U7fcYy8uDq9kgv9FACRnoQ==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2 (6 seconds)", Slow) {
      part2(input: _*) shouldBe answer2
    }
  }
}
