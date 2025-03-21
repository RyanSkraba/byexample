package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2024 Day 16 Solutions in scala=
  *
  * Input: A maze to be followed, where turning costs 1000 and going straight costs 1. Start facing east at point S and
  * find the point E.
  *
  * Part 1: Find the minimum cost to reach the end.
  *
  * Part 2: Find the number of grid positions that are on ANY minimum path.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/16]]
  */
class AdventOfCodeDay16Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }
    import Dir._

    /** Inspired from [[com.skraba.byexample.scala.hack.advent2024.AdventOfCodeDay6Spec.Solution.Plan]] */
    case class Plan(full: String, dx: Int) {

      /** The height of the full plan */
      lazy val dy: Int = full.length / dx

      /** If there is a border, this is the character it uses. */
      lazy val border: Char = full.head

      lazy val display: String = full.grouped(dx).mkString("\n")

      lazy val start: Cursor = Cursor(find('S').next(), East)

      /** Finds all instances of the character in the plan. */
      def find(c: Char): Iterator[Int] = new Iterator[Int] {
        var pos: Int = full.indexOf(c)

        def hasNext: Boolean = pos >= 0

        def next(): Int = {
          val res = pos;
          pos = full.indexOf(c, pos + 1);
          res
        }
      }

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

        def next: Set[Cursor] = Set(straight, rotCw, rotCcw).filterNot(_.at == '#')
      }

      object Cursor {
        def apply(x: Int, y: Int, dir: Dir): Cursor = Cursor((y + 1) * dx + x + 1, dir)
      }

      def bfsIterativePart1(): Long = {
        var min = Long.MaxValue
        // The memo contains the minimum cost to reach the given position (so far).
        val memoCost: mutable.Map[Cursor, Long] = mutable.Map.empty
        // The queue contains the current path, the head position on the path and the current cost.
        val queue = mutable.Queue(Set(start) -> (start, 0L))

        while (queue.nonEmpty) {
          val (path, (current, cost)) = queue.dequeue()
          if (cost < min) {
            if (current.at == 'E') {
              // We've found a solution, update the minimum cost
              min = min min cost
            } else if (cost < memoCost.getOrElse(current, Long.MaxValue)) {
              // It's not a solution, but we've either never been here before or it's lower than the previous visit
              memoCost(current) = cost
              val next = current.next.filterNot(path)
              // Add all the next possible steps to the queue.  They'll be trimmed if they're more costly.
              queue.enqueueAll(next.map(c => path + c -> (c -> (cost + (if (c.dir == current.dir) 1 else 1000)))))
            }
          }
        }
        min
      }

      def bfsIterativePart2(): Int = {
        var min = Long.MaxValue
        val memoCost: mutable.Map[Cursor, Long] = mutable.Map.empty
        val onPath = mutable.Map[Long, Set[Int]]()
        val queue = mutable.Queue(Set(start) -> (start, 0L))
        while (queue.nonEmpty) {
          val (path, (current, cost)) = queue.dequeue()
          if (cost <= min) {
            if (current.at == 'E') {
              // We've found a solution, update the minimum cost and the positions on the path with that cost.
              min = cost
              onPath(cost) = onPath.getOrElse(cost, Set()) ++ path.map(_.pos)
            } else if (cost <= memoCost.getOrElse(current, Long.MaxValue)) {
              // It's not a solution, but we've either never been here before or it's lower OR EQUAL TO the previous
              // visit.
              memoCost(current) = cost
              val next = current.next.filterNot(path)
              queue.enqueueAll(next.map(c => path + c -> (c -> (cost + (if (c.dir == current.dir) 1 else 1000)))))
            }
          }
        }
        onPath(min).size
      }
    }

    object Plan {
      def apply(in: Seq[String], border: Char = '+'): Plan = {
        val dx: Int = in.headOption.map(_.length).getOrElse(0) + 2
        Plan(full = in.mkString(border.toString * (dx + 1), border.toString * 2, border.toString * (dx + 1)), dx)
      }
    }

    def part1(in: String*): Long = Plan(in).bfsIterativePart1()
    def part2(in: String*): Long = Plan(in).bfsIterativePart2()
  }

  import Solution._

  describe("Example case") {
    val input =
      """###############
        |#.......#....E#
        |#.#.###.#.###.#
        |#.....#.#...#.#
        |#.###.#####.#.#
        |#.#.#.......#.#
        |#.#.#####.###.#
        |#...........#.#
        |###.#.#####.#.#
        |#...#.....#.#.#
        |#.#.#.###.#.#.#
        |#.....#...#.#.#
        |#.###.#.#.#.#.#
        |#S..#.....#...#
        |###############
        |""".trim.stripMargin.split("\n")

    val input2 =
      """#################
        |#...#...#...#..E#
        |#.#.#.#.#.#.#.#.#
        |#.#.#.#...#...#.#
        |#.#.#.#.###.#.#.#
        |#...#.#.#.....#.#
        |#.#.#.#.#.#####.#
        |#.#...#.#.#.....#
        |#.#.#####.#.###.#
        |#.#.#.......#...#
        |#.#.###.#####.###
        |#.#.#...#.....#.#
        |#.#.#.#####.###.#
        |#.#.#.........#.#
        |#.#.#.#########.#
        |#S#.............#
        |#################
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 7036
    }

    it("should match the puzzle description for part 1 example 2") {
      part1(input2: _*) shouldBe 11048
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 45
    }

    it("should match the puzzle description for part 2 example 2") {
      part2(input2: _*) shouldBe 64
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day16Input.txt")
    lazy val answer1 = decryptLong("+yERfwh/vb+tisro/IR0IA==")
    lazy val answer2 = decryptLong("h9vaCeCFbuzh1MelSH9EMA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
