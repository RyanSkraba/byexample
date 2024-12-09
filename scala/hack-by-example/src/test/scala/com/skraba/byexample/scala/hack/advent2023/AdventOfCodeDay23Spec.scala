package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.collection.mutable

/** =Advent of Code 2023 Day 23 Solutions in scala=
  *
  * Input: A 2D map with forests (`#`) and paths (`.`) as well as some contraints that where the path can only be taken
  * in one direction (`^` `v` * `<` `>`).
  *
  * Part 1: Find the longest path that goes from the upper row to the bottom row that doesn't visit the same spot twice,
  * obeying the directions for the one-way paths
  *
  * Part 2: Find the longest path that goes from the upper row to the bottom row ignoring the one-way constraints.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/23]]
  */
class AdventOfCodeDay23Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** Reusable North, South, East, West */
    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }

    case class Plan(input: Seq[String], border: String = "#") {

      /** The width of the full plan */
      lazy val dx: Int = input.headOption.map(_.length).getOrElse(0) + 2

      /** The full plan as a single string, including a full border. */
      lazy val full: String =
        input.mkString(border * (dx + 1), border * 2, border * (dx + 1))

      /** The start and end positions */
      lazy val (start, end) =
        (Cursor(full.indexOf('.')), Cursor(full.lastIndexOf('.')))

      override def toString: String = full.grouped(dx).mkString("\n")

      def withPath(cs: Set[Cursor]): String = {
        full.indices
          .map(Cursor.apply)
          .map(c => if (cs(c)) 'O' else c.at)
          .grouped(dx)
          .map(_.mkString)
          .mkString("\n")
      }

      def dfsRecursive(
          path: Set[Cursor] = Set(start),
          pos: Cursor = start,
          slippery: Boolean = true
      ): Int = {
        if (pos == end)
          path.size
        else
          (if (slippery) pos.nextSlippery else pos.next)
            .filterNot(path)
            .map(p => dfsRecursive(path + p, p, slippery))
            .maxOption
            .getOrElse(Int.MinValue)
      }

      def dfsIterative(
          pos: Cursor = start,
          slippery: Boolean = true
      ): Int = {
        var max = Int.MinValue
        val queue = mutable.Queue(Set(pos) -> pos)
        while (queue.nonEmpty) {
          val (path, current) = queue.dequeue()
          if (current == end)
            max = max max path.size
          val next =
            (if (slippery) current.nextSlippery else current.next)
              .filterNot(path)
          queue.addAll(next.map(c => path + c -> c))
        }
        max
      }

      def bfsIterative(
          pos: Cursor = start,
          slippery: Boolean = true
      ): Int = {
        var max = Int.MinValue
        val queue = mutable.Queue(Set(pos) -> pos)
        while (queue.nonEmpty) {
          val (path, current) = queue.dequeue()
          if (current == end)
            max = max max path.size
          val next =
            (if (slippery) current.nextSlippery else current.next)
              .filterNot(path)
          queue.enqueueAll(next.map(c => path + c -> c))
        }
        max
      }

      def simplify(slippery: Boolean = true): Set[(Cursor, Cursor, Int)] = {

        // each of these points are "nodes" in the simplified graph
        val forks = full.indices
          .map(Cursor.apply)
          .filterNot(_.at == '#')
          .filter(_.next.size > 2)
          .toSet + start + end

        // from any point to the next forks, including the distance.
        def dfsNextBranch(
            src: Cursor = start,
            distance: Int = 0,
            path: Option[(Cursor, Cursor)] = None,
            slippery: Boolean = true
        ): Set[(Cursor, Cursor, Int)] = {
          val (last, current) = path.getOrElse(src -> src)
          if (current != src && forks.contains(current))
            Set((src, current, distance))
          else
            (if (slippery) current.nextSlippery else current.next)
              .filterNot(_ == last)
              .flatMap(p =>
                dfsNextBranch(
                  src,
                  distance + 1,
                  Some(current -> p),
                  slippery
                )
              )
        }

        // the simplified graph edges, from points where there are options
        forks.flatMap(c => dfsNextBranch(src = c, slippery = false))
      }

      def bfsSimplifiedIterative(
          pos: Cursor = start,
          slippery: Boolean = true
      ): Int = {

        val graph = simplify(slippery)

        var max = Int.MinValue
        val queue = mutable.Queue(Set(pos) -> pos -> 0)
        while (queue.nonEmpty) {
          val ((path, current), distance) = queue.dequeue()
          if (current == end)
            max = max max distance
          val next =
            graph.filter(_._1 == current).filterNot(x => path.contains(x._2))
          queue.enqueueAll(
            next.map(x => path + x._2 -> x._2 -> (distance + x._3))
          )
        }
        max
      }

      /** A cursor moving through the plan. */
      case class Cursor(pos: Int) {
        lazy val e: Cursor = Cursor(pos + 1)
        lazy val s: Cursor = Cursor(pos + dx)
        lazy val w: Cursor = Cursor(pos - 1)
        lazy val n: Cursor = Cursor(pos - dx)
        lazy val at: Char = full(pos)

        /** The next steps if the slippery slopes are one way. */
        def nextSlippery: Set[Cursor] = (at match {
          case '>' => Set(e)
          case 'v' => Set(s)
          case '<' => Set(w)
          case '^' => Set(n)
          case '.' => Set(e, s, w, n)
        }).filterNot(_.at == '#')

        def next: Set[Cursor] = Set(e, s, w, n).filterNot(_.at == '#')
      }
    }

    def part1(in: String*): Long = {
      val plan = Plan(in)
      plan.bfsIterative() - 1
    }

    def part2(in: String*): Long = {
      val plan = Plan(in)
      plan.bfsSimplifiedIterative(slippery = false)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """#.#####################
        |#.......#########...###
        |#######.#########.#.###
        |###.....#.>.>.###.#.###
        |###v#####.#v#.###.#.###
        |###.>...#.#.#.....#...#
        |###v###.#.#.#########.#
        |###...#.#.#.......#...#
        |#####.#.#.#######.#.###
        |#.....#.#.#.......#...#
        |#.#####.#.#.#########v#
        |#.#...#...#...###...>.#
        |#.#.#v#######v###.###v#
        |#...#.>.#...>.>.#.###.#
        |#####v#.#.###v#.#.###.#
        |#.....#...#...#.#.#...#
        |#.#########.###.#.#.###
        |#...###...#...#...#.###
        |###.###.#.###v#####v###
        |#...#...#.#.>.>.#.>.###
        |#.###.###.#.###.#.#v###
        |#.....###...###...#...#
        |#####################.#
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1 (DFS recursive)") {
      val plan = Plan(input)
      plan.dfsRecursive() shouldBe 95
    }

    it("should match the puzzle description for part 1 (DFS iterative)") {
      val plan = Plan(input)
      plan.dfsIterative() shouldBe 95
    }

    it("should match the puzzle description for part 1 (BFS iterative)") {
      val plan = Plan(input)
      plan.bfsIterative() shouldBe 95
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 94
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 154
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day23Input.txt")
    lazy val answer1 = decryptLong("1YUyld8OEDiUZDewrAkKqQ==")
    lazy val answer2 = decryptLong("gsHJVAFg7ZdyYFgkWbO8bw==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2 (80 seconds)", Slow) {
      part2(input: _*) shouldBe answer2
    }
  }
}
