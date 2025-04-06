package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 10 Solutions in scala=
  *
  * Input: A topological map for hiking, to find hiking trails from '0' to '9' that where a path can only increment by 0
  * or 1.
  *
  * Part 1: From any '0', count how many '9's are reachable and return the sum.
  *
  * Part 2: From any '0', count how many different ways to reach a '9' and return the sum.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/10]]
  */
class AdventOfCodeDay10Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** Inspired from [[com.skraba.byexample.scala.hack.advent2024.AdventOfCodeDay6Spec.Solution.Plan]] */
    case class Plan(full: String, dx: Int) {

      /** The height of the full plan */
      lazy val dy: Int = full.length / dx

      /** If there is a border, this is the character it uses. */
      lazy val border: Char = full.head

      lazy val display: String = full.grouped(dx).mkString("\n")

      /** Finds all instances of the character in the plan. */
      def find(c: Char): Iterator[Cursor] = new Iterator[Cursor] {
        var pos: Int = full.indexOf(c)
        def hasNext: Boolean = pos >= 0
        def next(): Cursor = { val res = Cursor(pos); pos = full.indexOf(c, pos + 1); res }
      }

      /** A cursor moving through the plan. */
      case class Cursor(pos: Int) {

        /** The character at the cursor. */
        lazy val at: Char = full(pos)

        // Moves the cursor in the specified direction
        lazy val e: Cursor = Cursor(pos + 1)
        lazy val s: Cursor = Cursor(pos + dx)
        lazy val w: Cursor = Cursor(pos - 1)
        lazy val n: Cursor = Cursor(pos - dx)

        // All the neighbours
        lazy val neighbours: Seq[Cursor] = Seq(e, s, w, n)
      }

      object Cursor {
        def apply(x: Int, y: Int): Cursor = Cursor((y + 1) * dx + x + 1)
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

      // The memo contains all the characters of the same value that are reachable from a '9' and the set of '9's that
      // they can reach.
      case class Memo(canReach: Map[plan.Cursor, Set[plan.Cursor]]) {
        lazy val at: Char = canReach.head._1.at
        lazy val prevAt: Char = (at - 1).toChar
      }

      // For each reachable character, work backwards to find the previous character.
      val ll = LazyList.iterate(Memo(plan.find('9').map(c => c -> Set(c)).toMap)) { memo =>
        val prevReach: Set[(plan.Cursor, Set[plan.Cursor])] =
          for (c <- memo.canReach.keySet; neighbour <- c.neighbours if neighbour.at == memo.prevAt)
            yield neighbour -> memo.canReach(c)
        Memo(prevReach.groupMap(_._1)(_._2).view.mapValues(_.flatten.toSet).toMap)
      }

      val acc = ll.dropWhile(_.at != '0').head
      acc.canReach.map(_._2.size).sum
    }

    def part2(in: String*): Long = {
      val plan = Plan(in)

      // The memo contains all the characters of the same value that are reachable from a '9' and the number of
      // paths to reachable 9's.
      case class Memo(canReach: Map[plan.Cursor, Long]) {
        lazy val at: Char = canReach.head._1.at
        lazy val prevAt: Char = (at - 1).toChar
      }

      // For each reachable character, find the precedent.
      val ll = LazyList.iterate(Memo(plan.find('9').map(c => c -> 1L).toMap)) { memo =>
        val prevReach: Seq[(plan.Cursor, Long)] =
          for (c <- memo.canReach.keys.toSeq; neighbour <- c.neighbours if neighbour.at == memo.prevAt)
            yield neighbour -> memo.canReach(c)
        Memo(prevReach.groupMap(_._1)(_._2).view.mapValues(_.sum).toMap)
      }

      // Get the values at zero
      ll.dropWhile(_.at != '0').head.canReach.values.sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """89010123
        |78121874
        |87430965
        |96549874
        |45678903
        |32019012
        |01329801
        |10456732
        |""".trimSplit

    val input2 =
      """012345
        |123456
        |234567
        |345678
        |4.6789
        |56789.
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 36
    }

    it("should match the puzzle description for part 2") {
      part2(input2: _*) shouldBe 227
      part2(input: _*) shouldBe 81
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day10Input.txt")
    lazy val answer1 = decryptLong("LO2Rvnwl1GBfbm8Sl3u++g==")
    lazy val answer2 = decryptLong("XAmNXQzo0X6etD7hABMYqQ==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
