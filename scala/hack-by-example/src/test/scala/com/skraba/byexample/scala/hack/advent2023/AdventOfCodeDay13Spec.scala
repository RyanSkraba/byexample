package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 13 Solutions in scala=
  *
  * Input: A series of rectangular plans of rocks and ashes, using the characters . and #
  *
  * Part 1: For each plan, find a row or column in the pattern that is perfectly reflected in the plan all the way to
  * the edge. If it is a row, return 100 * the count of rows above and including that row. If it is a column, return the
  * count of columns to its left. The answer is the sum.
  *
  * Part 2: Change exactly one square from . to # or vice versa to change the point of reflection and return those
  * values.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/13]]
  */
class AdventOfCodeDay13Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** Given a position and an array size, generates ranges that reflect around the position.
      */
    def reflects(in: Int, size: Int): Iterable[(Int, Int)] = {
      if (in < size / 2)
        (in to 0 by -1).zip(in + 1 until size)
      else
        (in + 1 until size).zip(in to 0 by -1)
    }

    /** Find the count for a single plan, corresponding to the row or column where a reflection could occur.
      *
      * @param wasTransposed
      *   The implementation always searches for the row first, then the column. This is used to determine whether we
      *   are in the column search part (performed by transposing the arrays and calling again).
      * @param omit
      *   Part 2 introduces a smudge that must change the reflection. This might cause more than one symmetry, and this
      *   value is the count for the symmetry we do NOT want to return.
      * @return
      *   the count for the plan
      */
    def part1Count(wasTransposed: Boolean = false, omit: Long = -1)(
        plan: String*
    ): Long = {
      // A quick scan for any rows that are the same as their following row.  There could be a mirror between them.
      val reflect: Seq[Int] = plan
        .sliding(2)
        .zipWithIndex
        .filter(l => l._1.head == l._1(1))
        .map(_._2)
        .toSeq

      // For every possible mirror placement, go out and ensure that the mirror is valid.
      // Remember rows count for 100 (determined by wasTransposed).
      val counts = LazyList
        .from(reflect)
        .filter(reflects(_, plan.size).forall(i => plan(i._1) == plan(i._2)))
        .map(x => (if (wasTransposed) 100 else 1) * (x + 1L))
        .filterNot(_ == omit)

      // If one was found, then return it, otherwise potentially check the transpose of this plan to find any mirrored columns.
      counts.headOption
        .getOrElse {
          if (!wasTransposed) 0L
          else part1Count(omit = omit)(plan.transpose.map(_.mkString): _*)
        }
    }

    def part1(in: String*): Long = {
      in.mkString("\n")
        .split("\n\n")
        .toSeq
        .map(_.split("\n").toSeq)
        .map(part1Count(wasTransposed = true))
        .sum
    }

    def part2(in: String*): Long = {
      in.mkString("\n")
        .split("\n\n")
        .toSeq
        .map(_.split("\n").toSeq)
        .map { plan =>
          // The original value before smudging any of the spots in
          // the mirror
          val unsmudged = part1Count(wasTransposed = true)(plan: _*)
          val alternate = '.' + '#'

          (for (
            r <- plan.indices.to(LazyList);
            c <- plan(r).indices.to(LazyList);
            count = part1Count(wasTransposed = true, omit = unsmudged)(
              plan.updated(
                r,
                plan(r).updated(c, (alternate - plan(r)(c)).toChar)
              ): _*
            )
            if count != 0
          ) yield count).headOption.getOrElse(0L)

        }
        .sum
    }

  }

  import Solution._

  describe("Example case") {
    val input =
      """#.##..##.
        |..#.##.#.
        |##......#
        |##......#
        |..#.##.#.
        |..##..##.
        |#.#.##.#.
        |
        |#...##..#
        |#....#..#
        |..##..###
        |#####.##.
        |#####.##.
        |..##..###
        |#....#..#
        |""".trim.stripMargin.split("\n")

    it("should match horizontal examples") {
      val x =
        """#..
          |.#.
          |..#
          |..#
          |.#.
          |#..
          |""".trim.stripMargin.split("\n")

      val x2 =
        """###
          |.#.
          |..#
          |""".trim.stripMargin.split("\n")

      part1Count()(x ++ x2: _*) shouldBe 3
      part1Count()(x2 ++ x: _*) shouldBe 6
      part1Count()(x ++ x2 ++ x2: _*) shouldBe 3
      part1Count()(x2 ++ x2 ++ x: _*) shouldBe 9

    }

    it("should work on this example with two symmetries") {
      val example = """#.##..####.
                      |..##.#....#
                      |..#.###..##
                      |#.#.#......
                      |##...#....#
                      |###.#..##..
                      |##....#..#.
                      |##.#.######
                      |##....#..#.
                      |#...##.##.#
                      |#.#########
                      |.#.#.......
                      |..##.######
                      |..##.######
                      |.###.......
                      |#.#########
                      |#...##.##.#
                      |""".trim.stripMargin.split("\n")
      part1(example: _*) shouldBe 8
      part2(example: _*) shouldBe 1300
      part1(example.toSeq.transpose.map(_.mkString): _*) shouldBe 800
      part2(example.toSeq.transpose.map(_.mkString): _*) shouldBe 13
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 405
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 400
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day13Input.txt")
    lazy val answer1 = decryptLong("KzpjEuaziIm36ZW14rcUbw==")
    lazy val answer2 = decryptLong("RpA8c0ksXW4fhqSYENI8Zg==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
