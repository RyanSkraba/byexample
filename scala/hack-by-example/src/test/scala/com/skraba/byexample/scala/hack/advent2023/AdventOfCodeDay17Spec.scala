package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2023 Day 17 Solutions in scala=
  *
  * Input: A map of costs, where each square is the cost of *entering* that
  * square.
  *
  * Part 1: Given that you can't move three squares in the same direction
  * consecutively, what is the minimum cost of getting from the upper right to
  * the lower left
  *
  * Part 2: As above, but you must move at least 4 and at most 10 times in the
  * same direction.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/17]]
  */
class AdventOfCodeDay17Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    object Dir extends Enumeration {
      type Dir = Value
      val Horiz, Vert = Value
    }
    import Dir._

    case class Plan(full: String, w: Int, h: Int) {

      lazy val display: String = full.grouped(w).mkString("\n")

      def part1(min: Int, max: Int): Int = {
        // The starting positions
        val bfs = mutable.Queue(Cursor(0, Horiz), Cursor(0, Vert))

        // The distances that have been calculated from the start, when arriving there horizontally / vertically
        val memo: mutable.Map[Cursor, Int] = mutable.Map()
        bfs.foreach(memo(_) = 0)

        while (bfs.nonEmpty) {
          val current = bfs.dequeue()

          val next = if (current.dir == Horiz) {
            // If we've arrived there horizontally, we can go at most three vertically
            val up = current.up(max)
            val down = current.down(max)
            up.zip(up.scanLeft(memo(current))(_ + _.at).tail).drop(min - 1) ++
              down
                .zip(down.scanLeft(memo(current))(_ + _.at).tail)
                .drop(min - 1)
          } else {
            val left = current.left(max)
            val right = current.right(max)
            left
              .zip(left.scanLeft(memo(current))(_ + _.at).tail)
              .drop(min - 1) ++
              right
                .zip(right.scanLeft(memo(current))(_ + _.at).tail)
                .drop(min - 1)
          }

          next
            .filterNot(step => memo.getOrElse(step._1, Int.MaxValue) <= step._2)
            .foreach { step =>
              memo(step._1) = step._2
              bfs.enqueue(step._1)
            }
        }

        math.min(
          memo.getOrElse(Cursor(full.length - 1, Horiz), Int.MaxValue),
          memo.getOrElse(Cursor(full.length - 1, Vert), Int.MaxValue)
        )
      }

      case class Cursor(pos: Int, dir: Dir) {
        lazy val at: Int = full(pos) - '0'

        def up(dMax: Int): Seq[Cursor] =
          (1 to dMax)
            .map(pos - _ * w)
            .takeWhile(_ >= 0)
            .map(Cursor(_, Vert))

        def down(dMax: Int): Seq[Cursor] = (1 to dMax)
          .map(pos + _ * w)
          .takeWhile(_ < full.length)
          .map(Cursor(_, Vert))

        def left(dMax: Int): Seq[Cursor] =
          (1 to dMax)
            .map(pos - _)
            .takeWhile(_ > 0)
            .takeWhile(_ / w == pos / w)
            .map(Cursor(_, Horiz))

        def right(dMax: Int): Seq[Cursor] = (1 to dMax)
          .map(pos + _)
          .takeWhile(_ / w == pos / w)
          .map(Cursor(_, Horiz))
      }
    }

    object Plan {
      def from(in: Seq[String]): Plan =
        Plan(in.mkString, in.headOption.map(_.length).getOrElse(0), in.length)

    }

    def part1(in: String*): Long = Plan.from(in).part1(1, 3)

    def part2(in: String*): Long = Plan.from(in).part1(4, 10)
  }

  import Solution.Dir._
  import Solution._

  describe("Example case") {
    val input =
      """2413432311323
        |3215453535623
        |3255245654254
        |3446585845452
        |4546657867536
        |1438598798454
        |4457876987766
        |3637877979653
        |4654967986887
        |4564679986453
        |1224686865563
        |2546548887735
        |4322674655533
        |""".trim.stripMargin.split("\n")

    val unfortunate =
      """111111111111
        |999999999991
        |999999999991
        |999999999991
        |999999999991
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {

      val p = Plan.from(input)
      p.Cursor(0, Horiz).up(3) shouldBe Seq()
      p.Cursor(0, Horiz).down(3) shouldBe Seq(
        p.Cursor(13, Vert),
        p.Cursor(26, Vert),
        p.Cursor(39, Vert)
      )
      p.Cursor(0, Horiz).left(3) shouldBe Seq()
      p.Cursor(0, Horiz).right(3) shouldBe Seq(
        p.Cursor(1, Horiz),
        p.Cursor(2, Horiz),
        p.Cursor(3, Horiz)
      )
      p.Cursor(p.w + 1, Horiz).up(3) shouldBe Seq(p.Cursor(1, Vert))
      p.Cursor(p.w + 1, Horiz).down(3) shouldBe Seq(
        p.Cursor(27, Vert),
        p.Cursor(40, Vert),
        p.Cursor(53, Vert)
      )
      p.Cursor(p.w + 1, Horiz).left(3) shouldBe Seq(p.Cursor(p.w, Horiz))
      p.Cursor(p.w + 1, Horiz).right(3) shouldBe Seq(
        p.Cursor(p.w + 2, Horiz),
        p.Cursor(p.w + 3, Horiz),
        p.Cursor(p.w + 4, Horiz)
      )
      p.Cursor(p.full.length - 1, Horiz).up(3) shouldBe Seq(
        p.Cursor(p.full.length - p.w - 1, Vert),
        p.Cursor(p.full.length - 2 * p.w - 1, Vert),
        p.Cursor(p.full.length - 3 * p.w - 1, Vert)
      )
      p.Cursor(p.full.length - 1, Horiz).down(3) shouldBe Seq()
      p.Cursor(p.full.length - 1, Horiz).left(3) shouldBe Seq(
        p.Cursor(p.full.length - 2, Horiz),
        p.Cursor(p.full.length - 3, Horiz),
        p.Cursor(p.full.length - 4, Horiz)
      )
      p.Cursor(p.full.length - 1, Horiz).right(3) shouldBe Seq()

      part1(input: _*) shouldBe 102
    }

    it("should match the unfortunate puzzle description for part 2") {
      part2(unfortunate: _*) shouldBe 71
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 94
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day17Input.txt")
    lazy val answer1 = decryptLong("Apxl9Hw6a2h7Fjzfrp9qGA==")
    lazy val answer2 = decryptLong("pMaTQT667W+LKp5mvd+09A==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
