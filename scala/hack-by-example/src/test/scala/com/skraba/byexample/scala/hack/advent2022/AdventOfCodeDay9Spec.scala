package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 9 Solutions in scala=
  *
  * Input: A set of movements for the head of a rope.
  *
  * Part 1: When dragging around a rope on a board, the next knot follows along
  * so that it is always adjacent or diagonal to the head. Count the number of
  * squares the next knot visits.
  *
  * Part 2: When dragging around a rope on a board, count the number of squares
  * the 10th knot visits.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/9]]
  */
class AdventOfCodeDay9Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Knot(x: Int, y: Int) {
      def pulled(other: Knot): Knot = {
        val d = (x - other.x) * (x - other.x) + (y - other.y) * (y - other.y)
        if (d == 4 || d == 8) Knot((x + other.x) / 2, (y + other.y) / 2)
        else if (d == 5 && Math.abs(x - other.x) == 1)
          Knot(other.x, (y + other.y) / 2)
        else if (d == 5 && Math.abs(y - other.y) == 1)
          Knot((x + other.x) / 2, other.y)
        else this
      }
    }

    def reducePath(in: Seq[Knot]): Seq[Knot] = in.foldLeft(List.empty[Knot]) {
      case (Nil, xy) => List(xy)
      case (acc, xy) => acc :+ acc.last.pulled(xy)
    }

    def headPath(in: String*): Seq[Knot] =
      in.foldLeft(List.empty[(Int, Int)]) { (acc, mv: String) =>
        acc ++ Seq.fill(mv.substring(2).toInt)(
          mv match {
            case _ if mv.startsWith("R") => (1, 0)
            case _ if mv.startsWith("L") => (-1, 0)
            case _ if mv.startsWith("U") => (0, 1)
            case _ if mv.startsWith("D") => (0, -1)
            case _                       => (0, 0)
          }
        )
      }.foldLeft(Seq(Knot(0, 0))) { case (acc, (dx, dy)) =>
        acc :+ Knot(acc.last.x + dx, acc.last.y + dy)
      }

    def part1(in: String*): Long = reducePath(headPath(in: _*)).toSet.size

    def part2(knots: Int, in: String*): Long = (0 until knots - 1)
      .foldLeft(headPath(in: _*)) { case (acc, _) => reducePath(acc) }
      .toSet
      .size
  }

  import Solution._

  describe("Example case") {
    val input =
      """R 4
        |U 4
        |L 3
        |D 1
        |R 4
        |D 1
        |L 5
        |R 2
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should pull ropes in the right direction") {
      Knot(0, 0).pulled(Knot(0, 2)) shouldBe Knot(0, 1)
      Knot(0, 0).pulled(Knot(1, 2)) shouldBe Knot(1, 1)
      Knot(0, 0).pulled(Knot(2, 2)) shouldBe Knot(1, 1)
      Knot(0, 0).pulled(Knot(2, 1)) shouldBe Knot(1, 1)
      Knot(0, 0).pulled(Knot(2, 0)) shouldBe Knot(1, 0)
      Knot(0, 0).pulled(Knot(2, -1)) shouldBe Knot(1, -1)
      Knot(0, 0).pulled(Knot(2, -2)) shouldBe Knot(1, -1)
      Knot(0, 0).pulled(Knot(1, -2)) shouldBe Knot(1, -1)
      Knot(0, 0).pulled(Knot(0, -2)) shouldBe Knot(0, -1)
      Knot(0, 0).pulled(Knot(-1, -2)) shouldBe Knot(-1, -1)
      Knot(0, 0).pulled(Knot(-2, -2)) shouldBe Knot(-1, -1)
      Knot(0, 0).pulled(Knot(-2, -1)) shouldBe Knot(-1, -1)
      Knot(0, 0).pulled(Knot(-2, 0)) shouldBe Knot(-1, 0)
      Knot(0, 0).pulled(Knot(-2, 1)) shouldBe Knot(-1, 1)
      Knot(0, 0).pulled(Knot(-2, 2)) shouldBe Knot(-1, 1)
      Knot(0, 0).pulled(Knot(-1, 2)) shouldBe Knot(-1, 1)
    }

    it("should match the puzzle description") {
      part1(input: _*) shouldBe 13
      part2(1, input: _*) shouldBe 21
      part2(2, input: _*) shouldBe 13
      part2(3, input: _*) shouldBe 7
      part2(4, input: _*) shouldBe 4
      part2(5, input: _*) shouldBe 3
      part2(6, input: _*) shouldBe 2
      part2(7, input: _*) shouldBe 1
      part2(8, input: _*) shouldBe 1
      part2(9, input: _*) shouldBe 1
      part2(10, input: _*) shouldBe 1
    }
  }

  describe("Example2 case") {
    val example =
      """R 5
        |U 8
        |L 8
        |D 3
        |R 17
        |D 10
        |L 25
        |U 20
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description") {
      part1(example: _*) shouldBe 88
      part2(10, example: _*) shouldBe 36

    }
  }

  describe("Solution") {
    val input = puzzleInput("Day9Input.txt").filter(_.nonEmpty)
    ignore("should have answers") {
      part1(input: _*) shouldBe 6464
      part2(10, input: _*) shouldBe 2604
    }
  }
}
