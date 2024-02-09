package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2023 Day 18 Solutions in scala=
  *
  * Input: A list of directions that the trench cutter should take on a map, in the form R,L,U,D followed by the number
  * of metres to go in that direction. This will end up as a closed shape.
  *
  * Part 1: Calculate the surface covered by the shape when it is filled in.
  *
  * Part 2: The same problem as part 1, but it turns out that the colour code alongside the directions are the *real*
  * directions with much, much larger values.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/18]]
  */
class AdventOfCodeDay18Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {
    case class Pos(x: Int, y: Int) {
      def move(
          delta: Pos,
          scale: Int = 1
      ): Pos = Pos(x + delta.x * scale, y + delta.y * scale)
    }

    object Pos {
      def display(ps: Set[Pos]): String = {
        val (minX, minY) = (ps.minBy(_.x).x + 1, ps.minBy(_.y).y + 1)
        val (maxX, maxY) = (ps.maxBy(_.x).x + 1, ps.maxBy(_.y).y + 1)
        (0 to maxY)
          .map(y =>
            (0 to maxX)
              .map(x => if (ps(Pos(x, y))) "#" else ".")
              .mkString
          )
          .mkString("\n")
      }
    }

    def parsePart1(in: String*): Seq[(String, Int)] =
      in.map(_.split(" ")).map(mv => (mv.head, mv(1).toInt))

    def floodFillCount(in: Seq[(String, Int)]): Long = {
      // Parse into a list of positions
      val rawPoints = in.foldLeft(Seq[Pos]()) { (acc, mv) =>
        acc ++ {
          val start = acc.lastOption.getOrElse(Pos(0, 0))
          val dd = mv._1 match {
            case "R" => Pos(1, 0)
            case "D" => Pos(0, 1)
            case "L" => Pos(-1, 0)
            case "U" => Pos(0, -1)
          }
          (1 to mv._2).map(start.move(dd, _))
        }
      }

      // Find and adjust the boundaries to align to a minimum of 0, 0
      val (rawX, rawY) = (rawPoints.minBy(_.x).x, rawPoints.minBy(_.y).y)
      val ps = rawPoints.map(p => Pos(p.x - rawX, p.y - rawY)).toSet

      // Use the top left position to find an empty square inside the trenches
      val s0 = ps.filter(_.x == 0).minBy(_.y).move(Pos(1, 1))

      // Flood fill! Starting with the outline and the one empty square.
      val flooded: mutable.Set[Pos] = mutable.Set().addAll(ps)
      val next: mutable.Queue[Pos] = mutable.Queue(s0)

      while (next.nonEmpty) {
        val current: Pos = next.dequeue()
        if (flooded.add(current)) {
          next.enqueueAll(
            Seq(
              current.move(Pos(1, 0)),
              current.move(Pos(-1, 0)),
              current.move(Pos(0, 1)),
              current.move(Pos(0, -1))
            ).filterNot(flooded)
          )
        }
      }

      flooded.size
    }

    case class Line(x1: Long, y1: Long, x2: Long, y2: Long) {
      lazy val vertical: Boolean = y1 != y2
      def extend(dx: Long, dy: Long): Line = Line(x2, y2, x2 + dx, y2 + dy)
    }

    def parsePart2(in: String*): Seq[(String, Int)] = {
      val encodedDir = Map("0" -> "R", "1" -> "D", "2" -> "L", "3" -> "U")
      in
        .map(_.split("[ ()#]+")(2).splitAt(5))
        .map(x => (encodedDir(x._2), Integer.parseInt(x._1, 16)))
    }

    /** Given a two list of longs that indicate the *inclusive* starting and ending indexes for ranges (in subsequent
      * pairs), find the count of the elements in the union of the ranges.
      * @param x1
      *   A list of starting and ending points, in increasing order
      * @param x2
      *   Another list of starting and ending points, in increasing order
      * @return
      *   The number of elements in the ranges of these two lists
      */
    def countInUnion(x1: List[Long], x2: List[Long]): Long = {
      // All of the cases where a disjoint interval exists
      if (x1.isEmpty && x2.isEmpty) 0
      else if (x1.isEmpty) countInUnion(x2, x1)
      else if (x2.isEmpty || x1(1) < x2.head)
        x1(1) - x1.head + 1 + countInUnion(x1.drop(2), x2)
      else if (x1.head > x2.head) countInUnion(x2, x1)

      // At this point there must be an intersection between the two heads
      else if (x1.head < x2.head)
        x2.head - x1.head + countInUnion(x1.updated(0, x2.head), x2)
      else if (x1.head == x2.head && x1(1) == x2(1))
        x1(1) - x1.head + 1 + countInUnion(x1.drop(2), x2.drop(2))
      else if (x1.head == x2.head && x1(1) < x2(1))
        x1(1) - x1.head + 1 + countInUnion(
          x1.drop(2),
          x2.updated(0, x1(1) + 1)
        )
      else countInUnion(x2, x1)
    }

    def scanLineCount(in: Seq[(String, Int)]): Long = {
      // Parse into a list of positions
      val rawRanges = in
        .scanLeft(Line(0, 0, 0, 0)) { (start, mv) =>
          mv._1 match {
            case "R" => start.extend(mv._2, 0)
            case "D" => start.extend(0, mv._2)
            case "L" => start.extend(-mv._2, 0)
            case "U" => start.extend(0, -mv._2)
          }
        }

      val verticalSlices = rawRanges
        .filter(r => r.y1 != r.y2)
        .map {
          case Line(x1, y1, x2, y2) if y1 > y2 => Line(x1, y2, x2, y1)
          case r                               => r
        }

      val byStart: Map[Long, Seq[Line]] = verticalSlices
        .groupBy(_.y1)
        .view
        .mapValues(_.sortBy(_.x1))
        .toMap
      val byEnd: Map[Long, Seq[Line]] = verticalSlices.groupBy(_.y2)

      val rows = (byStart.keySet ++ byEnd.keySet).toSeq.sorted
      case class Acc(
          total: Long = 0,
          lastY: Long = rows.head,
          xs: List[Long] = Nil
      )

      rows
        .foldLeft(Acc()) { (acc, y) =>
          val start = byStart.getOrElse(y, Seq.empty)
          val end = byEnd.getOrElse(y, Seq.empty)
          val count = (y - acc.lastY - 1) * countInUnion(acc.xs, Nil)

          val nextXs = (acc.xs.diff(end.map(_.x1)) ++ start.map(_.x1)).sorted
          val diffCount = countInUnion(nextXs, acc.xs)

          Acc(acc.total + count + diffCount, y, nextXs)
        }
        .total
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """R 6 (#70c710)
        |D 5 (#0dc571)
        |L 2 (#5713f0)
        |D 2 (#d2c081)
        |R 2 (#59c680)
        |D 2 (#411b91)
        |L 5 (#8ceee2)
        |U 2 (#caa173)
        |L 1 (#1b58a2)
        |U 2 (#caa171)
        |R 2 (#7807d2)
        |U 3 (#a77fa3)
        |L 2 (#015232)
        |U 2 (#7a21e3)
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      floodFillCount(parsePart1(input: _*)) shouldBe 62
    }

    it("should match the puzzle description for part 1 using scanLine") {
      scanLineCount(parsePart1(input: _*)) shouldBe 62
    }

    it("should match the puzzle description for a U") {
      // ####  ####
      // #  #  #  #
      // #  #  #  #
      // #  ####  #
      // #        #
      // #        #
      // ##########
      val input = Seq(
        "R" -> 3,
        "D" -> 3,
        "R" -> 3,
        "U" -> 3,
        "R" -> 3,
        "D" -> 6,
        "L" -> 9,
        "U" -> 6
      )
      floodFillCount(input) shouldBe 64
      scanLineCount(input) shouldBe 64
    }

    it("should match the puzzle description for a G") {

      // #############
      // #           #
      // # ######### #
      // # #       # #
      // # # ##### # #
      // # # #   # # #
      // # # ### # # #
      // # #   # # # #
      // # ##### # # #
      // #       # # #
      // ######### ###
      val input = Seq(
        "R" -> 12,
        "D" -> 10,
        "L" -> 2,
        "U" -> 8,
        "L" -> 8,
        "D" -> 6,
        "R" -> 4,
        "U" -> 2,
        "L" -> 2,
        "U" -> 2,
        "R" -> 4,
        "D" -> 6,
        "L" -> 8,
        "U" -> 10
      )
      floodFillCount(input) shouldBe 123
      scanLineCount(input) shouldBe 123
    }

    it("should match the puzzle description for part 2") {
      scanLineCount(parsePart2(input: _*)) shouldBe 952408144115L
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day18Input.txt")
    lazy val answer1 = decryptLong("iaU8yh/JOJaMvNl2JRc9rA==")
    lazy val answer2 = decryptLong("1T/OiD9ac3/jrGs+VAnHAA==")

    it("should have answers for part 1") {
      floodFillCount(parsePart1(input: _*)) shouldBe answer1
    }

    it("should match the puzzle description for part 1 using scanLine") {
      scanLineCount(parsePart1(input: _*)) shouldBe answer1
    }

    it("should have answers for part 2") {
      scanLineCount(parsePart2(input: _*)) shouldBe answer2
    }
  }
}
