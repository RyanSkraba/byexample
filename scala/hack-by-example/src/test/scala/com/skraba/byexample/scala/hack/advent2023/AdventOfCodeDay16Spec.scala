package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** =Advent of Code 2023 Day 16 Solutions in scala=
  *
  * Input: A board containing diagonal light mirrors / and \ and splitters - and \|. Light passes through the board and
  * every mirror changes its direction by 90 degrees, while every splitter (if hit on the flat side) changes it 90
  * degrees in both directions. Light passes through each other and through splitters in the same direction.
  *
  * Part 1: If a beam starts at the upper left corner going right, how many squares are visited by a beam of light
  * (going in any direction).
  *
  * Part 2: If a beam starts at any edge square heading away from the edge, what is the maximum number of squares
  * energized by the light beams?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/16]]
  */
class AdventOfCodeDay16Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }
    import Dir._

    case class Plan(input: Seq[String], border: String = "#") {

      /** The width of the full plan */
      lazy val dx: Int = input.headOption.map(_.length).getOrElse(0) + 2

      /** The height of the full plan */
      lazy val dy: Int = input.length + 2

      /** The full plan as a single string, including a full border. */
      lazy val full: String =
        input.mkString(border * (dx + 1), border * 2, border * (dx + 1))

      lazy val display: String = full.grouped(dx).mkString("\n")

      /** A cursor moving through the plan. */
      case class Cursor(pos: Int, dir: Dir) {
        lazy val e: Cursor = Cursor(pos + 1, East)
        lazy val s: Cursor = Cursor(pos + dx, South)
        lazy val w: Cursor = Cursor(pos - 1, West)
        lazy val n: Cursor = Cursor(pos - dx, North)
        lazy val straight: Cursor = mv(dir)
        lazy val rot180: Cursor = copy(dir = dir match {
          case East  => West
          case South => North
          case West  => East
          case North => South
        })
        lazy val at: Char = full(pos)

        def mv(to: Dir): Cursor = to match {
          case East  => e
          case South => s
          case West  => w
          case North => n
        }
      }

      object Cursor {
        def apply(x: Int, y: Int, dir: Dir): Cursor =
          Cursor((y + 1) * dx + x + 1, dir)
      }

      def mv(in: Cursor): Set[Cursor] = (in.dir, full(in.pos)) match {
        case (_, '#')                     => Set.empty
        case (East | West, '|')           => Set(in.n, in.s)
        case (North | South, '-')         => Set(in.w, in.e)
        case (East, '\\') | (West, '/')   => Set(in.s)
        case (South, '\\') | (North, '/') => Set(in.e)
        case (West, '\\') | (East, '/')   => Set(in.n)
        case (North, '\\') | (South, '/') => Set(in.w)
        case _                            => Set(in.straight)
      }

      /** Count the energized squares if the beam of light starts at the given position
        */
      def count(start: Cursor): Long = {
        // Iterate over the list of positions that have been visited and
        // the current "heads" of the beams of light, until there are no more heads.
        case class Acc(beam: Set[Cursor], visited: Set[Cursor] = Set.empty)

        LazyList
          .iterate(new Acc(Set(start))) { acc =>
            Acc(
              acc.beam.flatMap(mv).filterNot(acc.visited),
              acc.visited ++ acc.beam
            )
          }
          .dropWhile(_.beam.nonEmpty)
          .head
          .visited
          .map(_.pos)
          .count(full(_) != '#')
      }
    }

    def part1(in: String*): Long = {
      val plan = Plan(in)
      plan.count(plan.Cursor(plan.dx + 1, East))
    }

    def part2(in: String*): Long = {
      val plan = Plan(in)
      math.max(
        (0 until plan.dx - 2)
          .map(x =>
            math.max(
              plan.count(plan.Cursor(x, 0, South)),
              plan.count(plan.Cursor(x, plan.dy - 2, North))
            )
          )
          .max,
        (0 until plan.dy - 2)
          .map(y =>
            math.max(
              plan.count(plan.Cursor(0, y, East)),
              plan.count(plan.Cursor(plan.dx - 2, y, West))
            )
          )
          .max
      )

    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """.|...\....
        ||.-.\.....
        |.....|-...
        |........|.
        |..........
        |.........\
        |..../.\\..
        |.-.-/..|..
        |.|....-|.\
        |..//.|....
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 46
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 51
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day16Input.txt")
    lazy val answer1 = decryptLong("v9G03qt+Jbs9YlAcxjNs8g==")
    lazy val answer2 = decryptLong("71mbkG+Zkzjwxt/KKjBWIA==")
    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2 (2 seconds)", Slow) {
      part2(input: _*) shouldBe answer2
    }
  }
}
