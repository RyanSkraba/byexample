package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.collection.immutable

/** =Advent of Code 2022 Day 23 Solutions in scala=
  *
  * Input: A map of elves in a rectangular grid.
  *
  * Part 1: After 10 moves, the number of empty spaces in the smallest bounding
  * rectangle.
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/23]]
  */
class AdventOfCodeDay23Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** The directions of the compass in the order they are tried. */
    object Dir extends Enumeration {
      type Dir = Value
      val North, South, West, East = Value
    }
    import Dir._

    /** Lookup table for the priorities in a cycle. */
    val Priorities: Seq[Seq[Dir]] =
      for (i <- 0 until Dir.maxId)
        yield (0 until Dir.maxId).map(i + _).map(_ % 4).map(Dir(_))

    /** A position of an elf.  The coordinate can be positive or negative. */
    case class Pos(x: Int, y: Int) {
      lazy val nw: Pos = Pos(x - 1, y - 1)
      lazy val n: Pos = Pos(x, y - 1)
      lazy val ne: Pos = Pos(x + 1, y - 1)
      lazy val e: Pos = Pos(x + 1, y)
      lazy val se: Pos = Pos(x + 1, y + 1)
      lazy val s: Pos = Pos(x, y + 1)
      lazy val sw: Pos = Pos(x - 1, y + 1)
      lazy val w: Pos = Pos(x - 1, y)

      /** The neighbours, with the direct followed by the diagonals */
      lazy val north: Seq[Pos] = Seq(n, nw, ne)
      lazy val south: Seq[Pos] = Seq(s, sw, se)
      lazy val west: Seq[Pos] = Seq(w, nw, sw)
      lazy val east: Seq[Pos] = Seq(e, ne, se)

      def neighbours(d: Dir): Seq[Pos] = if (d == North) north
      else if (d == South) south
      else if (d == West) west
      else east

      def unblocked(ps: Set[Pos])(d: Dir): Boolean =
        !neighbours(d).exists(ps(_))

      def lonely(ps: Set[Pos]): Boolean =
        unblocked(ps)(North) && unblocked(ps)(South) && !ps(e) && !ps(w)
    }

    /** All of the elves positions on the plain. */
    case class Plan(elves: Set[Pos]) {
      lazy val (minX: Int, maxX: Int, minY: Int, maxY: Int) = elves.foldLeft(
        (Int.MaxValue, Int.MinValue, Int.MaxValue, Int.MinValue)
      ) { case (acc, elf) =>
        (acc._1 min elf.x, acc._2 max elf.x, acc._3 min elf.y, acc._4 max elf.y)
      }

      lazy val width: Int = maxX - minX + 1
      lazy val height: Int = maxY - minY + 1

      def part1Iterate(i: Int): Plan = {
        val newElves = collection.mutable.Set[Pos]()
        val priorities = Priorities(i % Dir.maxId)
        for (e <- elves) {
          priorities.find(e.unblocked(elves)) match {
            case Some(dir) if dir != priorities.head || !e.lonely(elves) =>
              val newPos = e.neighbours(dir).head
              if (newElves(newPos)) {
                newElves -= newPos
                newElves += e
                newElves += newPos.neighbours(dir).head
              } else newElves += newPos
            case _ => newElves += e
          }
        }
        Plan(newElves.toSet)
      }

      def mkString: String = {
        elves
          .foldLeft("." * (width * height)) { case (acc, elf) =>
            acc.updated(elf.x - minX + (elf.y - minY) * width, '#')
          }
          .grouped(width)
          .mkString("\n")
      }

      def part1(): Int = {
        val plan10 = Stream.from(0).scanLeft(this)(_.part1Iterate(_)).apply(10)
        plan10.width * plan10.height - elves.size
      }

      def part2(): Int = {
        1 + Stream
          .from(0)
          .scanLeft(this)(_.part1Iterate(_))
          .sliding(2)
          .takeWhile(_.distinct.length == 2)
          .length
      }
    }

    object Plan {
      def apply(in: String*): Plan = {
        val elves = (for (
          y <- in.indices;
          x <- in(y).indices if in(y)(x) == '#'
        ) yield Pos(x, y)).toSet
        Plan(elves)
      }
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """....#..
        |..###.#
        |#...#.#
        |.#...##
        |#.###..
        |##.#.##
        |.#..#..
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should iterate correctly on the small example") {
      val simple =
        """.....
          |..##.
          |..#..
          |.....
          |..##.
          |.....""".stripMargin.split("\n").filter(_.nonEmpty)

      val plan0 = Plan(simple: _*)
      plan0.mkString shouldBe
        """##
          |#.
          |..
          |##""".stripMargin

      val plan1 = plan0.part1Iterate(0)
      plan1.mkString shouldBe
        """##
          |..
          |#.
          |.#
          |#.""".stripMargin

      val plan2 = plan1.part1Iterate(1)
      plan2.mkString shouldBe
        """.##.
          |#...
          |...#
          |....
          |.#..""".stripMargin

      val plan3 = plan2.part1Iterate(2)
      plan3.mkString shouldBe
        """..#..
          |....#
          |#....
          |....#
          |.....
          |..#..""".stripMargin

      val plan4 = plan3.part1Iterate(3)
      plan4.mkString shouldBe plan3.mkString
    }

    it("should iterate correctly on the sample") {
      val plan0 = Plan(input: _*)
      val plans = Stream.from(0).scanLeft(plan0) { _.part1Iterate(_) }

      plans.head.width shouldBe 7
      plans.head.height shouldBe 7

      plans(1).width shouldBe 9
      plans(1).height shouldBe 9
      plans(1).mkString shouldBe
        """.....#...
          |...#...#.
          |.#..#.#..
          |.....#..#
          |..#.#.##.
          |#..#.#...
          |#.#.#.##.
          |.........
          |..#..#...
          |""".stripMargin.trim
      plans(2).width shouldBe 11
      plans(2).height shouldBe 9
      plans(2).mkString shouldBe
        """......#....
          |...#.....#.
          |..#..#.#...
          |......#...#
          |..#..#.#...
          |#...#.#.#..
          |...........
          |.#.#.#.##..
          |...#..#....""".stripMargin.trim
      plans(3).width shouldBe 11
      plans(3).height shouldBe 10
      plans(3).mkString shouldBe
        """......#....
          |....#....#.
          |.#..#...#..
          |......#...#
          |..#..#.#...
          |#..#.....#.
          |......##...
          |.##.#....#.
          |..#........
          |......#....""".stripMargin.trim
      plans(4).width shouldBe 11
      plans(4).height shouldBe 10
      plans(4).mkString shouldBe
        """......#....
          |.....#....#
          |.#...##....
          |..#.....#.#
          |........#..
          |#...###..#.
          |.#......#..
          |...##....#.
          |...#.......
          |......#....""".stripMargin.trim
      plans(5).width shouldBe 11
      plans(5).height shouldBe 11
      plans(5).mkString shouldBe
        """......#....
          |...........
          |.#..#.....#
          |........#..
          |.....##...#
          |#.#.####...
          |..........#
          |...##..#...
          |.#.........
          |.........#.
          |...#..#....""".stripMargin.trim

      plans(10).mkString shouldBe
        """......#.....
          |..........#.
          |.#.#..#.....
          |.....#......
          |..#.....#..#
          |#......##...
          |....##......
          |.#........#.
          |...#.#..#...
          |............
          |...#..#..#..""".stripMargin.trim
    }

    it("should match the puzzle description for part 1") {
      val plan = Plan(input: _*)
      plan.part1() shouldBe 110
    }

    it("should match the puzzle description for part 2") {
      val plan = Plan(input: _*)
      plan.part2() shouldBe 20
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day23Input.txt")
    it("should have answers for part 1") {
      val plan = Plan(input: _*)
      plan.elves.size shouldBe 2683
      plan.width shouldBe 73
      plan.height shouldBe 73
      plan.part1() shouldBe 4123
    }

    it("should have answers for part 2 (2 seconds)", Slow) {
      val plan = Plan(input: _*)
      plan.part2() shouldBe 1029
    }
  }
}
