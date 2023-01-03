package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2022 Day 24 Solutions in scala=
  *
  * Input: A map of the extraction point with all of the blizzards marked by the
  * direction they are taking.
  *
  * Part 1: The minimum number of minutes that it takes to get from 0,0 to the
  * bottom right corner.
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/24]]
  */
class AdventOfCodeDay24Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Blizzard(
        txt: String,
        width: Int,
        hit: Char,
        ox: Int = 0,
        oy: Int = 0
    ) {
      lazy val height: Int = txt.length / width

      def apply(t: Int, x: Int, y: Int): Boolean = {
        val dx = (ox * t + x) % width
        val dy = (oy * t + y) % height
        hit == txt(
          width * (if (dy < 0) dy + height else dy) +
            (if (dx < 0) dx + width else dx)
        )
      }
    }

    case class Blizzards(blzs: Blizzard*) {

      val width: Int = blzs.head.width
      val height: Int = blzs.head.height

      def apply(t: Int, x: Int, y: Int): Boolean =
        x < 0 || y < 0 || x >= width || y >= height || blzs.exists(_(t, x, y))

      def mkString(t: Int): String =
        (for (y <- 0 until height; x <- 0 until width) yield {
          blzs.filter(_(t, x, y)) match {
            case Nil           => '.'
            case Seq(blizzard) => blizzard.hit
            case blizzards     => blizzards.length.toString.head
          }
        }).grouped(width).map(_.mkString).mkString("\n")
    }

    object Blizzards {
      def apply(in: String): Blizzards = {
        val map = in.split("\n")
        val width = map.head.length - 2
        val txt = map.drop(1).dropRight(1).map(_.drop(1).dropRight(1)).mkString
        Blizzards(
          Blizzard(txt, width, '^', oy = 1),
          Blizzard(txt, width, '>', ox = -1),
          Blizzard(txt, width, 'v', oy = -1),
          Blizzard(txt, width, '<', ox = 1)
        )
      }
    }

    trait State[T] {
      def isValid: Boolean

      def valid: Option[this.type] = if (isValid) Some(this) else None

      def nextStates: Iterable[T]
    }

    case class MoveState(
        init: Boolean = true,
        time: Int = 0,
        x: Int = 0,
        y: Int = -1,
        blizzards: Blizzards
    ) extends State[MoveState] {
      override def isValid: Boolean = init ||
        (x == blizzards.width - 1 && y == blizzards.height) ||
        !blizzards(time, x, y)

      override def nextStates: Iterable[MoveState] = Seq(
        copy(time = time + 1, x = x + 1, init = false),
        copy(time = time + 1, y = y + 1, init = false),
        copy(time = time + 1, x = x - 1, init = false),
        copy(time = time + 1, y = y - 1, init = false),
        copy(time = time + 1)
      ).filter(_.isValid)

      def dfsTimeToDestination(best: Int = Int.MaxValue): Int = {
        if (x == blizzards.width - 1 && y == blizzards.height)
          return time
        if (time + blizzards.width - x + blizzards.height - y - 2 >= best)
          return best
        nextStates.foldLeft(best) { case (best, state) =>
          state.dfsTimeToDestination(best) min best
        }
      }

      def bfsTimeToDestination(): Int = {
        val bfs = mutable.SortedSet(this)(Ordering.by(x => (x.time, x.x, x.y)))
        while (bfs.nonEmpty) {
          val next = bfs.head
          if (next.x == blizzards.width - 1 && next.y == blizzards.height)
            return next.time
          bfs ++= next.nextStates
          bfs -= next
        }
        // Not found
        -1
      }
    }

    def part2(in: String*): Long = 200
  }

  import Solution._

  describe("Example case") {
    val input =
      """#.######
        |#>>.<^<#
        |#.<..<<#
        |#>v.><>#
        |#<^v^^>#
        |######.#
        |""".stripMargin

    it("should test hits correctly") {
      val b = Blizzards("""#.#####
          |#.....#
          |#>....#
          |#.....#
          |#...v.#
          |#.....#
          |#####.#
          |""".stripMargin)

      // At time 0
      b.mkString(0) shouldBe
        """.....
          |>....
          |.....
          |...v.
          |.....""".stripMargin
      b(0, 0, 1) shouldBe true
      b.blzs.head(0, 0, 1) shouldBe false
      b.blzs(1)(0, 0, 1) shouldBe true
      b.blzs(2)(0, 0, 1) shouldBe false
      b.blzs(3)(0, 0, 1) shouldBe false
      b(0, 3, 3) shouldBe true

      // All points on the blizzard at time 0
      for (x <- 0 until 5; y <- 0 until 5)
        withClue(s"at time 0, $x and $y") {
          b(0, x, y) shouldBe (x == 0 && y == 1 || x == 3 && y == 3)
        }

      // At time 1
      b.mkString(1) shouldBe
        """.....
          |.>...
          |.....
          |.....
          |...v.""".stripMargin
      b(1, 0, 1) shouldBe false
      b(1, 1, 1) shouldBe true
      b(1, 3, 3) shouldBe false
      b(1, 3, 4) shouldBe true
      for (x <- 0 until 5; y <- 0 until 5) withClue(s"at time 1, $x and $y") {
        b(1, x, y) shouldBe (x == 1 && y == 1 || x == 3 && y == 4)
      }

      // At time 2
      b.mkString(2) shouldBe
        """...v.
          |..>..
          |.....
          |.....
          |.....""".stripMargin
      for (x <- 0 until 5; y <- 0 until 5) withClue(s"at time 1, $x and $y") {
        b(2, x, y) shouldBe (x == 2 && y == 1 || x == 3 && y == 0)
      }

      // At time 3 both points overlap
      b.mkString(3) shouldBe
        """.....
          |...2.
          |.....
          |.....
          |.....""".stripMargin
      for (x <- 0 until 5; y <- 0 until 5) withClue(s"at time 1, $x and $y") {
        b(3, x, y) shouldBe (x == 3 && y == 1)
      }

      // At time 4
      b.mkString(4) shouldBe
        """.....
          |....>
          |...v.
          |.....
          |.....""".stripMargin
      for (x <- 0 until 5; y <- 0 until 5) withClue(s"at time 1, $x and $y") {
        b(4, x, y) shouldBe (x == 4 && y == 1 || x == 3 && y == 2)
      }

      // At time 5 (every point has wrapped around)
      b.mkString(5) shouldBe b.mkString(0)
      for (x <- 0 until 5; y <- 0 until 5) withClue(s"at time 1, $x and $y") {
        b(5, x, y) shouldBe (x == 0 && y == 1 || x == 3 && y == 3)
      }
    }

    it("should match the puzzle description for part 1") {
      val b = Blizzards(input)
      b.height shouldBe 4
      b.width shouldBe 6
      // Compare the depth first search to breadth-first search
      MoveState(blizzards = b).dfsTimeToDestination() shouldBe 18
      MoveState(blizzards = b).bfsTimeToDestination() shouldBe 18
    }

    it("should match the puzzle description for part 2") {
      part2(input) shouldBe 200
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day24Input.txt").mkString("\n")
    it("should have answers for part 1") {
      val b = Blizzards(input)
      b.height shouldBe 25
      b.width shouldBe 120
      // DFS doesn't finish
      // MoveState(blizzards = b).dfsTimeToDestination() shouldBe 253
      MoveState(blizzards = b).bfsTimeToDestination() shouldBe 253
    }

    it("should have answers for part 2") {
      part2(input) shouldBe 200
    }
  }
}
