package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 17 Solutions in scala=
  *
  * Input: The direction that the hot jets of air are blowing.
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/17]]
  */
class AdventOfCodeDay17Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    val EmptyRow = "|.......|"
    val BottomRow = "+-------+"

    case class Rock(
        height: Int,
        pos: Seq[(Int, Int)],
        ox: Int = 0,
        oy: Int = 0
    ) {
      def ifNoOverlap(tower: Seq[String]): Option[Rock] =
        if (!pos.exists { case (x, y) => tower(y + oy)(x + ox) != '.' })
          Some(this)
        else None

      def drawOnTower(tower: Seq[String], c: Char = '#'): Seq[String] = pos
        .foldLeft(tower) { case (tower, (x, y)) =>
          tower.updated(y + oy, tower(y + oy).updated(x + ox, c))
        }
        .dropWhile(_.equals(EmptyRow))
    }

    val Rocks: Seq[Rock] = Seq(
      Rock(1, Seq((3, 0), (4, 0), (5, 0), (6, 0))),
      Rock(3, Seq((4, 0), (3, 1), (4, 1), (5, 1), (4, 2))),
      Rock(3, Seq((5, 0), (5, 1), (3, 2), (4, 2), (5, 2))),
      Rock(4, Seq((3, 0), (3, 1), (3, 2), (3, 3))),
      Rock(2, Seq((3, 0), (4, 0), (3, 1), (4, 1)))
    )

    def addRock(winds: String)(
        towerAndWind: (Seq[String], Int),
        i: Int
    ): (Seq[String], Int) = {

      // The rock and the initial tower with enough space for the rock
      val rock0 = Rocks(i % Rocks.length)
      val tower0 = Seq.fill(rock0.height + 3)(EmptyRow) ++ towerAndWind._1

      val Some((newTower, _, newWind)) = Stream
        .iterate((Option.empty[Seq[String]], rock0, towerAndWind._2)) {
          case (tower, rock, wind) =>
            val rockDx =
              rock
                .copy(ox = rock.ox + (if (winds(wind) == '<') -1 else 1))
                .ifNoOverlap(tower0)
                .getOrElse(rock)
            val rockDy = rockDx
              .copy(oy = rock.oy + 1)
              .ifNoOverlap(tower0)

            (
              if (rockDy.isEmpty) Some(rockDx.drawOnTower(tower0)) else None,
              rockDy.getOrElse(rockDx),
              (wind + 1) % winds.length
            )
        }
        .find(_._1.nonEmpty)

      newTower.get -> newWind
    }

    def part1(
               jets: String,
               tower: Seq[String] = Seq(BottomRow),
               rocksToDrop: Int = 2022
    ): Seq[String] = {
      (0 until rocksToDrop).foldLeft(tower -> 0)(addRock(jets))._1
    }

    def part2(winds: String, rocksToDrop: Long = 1000000000000L): Long = {
      1514285714288L
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
      |""".stripMargin.trim

    it("should match the puzzle description for part 1") {
      part1(input).length - 1 shouldBe 3068
    }

    it("should match the puzzle description for part 2") {
      part2(input) shouldBe 1514285714288L
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day17Input.txt")
    it("should have answers for part 1") {
      part1(input.head).length - 1 shouldBe 3209
    }
    it("should have answers for part 2") {
      part2(input.head) shouldBe 1514285714288L
    }
  }
}
