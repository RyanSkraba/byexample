package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

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
class AdventOfCodeDay17Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    val EmptyRow = "|.......|"
    val EmptyTower: Seq[String] = Seq("+-------+")

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

    def addRock(
        jets: String
    )(acc: (Seq[String], Int), rockI: Int): (Seq[String], Int) = {
      val (tower, jet0) = acc

      // The rock and the initial tower with enough space for the rock
      val rock0 = Rocks(rockI % Rocks.length)
      val tower0 = Seq.fill(rock0.height + 3)(EmptyRow) ++ tower

      val Some((towerNext, _, jetNext)) = Stream
        .iterate((Option.empty[Seq[String]], rock0, jet0)) { case (None, rock, jetN) =>
          val rockDx =
            rock
              .copy(ox = rock.ox + (if (jets(jetN) == '<') -1 else 1))
              .ifNoOverlap(tower0)
              .getOrElse(rock)
          val rockDy = rockDx
            .copy(oy = rock.oy + 1)
            .ifNoOverlap(tower0)
          (
            if (rockDy.isEmpty) Some(rockDx.drawOnTower(tower0)) else None,
            rockDy.getOrElse(rockDx),
            (jetN + 1) % jets.length
          )
        }
        .find(_._1.nonEmpty)

      (towerNext.get, jetNext)
    }

    def part1(
        jets: String,
        rocksToDrop: Int = 2022
    ): Seq[String] = {
      LazyList
        .from(0)
        .scanLeft(EmptyTower -> 0)(addRock(jets))
        .drop(rocksToDrop)
        .head
        ._1
    }

    def part2(jets: String, rocksToDrop: Long = 1000000000000L): Long = {

      // The endless stream of dropped rocks
      val dropped: Seq[(Seq[String], Int)] =
        LazyList.from(0).scanLeft(EmptyTower -> 0)(addRock(jets))

      // These were calculated by investigation...
      val targetJetI = if (jets.length == 40) 2 else 1

      // Find the cycles in the towers at the target jet indices
      val cycle: Seq[(Int, Int)] = dropped.zipWithIndex
        .filter { case ((_, jetI), rockI) =>
          jetI == targetJetI && rockI % Rocks.length == 0
        }
        .map { case ((tower, _), rockI) =>
          (rockI, tower.length - 1)
        }
        .take(3)

      // This is used to calculate, in a cycle, how many rocks provide what height
      val (r1, h1) = cycle.dropRight(1).last
      val (r2, h2) = cycle.last

      // This is the additional height to take into account from the remainder and the rocks before r1
      val remainder = (rocksToDrop - r1) % (r2 - r1)
      val initTower = dropped(r1 + remainder.toInt)._1.length - 1

      initTower + (rocksToDrop - r1) / (r2 - r1) * (h2 - h1)
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

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day17Input.txt")
    it("should have answers for part 1") {
      part1(input.head).length - 1 shouldBe decryptLong(
        "lBfsnZ3yvW2YWHSfdITHFQ=="
      )
    }
    it("should have answers for part 2") {
      part2(input.head) shouldBe decryptLong("fVutY3CXJ7XExXTxh3Tipg==")
    }
  }
}
