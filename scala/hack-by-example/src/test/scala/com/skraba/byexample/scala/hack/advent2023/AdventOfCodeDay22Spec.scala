package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

/** =Advent of Code 2023 Day 22 Solutions in scala=
  *
  * Input: A list of bricks of sand hanging in the air.
  *
  * Part 1: Drop the bricks like tetris blocks so they are stacked on each
  * other. Count how many bricks can be removed while leaving every single other
  * brick in place.
  *
  * Part 1: For every brick that would cause other bricks to fall if it were
  * removed, count the number of falling bricks. Add these all together.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/22]]
  */
class AdventOfCodeDay22Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Brick(
        x1: Long,
        y1: Long,
        z1: Long,
        x2: Long,
        y2: Long,
        z2: Long
    ) {
      def toTuple: (Long, Long, Long, Long, Long, Long) =
        (x1, y1, z1, x2, y2, z2)
    }

    object Brick {
      def apply(in: String): Brick = {
        val xyz = in.split("[,~]").map(_.toLong)
        Brick(
          xyz.head min xyz(3),
          xyz(1) min xyz(4),
          xyz(2) min xyz(5),
          xyz.head max xyz(3),
          xyz(1) max xyz(4),
          xyz(2) max xyz(5)
        )
      }
    }

    case class Plan(
        bs: Seq[Brick] = Seq.empty,
        zs: Map[(Long, Long), Brick] = Map(),
        supporting: Set[(Brick, Brick)] = Set.empty
    ) {

      /** For each brick, the bricks that it supports. */
      lazy val supports: Map[Brick, Set[Brick]] =
        supporting.groupMap(_._1)(_._2).withDefaultValue(Set.empty)

      /** For each brick, the bricks that support it. */
      lazy val supportedBy: Map[Brick, Set[Brick]] =
        supporting.groupMap(_._2)(_._1).withDefaultValue(Set.empty)

      lazy val safeToRemove: Set[Brick] = bs.filter { b =>
        !supports(b).map(supportedBy).exists(_.size <= 1)
      }.toSet

      def drop(b: Brick): Plan = {
        val zBricks =
          for (
            x <- b.x1 to b.x2;
            y <- b.y1 to b.y2;
            z <- zs.get(x -> y)
          )
            yield z

        val zMax: Long = zBricks.maxByOption(_.z2).map(_.z2).getOrElse(0)
        val b1 = b.copy(z1 = zMax + 1L, z2 = b.z2 - b.z1 + zMax + 1)

        val zs1: Seq[((Long, Long), Brick)] =
          for (
            x <- b.x1 to b.x2;
            y <- b.y1 to b.y2
          ) yield (x -> y -> b1)

        val supports1 = zBricks.filter(_.z2 == zMax).map(_ -> b1)

        Plan(
          bs = bs :+ b1,
          zs = zs ++ zs1,
          supporting = supporting ++ supports1
        )
      }

    }

    object Plan {
      def from(in: String*): Plan =
        in.map(Brick.apply).sortBy(_.z1).foldLeft(Plan()) { (acc, b) =>
          acc.drop(b)
        }
    }

    def part1(in: String*): Long = {
      val plan = Plan.from(in: _*)
      plan.safeToRemove.size
    }

    def part2(in: String*): Long = {
      val plan = Plan.from(in: _*)

      /** If you consider the graph of supported and supporting bricks, brick1
        * "dominates" another brick2 if any path to brick2 from the bottom brick
        * must pass through brick one.
        */
      @tailrec
      def dominates(
          falling: Set[Brick],
          fallen: Set[Brick] = Set.empty
      ): Set[Brick] = {
        // If there are no bricks that are currently "falling", then return the
        // list of fallen bricks.
        if (falling.isEmpty) fallen
        else {
          // Otherwise, find all of the bricks supported by the "falling" bricks
          // that are themselves supported ONLY by fallen and falling bricks.
          dominates(
            falling
              .flatMap(plan.supports)
              .filterNot(
                !plan
                  .supportedBy(_)
                  .filterNot(falling)
                  .forall(fallen)
              ),
            falling ++ fallen
          )
        }
      }

      plan.bs
        .filterNot(plan.safeToRemove)
        .map(Set(_))
        .map(dominates(_))
        .map(_.size - 1)
        .sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """1,0,1~1,2,1
        |0,0,2~2,0,2
        |0,2,3~2,2,3
        |0,0,4~0,2,4
        |2,0,5~2,2,5
        |0,1,6~2,1,6
        |1,1,8~1,1,9
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 5
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 7
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day22Input.txt")
    lazy val answer1 = decryptLong("VYu7M9BoWrU3wucbdFU08g==")
    lazy val answer2 = decryptLong("DRilwn4zwOvWPwHrn3C1Pg==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
