package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** =Advent of Code 2022 Day 19 Solutions in scala=
  *
  * Input: Blueprints for building ore, clay, obsidian and geode robots.
  *
  * Part 1: The quality index for the blueprints combined.
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/19]]
  */
class AdventOfCodeDay19Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    def parse(in: String*): Seq[Blueprint] =
      in.map(_.split("\\D+").filter(_.nonEmpty).map(_.toInt)).map {
        case Array(id, ore, clay, obsidian1, obsidian2, geode1, geode2, _*) =>
          Blueprint(id, Count(ore, clay, obsidian1, geode1), obsidian2, geode2)
      }

    /** Helper class to keep track of resource and robot counts. Some arithmetic
      * functions are defined, but only on non-geode counts.
      */
    case class Count(
        ore: Int = 0,
        clay: Int = 0,
        obsidian: Int = 0,
        geode: Int = 0
    ) {

      def +(that: Count): Count =
        Count(ore + that.ore, clay + that.clay, obsidian + that.obsidian, geode)

      def -(that: Count): Count =
        Count(ore - that.ore, clay - that.clay, obsidian - that.obsidian, geode)

      def *(m: Int): Count = Count(ore * m, clay * m, obsidian * m, geode)

      def hasNeg: Boolean = ore < 0 || clay < 0 || obsidian < 0 || geode < 0
    }

    case class Blueprint(
        id: Int,
        oreFor: Count,
        clayForObsidian: Int,
        obsidianForGeode: Int
    )

    trait State[T] {
      def isValid: Boolean
      def nextStates: Seq[T]
    }

    case class BuildState(
        bp: Blueprint,
        time: Int,
        rbt: Count = Count(ore = 1),
        avail: Count = Count()
    ) extends State[BuildState] {

      import BuildState._

      def buildOreRobot(): BuildState = {
        val minutes =
          1 + turnsFor(avail.ore, bp.oreFor.ore, rbt.ore)
        copy(
          time = time - minutes,
          rbt = rbt.copy(ore = rbt.ore + 1),
          avail = avail + rbt * minutes - Count(bp.oreFor.ore)
        )
      }

      def buildClayRobot(): BuildState = {
        val minutes =
          1 + turnsFor(avail.ore, bp.oreFor.clay, rbt.ore)
        copy(
          time = time - minutes,
          rbt = rbt.copy(clay = rbt.clay + 1),
          avail = avail + rbt * minutes - Count(bp.oreFor.clay)
        )
      }

      def buildObsidianRobot(): BuildState = {
        val minutesOre =
          1 + turnsFor(avail.ore, bp.oreFor.obsidian, rbt.ore)
        val minutesClay =
          1 + turnsFor(avail.clay, bp.clayForObsidian, rbt.clay)
        val minutes = minutesOre max minutesClay
        copy(
          time = time - minutes,
          rbt = rbt.copy(obsidian = rbt.obsidian + 1),
          avail = avail + rbt * minutes - Count(
            bp.oreFor.obsidian,
            clay = bp.clayForObsidian
          )
        )
      }

      def buildGeodeRobot(): BuildState = {
        val minutesOre =
          1 + turnsFor(avail.ore, bp.oreFor.geode, rbt.ore)
        val minutesObsidian = 1 + turnsFor(
          avail.obsidian,
          bp.obsidianForGeode,
          rbt.obsidian
        )
        val minutes = minutesOre max minutesObsidian
        val geodes =
          avail.copy(geode = avail.geode + time - minutes)
        copy(
          time = time - minutes,
          rbt = rbt.copy(geode = rbt.geode + 1),
          avail = geodes + rbt * minutes - Count(
            bp.oreFor.geode,
            obsidian = bp.obsidianForGeode
          )
        )
      }

      override def isValid: Boolean = time > 0 && !avail.hasNeg

      override def nextStates: Seq[BuildState] = {
        if (!isValid) return Seq.empty
        Seq(
          buildOreRobot(),
          buildClayRobot(),
          buildObsidianRobot(),
          buildGeodeRobot()
        )
      }

      def maxGeodes(): Int = (nextStates.map(_.maxGeodes()) :+ avail.geode).max
    }

    object BuildState {
      // If I need n resources and provide x per turn, how many turns do I need to build?
      def turnsFor(have: Int, necessary: Int, providesPerTurn: Int): Int =
        if (providesPerTurn <= 0) Int.MaxValue - 10
        else if (have >= necessary) 0
        else ((necessary - have - 1) / providesPerTurn) + 1
    }

    def part1(time: Int, in: Seq[Blueprint]): Long = {
      in.map(bp => bp.id * BuildState(bp, time).maxGeodes()).sum
    }

    def part2(time: Int, in: Seq[Blueprint]): Long = {
      in.map(bp => BuildState(bp, time).maxGeodes()).product
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Blueprint 1:
            |  Each ore robot costs 4 ore.
            |  Each clay robot costs 2 ore.
            |  Each obsidian robot costs 3 ore and 14 clay.
            |  Each geode robot costs 2 ore and 7 obsidian.
            |
            |Blueprint 2:
            |  Each ore robot costs 2 ore.
            |  Each clay robot costs 3 ore.
            |  Each obsidian robot costs 3 ore and 8 clay.
            |  Each geode robot costs 3 ore and 12 obsidian.
            |""".stripMargin.split("\n\n")
    val bps = parse(input: _*)

    it("should generate the example states") {
      val bp = bps.head
      val s0 = BuildState(bp, 24)

      val s1 = {
        val next = s0.nextStates
        next(1)
      }
      s1 shouldBe BuildState(bp, 21, Count(1, 1), Count(1))

      val s2 = {
        val next = s1.nextStates
        next(1)
      }
      s2 shouldBe BuildState(bp, 19, Count(1, 2), Count(1, 2))

      val s3 = {
        val next = s2.nextStates
        next(1)
      }
      s3 shouldBe BuildState(bp, 17, Count(1, 3), Count(1, 6))

      val s4 = {
        val next = s3.nextStates
        next(2)
      }
      s4 shouldBe BuildState(bp, 13, Count(1, 3, 1), Count(2, 4))

      val s5 = {
        val next = s4.nextStates
        next(1)
      }
      s5 shouldBe BuildState(bp, 12, Count(1, 4, 1), Count(1, 7, 1))

      val s6 = {
        val next = s5.nextStates
        next(2)
      }
      s6 shouldBe BuildState(bp, 9, Count(1, 4, 2), Count(1, 5, 4))

      val s7 = {
        val next = s6.nextStates
        next(3)
      }
      s7 shouldBe BuildState(bp, 6, Count(1, 4, 2, 1), Count(2, 17, 3, 6))

      val s8 = {
        val next = s7.nextStates
        next(3)
      }
      s8 shouldBe BuildState(bp, 3, Count(1, 4, 2, 2), Count(3, 29, 2, 9))
    }

    it("should match the puzzle description for part 1 (51 seconds)", Slow) {
      part1(24, bps) shouldBe 33
    }

    it("should match the puzzle description for part 2 (XX seconds)", Slow) {
      part2(32, bps.take(3)) shouldBe 56 * 62
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day19Input.txt")
    val bps = parse(input: _*)

    it("should have answers for part 1 (3 minutes)", Slow) {
      part1(24, bps) shouldBe 1650
    }

    it("should have answers for part 2 (XX seconds)", Slow) {
      part2(32, bps.take(3)) shouldBe 200
    }

  }
}
