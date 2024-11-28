package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
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
  * Part 2: Given more time but fewer blueprints, find the product of the first three blueprints.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/19]]
  */
class AdventOfCodeDay19Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String*): Seq[Blueprint] =
      in.map(_.split("\\D+").filter(_.nonEmpty).map(_.toInt)).map {
        case Array(id, ore, clay, obsidian1, obsidian2, geode1, geode2, _*) =>
          Blueprint(id, Count(ore, clay, obsidian1, geode1), obsidian2, geode2)
      }

    /** Helper class to keep track of resource and robot counts. Some arithmetic functions are defined, but only on
      * non-geode counts.
      */
    case class Count(ore: Int = 0, clay: Int = 0, obsidian: Int = 0, geode: Int = 0) {

      def +(that: Count): Count = Count(ore + that.ore, clay + that.clay, obsidian + that.obsidian, geode)

      def -(that: Count): Count = Count(ore - that.ore, clay - that.clay, obsidian - that.obsidian, geode)

      def *(m: Int): Count = Count(ore * m, clay * m, obsidian * m, geode)

      def isNonNegative: Boolean = ore >= 0 && clay >= 0 && obsidian >= 0 && geode >= 0
    }

    /** @param id
      *   The ID for the blueprint
      * @param oreFor
      *   The amount of ore necessary to create each robot
      * @param clayForObsidian
      *   The amount of clay necessary to create an obsidian robot
      * @param obsidianForGeode
      *   The amount of obsidian necessary to create a geode robot
      */
    case class Blueprint(id: Int, oreFor: Count, clayForObsidian: Int, obsidianForGeode: Int)

    trait State[T] {
      def isValid: Boolean
      def valid: Option[this.type] = if (isValid) Some(this) else None
      def nextStates: Iterable[T]
    }

    case class BuildState(bp: Blueprint, time: Int, rbt: Count = Count(ore = 1), avail: Count = Count())
        extends State[BuildState] {

      import BuildState._

      override def isValid: Boolean = time > 0 && avail.isNonNegative

      /** @return
        *   the state we'd be in if we built a ore robot next, regardless of whether it's valid (we might go into debt
        *   with resources or go past the time limit).
        */
      def buildOreRobot(): BuildState = {
        val minutes = 1 + turnsFor(avail.ore, bp.oreFor.ore, rbt.ore)
        copy(
          time = time - minutes,
          rbt = rbt.copy(ore = rbt.ore + 1),
          avail = avail + rbt * minutes - Count(bp.oreFor.ore)
        )
      }

      def buildClayRobot(): BuildState = {
        val minutes = 1 + turnsFor(avail.ore, bp.oreFor.clay, rbt.ore)
        copy(
          time = time - minutes,
          rbt = rbt.copy(clay = rbt.clay + 1),
          avail = avail + rbt * minutes - Count(bp.oreFor.clay)
        )
      }

      def buildObsidianRobot(): BuildState = {
        // If we build an obsidian robot next, we're either constrained by the ore or clay
        val minutesOre = 1 + turnsFor(avail.ore, bp.oreFor.obsidian, rbt.ore)
        val minutesClay = 1 + turnsFor(avail.clay, bp.clayForObsidian, rbt.clay)
        val minutes = minutesOre max minutesClay
        copy(
          time = time - minutes,
          rbt = rbt.copy(obsidian = rbt.obsidian + 1),
          avail = avail + rbt * minutes - Count(bp.oreFor.obsidian, clay = bp.clayForObsidian)
        )
      }

      def buildGeodeRobot(): BuildState = {
        // If we build an obsidian robot next, we're either constrained by the
        // ore or obsidian
        val minutesOre = 1 + turnsFor(avail.ore, bp.oreFor.geode, rbt.ore)
        val minutesObsidian = 1 + turnsFor(avail.obsidian, bp.obsidianForGeode, rbt.obsidian)
        val minutes = minutesOre max minutesObsidian
        // If we do build it, this is how many geodes it'll produce until the
        // end of the plan
        val geodes =
          avail.copy(geode = avail.geode + time - minutes)
        copy(
          time = time - minutes,
          rbt = rbt.copy(geode = rbt.geode + 1),
          avail = geodes + rbt * minutes - Count(bp.oreFor.geode, obsidian = bp.obsidianForGeode)
        )
      }

      override def nextStates: Iterable[BuildState] = {
        // The next possible four robots we could build
        val buildOre = buildOreRobot().valid
        val buildClay = buildClayRobot().valid
        val buildObsidian = buildObsidianRobot().valid
        val buildGeode = buildGeodeRobot().valid

        // If we're thinking about building obsidian robot, see if it would be
        // at least as fast to build a different robot first.
        val actuallyBuildObsidian =
          buildObsidian.flatMap(b =>
            if (
              (buildOre.map(_.buildObsidianRobot()) ++ buildClay.map(_.buildObsidianRobot()) ++ buildGeode
                .map(_.buildObsidianRobot())).filter(_.isValid).exists(other => other.time >= b.time)
            ) None
            else buildObsidian
          )

        // Likewise for building a geode robot
        val actuallyBuildGeode =
          buildGeode.flatMap(b =>
            if (
              (buildOre.map(_.buildGeodeRobot()) ++ buildClay.map(_.buildGeodeRobot()) ++ actuallyBuildObsidian
                .map(_.buildGeodeRobot())).filter(_.isValid).exists(other => other.time >= b.time)
            ) None
            else buildGeode
          )

        actuallyBuildGeode ++ actuallyBuildObsidian ++ buildClay ++ buildOre
      }

      def maxGeodes(maxToBeat: Int = 0): Int = {
        // A bad heuristic to cut short if it's impossible to build more geodes
        // than the max already known, even if we had enough resources to build
        // a geode robot every minute
        if (avail.geode + time * (time - 1) / 2 < maxToBeat)
          return maxToBeat
        nextStates.foldLeft(avail.geode max maxToBeat) { case (maxGeodes, state) =>
          state.maxGeodes(maxGeodes) max maxGeodes
        }
      }
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

      val s1 = s0.buildClayRobot()
      s1 shouldBe BuildState(bp, 21, Count(1, 1), Count(1))
      s0.nextStates should contain(s1)

      val s2 = s1.buildClayRobot()
      s2 shouldBe BuildState(bp, 19, Count(1, 2), Count(1, 2))
      s1.nextStates should contain(s2)

      val s3 = s2.buildClayRobot()
      s3 shouldBe BuildState(bp, 17, Count(1, 3), Count(1, 6))
      s2.nextStates should contain(s3)

      val s4 = s3.buildObsidianRobot()
      s4 shouldBe BuildState(bp, 13, Count(1, 3, 1), Count(2, 4))
      s3.nextStates should contain(s4)

      val s5 = s4.buildClayRobot()
      s5 shouldBe BuildState(bp, 12, Count(1, 4, 1), Count(1, 7, 1))
      s4.nextStates should contain(s5)

      val s6 = s5.buildObsidianRobot()
      s6 shouldBe BuildState(bp, 9, Count(1, 4, 2), Count(1, 5, 4))
      s5.nextStates should contain(s6)

      val s7 = s6.buildGeodeRobot()
      s7 shouldBe BuildState(bp, 6, Count(1, 4, 2, 1), Count(2, 17, 3, 6))
      s6.nextStates should contain(s7)

      val s8a = s7.buildClayRobot()
      val s8b = s8a.buildGeodeRobot()
      s8b shouldBe BuildState(bp, 3, Count(1, 5, 2, 2), Count(1, 31, 2, 9))
      s8a.nextStates should contain(s8b)
    }

    it("should not generate an obsidian robot if building another is quicker") {
      val s0 = BuildState(Blueprint(1, Count(1, 1, 1, 1), 100, 100), 1000, Count(1, 1))

      s0.buildOreRobot().buildObsidianRobot().time shouldBe 899
      s0.buildClayRobot().buildObsidianRobot().time shouldBe 948
      s0.buildObsidianRobot().time shouldBe 899
      s0.nextStates should not contain s0.buildObsidianRobot()
    }

    it("should not generate an geode robot if building another is quicker") {
      val s0 = BuildState(
        Blueprint(1, Count(1, 1, 1, 1), 1, 100),
        1000,
        Count(1, 1, 1)
      )

      s0.buildOreRobot().buildGeodeRobot().time shouldBe 899
      s0.buildClayRobot().buildGeodeRobot().time shouldBe 899
      s0.buildObsidianRobot().buildGeodeRobot().time shouldBe 948
      s0.buildGeodeRobot().time shouldBe 899
      s0.nextStates should not contain s0.buildGeodeRobot()
    }

    it("should match the puzzle description for part 1 (1 seconds)", Slow) {
      part1(24, bps) shouldBe 33
    }

    it("should match the puzzle description for part 2, blueprint 1") {
      BuildState(bps.head, 32).maxGeodes() shouldBe 56
    }

    it("should match the puzzle description for part 2 (12 seconds)", Slow) {
      part2(32, bps.take(3)) shouldBe 56 * 62
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day19Input.txt")
    lazy val bps = parse(input: _*)

    it("should have answers for part 1 (20 seconds)", Slow) {
      part1(24, bps) shouldBe decryptLong("qpLyiTwKt/cyp45AVdQP1A==")
    }

    it("should have answers for part 2 (2 minutes)", Slow) {
      part2(32, bps.take(3)) shouldBe decryptLong("hkdXLrLagO7WIap0aYFwSg==")
    }
  }
}
