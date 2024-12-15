package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** =Advent of Code 2024 Day 13 Solutions in scala=
  *
  * Input: Descriptions of a claw machine with two buttons and a prize location. Each button moves the claw a specific
  * amount in the x and y directions. Pushing button A costs three tokens and button B costs one.
  *
  * Part 1: For each game, find the number of tokens necessary to win the winnable games with less than 100 button
  * pushes on any button. Ignore unwinnable games.
  *
  * Part 2: Add 10000000000000 to the X and Y coordinates of the prize and repeat with any number of button pushes.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/13]]
  */
class AdventOfCodeDay13Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    val ClawRe = "Button A: X(.+), Y(.+)\nButton B: X(.+), Y(.+)\nPrize: X=(.+), Y=(.+)".r: Regex

    /** A configuration for the claw machine. Each push of A moves the claw (ax, ay), B moves the claw (bx, by) and (px,
      * py) is the destination.
      *
      * for N pushes of A and M pushes of B that succeeds:
      *
      * {{{
      * |N| |ax bx| = |px|
      * |M| |ay by| = |py|
      *
      * N * ax + M * bx = px    N * ax / px + M * bx / px = 1
      * N * ay + M * by = py    N * ay / py + M * by / py = 1
      *
      * N * ax / px + M * bx / px =  N * ay / py + M * by / py
      * N * ax / px - N * ay / py =  M * by / py M - M * bx / px
      * N * (ax / px - ay / py) = M * (by / py - bx / px)
      * N  = M * (by / py - bx / px) / (ax / px - ay / py)
      *
      * N + M * bx / ax = px / ax
      * N + M * by / ay = py / ay
      *
      * M * bx / ax - M * by / ay = px / ax - py / ay
      * M  = (px / ax - py / ay ) / (bx / ax - by / ay)
      * }}}
      */
    case class Claw(ax: Long, ay: Long, bx: Long, by: Long, px: Long, py: Long) {

      def iterateMinCostForWin(): Long = {
        // Iterate over all the possibilities for N, and check whether M is a possible integer candidate.
        val costs =
          for (
            n <- 0L to (px / ax min 100L min py / ay);
            distAx = n * ax if distAx <= px;
            distAy = n * ay if distAy <= py;
            m = (px - distAx) / bx if (px - distAx) % bx == 0
          ) yield if ((py - distAy) % by == 0 && (py - distAy) / by == m) Some(3 * n + m, n, m) else None
        costs.flatten.map(_._1).minOption.getOrElse(0L)
      }

      def solveMinCostForWin(offset: Long = 10000000000000L): Long = {
        // The new prize location
        val opx = px + offset.toDouble
        val opy = py + offset.toDouble

        // Solving for m numerically.
        val m = (opx / ax - opy / ay) / (bx.toDouble / ax - by.toDouble / ay)
        val n = (opx - m * bx) / ax
        if (n < 0 || m < 0) return 0

        // Check that it's "close enough" to an integer by recalculating the positions.
        val mm = Math.round(m)
        val nn = Math.round(n)

        if (nn * ax + mm * bx == opx && nn * ay + mm * by == opy) mm + 3 * nn
        else 0
      }
    }

    def parse(in: String): Option[Claw] = ClawRe
      .findFirstMatchIn(in)
      .map(m =>
        Claw(
          m.group(1).toLong,
          m.group(2).toLong,
          m.group(3).toLong,
          m.group(4).toLong,
          m.group(5).toLong,
          m.group(6).toLong
        )
      )

    def part1(in: String): Long = in.split("\n\n").flatMap(parse).map(_.iterateMinCostForWin()).sum

    def part2(in: String): Long = in.split("\n\n").flatMap(parse).map(_.solveMinCostForWin()).sum
  }

  import Solution._

  describe("Example case") {
    val input =
      """Button A: X+94, Y+34
        |Button B: X+22, Y+67
        |Prize: X=8400, Y=5400
        |
        |Button A: X+26, Y+66
        |Button B: X+67, Y+21
        |Prize: X=12748, Y=12176
        |
        |Button A: X+17, Y+86
        |Button B: X+84, Y+37
        |Prize: X=7870, Y=6450
        |
        |Button A: X+69, Y+23
        |Button B: X+27, Y+71
        |Prize: X=18641, Y=10279
        |""".trim.stripMargin

    it("should match the puzzle description for part 1") {
      part1(input) shouldBe 480
    }

    it("should match the puzzle description for part 2") {
      part2(input) shouldBe 875318608908L
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day13Input.txt").mkString("\n")
    lazy val answer1 = decryptLong("QtIUofosvafPBdWIBAVHUQ==")
    lazy val answer2 = decryptLong("WZ4/f0ChMTGVgvLs7vDa1Q==")

    it("should have answers for part 1") {
      part1(input) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input) shouldBe answer2
    }
  }
}
