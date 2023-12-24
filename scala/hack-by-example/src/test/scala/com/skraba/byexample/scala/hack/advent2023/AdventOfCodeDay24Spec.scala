package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 24 Solutions in scala=
  *
  * Input: Many hailstone trajectories with a position in 3D space and a
  * velocity vector.
  *
  * Part 1: Ignoring the Z axis, find the number of hailstone trajectories that
  * intersect in a given square. The intersection doesn't need to be at the same
  * time, but it needs to happen after time 0.
  *
  * Part 2: Given all the trajectories, find one more trajectory that intersects
  * every hailstone at some point. Add the coordinates of the starting point of
  * that trajectory.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/24]]
  */
class AdventOfCodeDay24Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Hail(
        px: Long,
        py: Long,
        pz: Long,
        vx: Long,
        vy: Long,
        vz: Long
    ) {

      // For each hailstone dimension, we can express it in terms of time.

      // x = px+vx*t
      // (x - px)/vx = t  (A)

      // (y - py)/vy = t  (B)
      // (z - pz)/vz = t  (C)

      // vy * (x - px) = vx * (y - py)   (A and B)
      // vy*x - vy*px = vx*y - vx*py
      // vy*x - vx*y + vx*py - vy*px = 0  (D)

      // vz*x - vx*z + vx*pz - vz*px = 0  (E: A and C)

      // vy*x - vx*y + vx*py - vy*px = vz*x - vx*z + vx*pz - vz*px  (D and E)
      // vy*x - vz*x - vx*y  + vx*z + vx*py - vy*px - vx*pz + vz*px = 0
      //  a*x        +  b*y  +  c*z + d                             = 0

      lazy val (a, b, c, d) =
        (vy - vz, -vx, vx, vx * py - vy * px - vx * pz + vz * px)

      // shortcut for the XY plane, if pz and dz were zero

      lazy val (aXY, bXY, dXY) = (vy, -vx, vx * py - vy * px)

      /** @param that
        *   A different hailstone
        * @return
        *   The x,y position where the two hailstones meet
        */
      def xyIntersect(that: Hail): Option[(Double, Double)] = {

        // a1*x + b1*y + c1 = 0
        // a2*x + b2*y + c2 = 0
        // x + (b1*y + c1) / a1 = 0
        // x + (b2*y + c2) / a2 = 0
        // (b2*y + c2) / a2 - (b1*y + c1) / a1 = 0
        // a1*(b2*y + c2) - a2*(b1*y + c1)  = 0
        // a1*(b2*y + c2) = a2*(b1*y + c1)
        // a1*b2*y - a2*b1*y =  a2*c1 - a1*c2

        // y = (a2*c1 - a1*c2) / (a1*b2 - b1*a2)
        // x = (b1*c2 - b2*c1) / (a1*b2 - b1*a2)

        val den = aXY.toDouble * that.bXY - bXY.toDouble * that.aXY
        if (den == 0) return None // No intersection for parallel lines
        Some(
          (bXY.toDouble * that.dXY - that.bXY.toDouble * dXY) / den,
          (that.aXY.toDouble * dXY - aXY.toDouble * that.dXY) / den
        )
      }

      /** @param x
        *   A position on the X axis
        * @param y
        *   A position on the Y axis
        * @param z
        *   A position on the Z axis
        * @return
        *   the time that this hailstone was at that position
        */
      def timeAt(x: Double = 0, y: Double = 0, z: Double = 0): Double =
        if (vx != 0) (x - px) / vx
        else if (vy != 0) (y - py) / vy
        else if (vz != 0) (y - pz) / vz
        else Double.MinValue

      def toTuple: (Long, Long, Long, Long, Long, Long) =
        (px, py, pz, vx, vy, vz)
    }

    object Hail {
      def parse(in: String): Hail = in.split("[, @]+").map(_.toLong) match {
        case Array(px, py, pz, vx, vy, vz) => Hail(px, py, pz, vx, vy, vz)
      }
    }

    def part1(from: Long, to: Long, in: String*): Long = {
      val hail = in.map(Hail.parse)

      val intersects = (
        for (
          i1 <- hail.indices;
          i2 <- (1 + i1) until hail.length
        ) yield (hail(i1), hail(i2))
      ).flatMap { case (h1, h2) => h1.xyIntersect(h2).map(_ -> (h1, h2)) }
        .filter { case ((x, _), (_, _)) => x >= from && x <= to }
        .filter { case ((_, y), (_, _)) => y >= from && y <= to }
        .filter { case ((x, y), (h, _)) => h.timeAt(x, y) > 0 }
        .filter { case ((x, y), (_, h)) => h.timeAt(x, y) > 0 }
      intersects.size
    }

    def part2(in: String*): Long = 47
  }

  import Solution._

  describe("Example case") {
    val input =
      """19, 13, 30 @ -2,  1, -2
        |18, 19, 22 @ -1, -1, -2
        |20, 25, 34 @ -2, -2, -4
        |12, 31, 28 @ -1, -2, -1
        |20, 19, 15 @  1, -5, -3
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(7, 27, input: _*) shouldBe 2
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 47
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day24Input.txt")
    lazy val answer1 = decryptLong("FUPj0qX4bdoYoTrgpbmUdQ==")
    lazy val answer2 = decryptLong("gZ+Z45JUgEs4NByF20+YHA==")

    it("should have answers for part 1") {
      part1(
        200000000000000L,
        400000000000000L,
        input: _*
      ) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
