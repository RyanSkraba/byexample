package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 24 Solutions in scala=
  *
  * Input: Many hailstone trajectories with a position in 3D space and a velocity vector.
  *
  * Part 1: Ignoring the Z axis, find the number of hailstone trajectories that intersect in a given square. The
  * intersection doesn't need to be at the same time, but it needs to happen after time 0.
  *
  * Part 2: Given all the trajectories, find one more trajectory that intersects every hailstone at some point. Add the
  * coordinates of the starting point of that trajectory.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/24]]
  */
class AdventOfCodeDay24Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Hail(px: Long, py: Long, pz: Long, vx: Long, vy: Long, vz: Long) {

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

      lazy val (a, b, c, d) = (vy - vz, -vx, vx, vx * py - vy * px - vx * pz + vz * px)

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

      def toTuple: (Long, Long, Long, Long, Long, Long) = (px, py, pz, vx, vy, vz)
    }

    object Hail {
      def parse(in: String): Hail = in.split("[, @]+").map(_.toLong) match {
        case Array(px, py, pz, vx, vy, vz) => Hail(px, py, pz, vx, vy, vz)
      }
    }

    def part1(from: Long, to: Long, in: String*): Long = {
      val hail = in.map(Hail.parse)

      val intersects = (
        for (i1 <- hail.indices; i2 <- (1 + i1) until hail.length) yield (hail(i1), hail(i2))
      ).flatMap { case (h1, h2) => h1.xyIntersect(h2).map(_ -> (h1, h2)) }
        .filter { case ((x, _), (_, _)) => x >= from && x <= to }
        .filter { case ((_, y), (_, _)) => y >= from && y <= to }
        .filter { case ((x, y), (h, _)) => h.timeAt(x, y) > 0 }
        .filter { case ((x, y), (_, h)) => h.timeAt(x, y) > 0 }
      intersects.size
    }

    def matrixReduce(in: Seq[Seq[Double]]): Seq[Seq[Double]] = {
      // An ordering that attempts to put a non-zero value at the
      // diagonal while the matrix is being reduced.
      val byCol: Ordering[Seq[Double]] = Ordering.by { in =>
        val row = in.map(math.abs)
        (row.head, row(1), row(2), row(3), row(4), row(5))
      }

      // Solve for a diagonal of 1s from 0,0 to 5,5
      in.indices.foldLeft(in) { (acc, toReduce) =>
        val m = acc.sorted(byCol).reverse

        val reducedRow = m(toReduce).map(_ / m(toReduce)(toReduce))
        val reduced = for (row <- m.indices) yield {
          if (toReduce == row) reducedRow
          else {
            val multiplier = m(row)(toReduce) / reducedRow(toReduce)
            m(row).zip(reducedRow).map(x => x._1 - multiplier * x._2)
          }
        }
        reduced
      }
    }

    def part2(in: String*): Long = {
      val hail = in.map(Hail.parse)

      // We want to find a rock with a trajectory (6 unknowns)
      // rx, ry, rz, vrx, vry, vrz

      // We know:
      // x = rx + vrx*t
      // y = ry + vry*t
      // z = rz + vrz*t

      // And at a certain t0 it will collide with hailstone 0
      // x = rx + vrx*t0 = p0x + v0x*t0
      // y = ry + vry*t0 = p0y + v0y*t0
      // z = rz + vrz*t0 = p0z + v0z*t0

      // Solving for t0:
      // t0 = (rx - p0x) / (v0x - vrx)   [A]
      // t0 = (ry - p0y) / (v0y - vry)   [B]
      // t0 = (rz - p0z) / (v0z - vrz)   [C]

      // Combining these gives us three equations:
      // (rx - p0x) / (v0x - vrx) = (ry - p0y) / (v0y - vry)  [D: A+B}
      // (rx - p0x) / (v0x - vrx) = (rz - p0z) / (v0z - vrz)  [E: A+C}
      // (ry - p0y) / (v0y - vry) = (rz - p0z) / (v0z - vrz)  [F: B+C}

      // These can be expanded out to three equations (still not linear, non-linear unknowns on the left):
      // v0y*rx - p0x*v0y - rx*vry + p0x*vry = v0x*ry - p0y*v0x - ry*vrx + p0y*vrx
      // ry*vrx - rx*vry = v0x*ry - p0y*v0x  + p0y*vrx - v0y*rx + p0x*v0y - p0x*vry [G from D]
      // rz*vrx - rx*vrz = v0x*rz - p0z*v0x  + p0z*vrx - v0z*rx + p0x*v0z - p0x*vrz [H from E]
      // ry*vrz - rz*vry = v0z*ry - p0y*v0z  + p0y*vrz - v0y*rz + p0z*v0y - p0z*vry [I from F]

      // Every constant with a digit is known, and we can find more easily:
      // ry*vrx - rx*vry = v1x*ry - p1y*v1x  + p1y*vrx - v1y*rx + p1x*v1y - p1x*vry [J]
      // rz*vrx - rx*vrz = v1x*rz - p1z*v1x  + p1z*vrx - v1z*rx + p1x*v1z - p1x*vrz [K]
      // ry*vrz - rz*vry = v1z*ry - p1y*v1z  + p1y*vrz - v1y*rz + p1z*v1y - p1z*vry [L]

      // So making them linear:
      // v0x*ry - p0y*v0x  + p0y*vrx - v0y*rx + p0x*v0y - p0x*vry = v1x*ry - p1y*v1x  + p1y*vrx - v1y*rx + p1x*v1y - p1x*vry [G and J]
      // v0x*rz - p0z*v0x  + p0z*vrx - v0z*rx + p0x*v0z - p0x*vrz = v1x*rz - p1z*v1x  + p1z*vrx - v1z*rx + p1x*v1z - p1x*vrz [H and K]
      // v0z*ry - p0y*v0z  + p0y*vrz - v0y*rz + p0z*v0y - p0z*vry = v1z*ry - p1y*v1z  + p1y*vrz - v1y*rz + p1z*v1y - p1z*vry [I and L]

      //           rx             ry             rz             vrx             vry             vrz = Constant
      // (v1y-v0y)*rx + (v0x-v1x)*ry                + (p0y-p1y)*vrx + (p1x-p0x)*vry                 = p0y*v0x - p0x*v0y + p1x*v1y - p1y*v1x
      // (v1z-v0z)*rx                + (v0x-v1x)*rz + (p0z-p1z)*vrx                 + (p1x-p0x)*vrz = p0z*v0x - p0x*v0z + p1x*v1z - p1z*v1x
      //                (v0z-v1z)*ry + (v1y-v0y)*rz +               + (p1z-p0z)*vry + (p0y-p1y)*vrz = p0y*v0z - p0z*v0y + p1z*v1y - p1y*v1z

      // For any two points, this returns the above matrix.
      def matrix3(h0: Hail, h1: Hail): Seq[Seq[Double]] = {
        val (p0x, p0y, p0z, v0x, v0y, v0z) = h0.toTuple
        val (p1x, p1y, p1z, v1x, v1y, v1z) = h1.toTuple
        Seq(
          Seq(v1y - v0y, v0x - v1x, 0, p0y - p1y, p1x - p0x, 0, p0y * v0x - p0x * v0y + p1x * v1y - p1y * v1x),
          Seq(v1z - v0z, 0, v0x - v1x, p0z - p1z, 0, p1x - p0x, p0z * v0x - p0x * v0z + p1x * v1z - p1z * v1x),
          Seq(0, v0z - v1z, v1y - v0y, 0, p1z - p0z, p0y - p1y, p0y * v0z - p0z * v0y + p1z * v1y - p1y * v1z)
        ).map(_.map(_.toDouble))
      }

      // Reduce the matrix using any three points.
      val solved = matrixReduce(matrix3(hail.head, hail(1)) ++ matrix3(hail.head, hail(2)))

      solved.map(_.last).map(math.round).take(3).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """19, 13, 30 @ -2,  1, -2
        |18, 19, 22 @ -1, -1, -2
        |20, 25, 34 @ -2, -2, -4
        |12, 31, 28 @ -1, -2, -1
        |20, 19, 15 @  1, -5, -3
        |""".trimSplit

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
    lazy val answer2 = decryptLong("b1SEz1TocSBtGc37FTnMEg==")

    it("should have answers for part 1") {
      part1(200000000000000L, 400000000000000L, input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
