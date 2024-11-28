package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2022 Day 18 Solutions in scala=
  *
  * Input: A sequence of coordinates that describe a lava droplet in the air (as a series of unit sized blocks)
  *
  * Part 1: The total number of square surfaces that the lava drop has exposed to air (including internal pockets).
  *
  * Part 2: The total number of square surfaces that the lava drop has exposed to air (excluding internal pockets).
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/18]]
  */
class AdventOfCodeDay18Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Pos(x: Int, y: Int, z: Int) {
      def neighbours: Seq[Pos] = Seq(
        Pos(x + 1, y, z),
        Pos(x - 1, y, z),
        Pos(x, y + 1, z),
        Pos(x, y - 1, z),
        Pos(x, y, z + 1),
        Pos(x, y, z - 1)
      )

      def within(bMin: Pos, bMax: Pos): Boolean =
        x >= bMin.x && x <= bMax.x && y >= bMin.y && y <= bMax.y && z >= bMin.z && z <= bMax.z
    }

    object Pos {
      def parse(in: String*): Seq[Pos] = in.map(_.split(',') match {
        case Array(x, y, z) => Pos(x.toInt, y.toInt, z.toInt)
      })
    }

    def part1(ds: Seq[Pos]): Long = {
      val dset = ds.toSet
      val neighbours = ds.map(_.neighbours.count(dset))
      // There are 6 surfaces per droplet, removing one for every neighbour
      dset.size * 6 - neighbours.sum
    }

    def part2(ds: Seq[Pos]): Long = {

      // Find the bounds of the droplets with a margin of 1
      val bMin = Pos(ds.minBy(_.x).x - 1, ds.minBy(_.y).y - 1, ds.minBy(_.z).z - 1)
      val bMax = Pos(ds.maxBy(_.x).x + 1, ds.maxBy(_.y).y + 1, ds.maxBy(_.z).z + 1)

      // Flood fill an external block defined by the bounds, excluding the droplet
      val inside = ds.toSet
      val outside = {
        val filled = mutable.Set[Pos](bMin)
        val next = mutable.Queue[Pos](bMin)

        while (next.nonEmpty) {
          val pos = next.dequeue()
          for (n <- pos.neighbours.filterNot(filled).filterNot(inside) if n.within(bMin, bMax)) {
            filled += n
            next.enqueue(n)
          }
        }

        filled.toSeq
      }

      // The total surface of the external block, minus the rectangular outsides
      part1(outside) -
        2 * (bMax.x - bMin.x + 1) * (bMax.y - bMin.y + 1) -
        2 * (bMax.x - bMin.x + 1) * (bMax.z - bMin.z + 1) -
        2 * (bMax.y - bMin.y + 1) * (bMax.z - bMin.z + 1)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """2,2,2
        |1,2,2
        |3,2,2
        |2,1,2
        |2,3,2
        |2,2,1
        |2,2,3
        |2,2,4
        |2,2,6
        |1,2,5
        |3,2,5
        |2,1,5
        |2,3,5
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description") {
      part1(Pos.parse("1,1,1", "2,1,1")) shouldBe 10
      part2(Pos.parse("1,1,1", "2,1,1")) shouldBe 10
      part1(Pos.parse(input: _*)) shouldBe 64
      part2(Pos.parse(input: _*)) shouldBe 58
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day18Input.txt").filter(_.nonEmpty)
    it("should have answers") {
      part1(Pos.parse(input: _*)) shouldBe decryptLong("NtUlN2GRC0bYWH1u8RnGNg==")
      part2(Pos.parse(input: _*)) shouldBe decryptLong("+vUA7F8j3jy4TtCQBNEKfQ==")
    }
  }
}
