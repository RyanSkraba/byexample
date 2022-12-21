package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** =Advent of Code 2022 Day 20 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/20]]
  */
class AdventOfCodeDay20Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    def mix(file: Seq[(Long, Int)], indexToMix: Int): Seq[(Long, Int)] = {
      val (left, element :: right) = file.span(_._2 != indexToMix)
      val moveTo = (left.size + element._1) % (file.size - 1)
      (left ++ right).patch(
        (if (moveTo < 0) moveTo + file.size - 1 else moveTo).toInt,
        Seq(element),
        0
      )
    }

    def part1(in: Seq[Long]): Long = {
      // The input does not have unique values, so include the original index while mixing
      val withIndex = in.zipWithIndex
      val mixed: Seq[(Long, Int)] = in.indices.foldLeft(withIndex)(mix)

      // Find the zero, and sum the 1000th, 2000th, 3000th character after it
      val zeroIndex = mixed.indexWhere(_._1 == 0)
      val a = mixed((zeroIndex + 1000) % in.size)._1
      val b = mixed((zeroIndex + 2000) % in.size)._1
      val c = mixed((zeroIndex + 3000) % in.size)._1
      a + b + c
    }

    def part2(in: Seq[Long]): Long = {
      val withIndex = in.map(_*811589153L).zipWithIndex
      val mixed = Stream.iterate(withIndex)(in.indices.foldLeft(_)(mix))(10)

      val zeroIndex = mixed.indexWhere(_._1 == 0)
      val a = mixed((zeroIndex + 1000) % in.size)._1
      val b = mixed((zeroIndex + 2000) % in.size)._1
      val c = mixed((zeroIndex + 3000) % in.size)._1
      a + b + c
    }
  }

  import Solution._

  describe("Example case") {
    val input = Seq(1L, 2, -3, 3, -2, 0, 4)

    it("should mix the given examples") {
      mix(Seq(4L, 5, 6, 1, 7, 8, 9).zipWithIndex, 3).map(_._1) shouldBe Seq(4, 5,
        6, 7, 1, 8, 9)
      mix(Seq(4L, -2, 5, 6, 7, 8, 9).zipWithIndex, 1).map(_._1) shouldBe Seq(4,
        5, 6, 7, 8, -2, 9)
    }

    it("should mix moving one from the front or back") {
      mix(Seq(-1L, -22, -33, -44).zipWithIndex, 0)
        .map(_._1) shouldBe Seq(-22, -33, -1, -44)
      mix(Seq(44L, 33, 22, 1).zipWithIndex, 3)
        .map(_._1) shouldBe Seq(44, 1, 33, 22)
    }

    it("should not change any zero positions") {
      mix(Seq(0L, 11, 22, 33).zipWithIndex, 0)
        .map(_._1) shouldBe Seq(0, 11, 22, 33)
      mix(Seq(11L, 0, 22, 33).zipWithIndex, 1)
        .map(_._1) shouldBe Seq(11, 0, 22, 33)
      mix(Seq(11L, 22, 0, 33).zipWithIndex, 2)
        .map(_._1) shouldBe Seq(11, 22, 0, 33)
      // Note that the implementation is allowed to rotate
      mix(Seq(11L, 22, 33, 0).zipWithIndex, 3)
        .map(_._1) shouldBe Seq(0, 11, 22, 33)
    }

    it("should mix wrapping around correctly") {
      mix(Seq(4L, 11, 22, 33).zipWithIndex, 0)
        .map(_._1) shouldBe Seq(11, 4, 22, 33)
      mix(Seq(7L, 11, 22, 33).zipWithIndex, 0)
        .map(_._1) shouldBe Seq(11, 7, 22, 33)
      mix(Seq(10L, 11, 22, 33).zipWithIndex, 0)
        .map(_._1) shouldBe Seq(11, 10, 22, 33)
    }

    it("should mix wrapping around but not moving anything") {
      mix(Seq(11L, 3, 22, 33).zipWithIndex, 1)
        .map(_._1) shouldBe Seq(11, 3, 22, 33)
      mix(Seq(11L, -3, 22, 33).zipWithIndex, 1)
        .map(_._1) shouldBe Seq(11, -3, 22, 33)
      mix(Seq(11L, -6, 22, 33).zipWithIndex, 1)
        .map(_._1) shouldBe Seq(11, -6, 22, 33)
      mix(Seq(11L, 33, 22, 33).zipWithIndex, 1)
        .map(_._1) shouldBe Seq(11, 33, 22, 33)
      mix(Seq(11L, -33, 22, 33).zipWithIndex, 1)
        .map(_._1) shouldBe Seq(11, -33, 22, 33)
    }

    it("should match the puzzle description") {
      part1(input) shouldBe 3
      part2(input) shouldBe 1623178306L
    }
  }

  describe("Solution") {
    val input =
      puzzleInput("Day20Input.txt").filter(_.nonEmpty).map(_.toLong).toList

    it("should have answers (4 seconds)", Slow) {
      part1(input) shouldBe 7278
      part2(input) shouldBe 14375678667089L
    }
  }
}
