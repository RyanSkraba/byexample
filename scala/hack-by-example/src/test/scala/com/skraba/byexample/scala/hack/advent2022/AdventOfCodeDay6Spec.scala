package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** =Advent of Code 2022 Day 6 Solutions in scala=
  *
  * Input: One string as a message with letters
  *
  * Part 1: The number of letters that you have to read before finding 4
  * consecutive different letters
  *
  * Part 2: The number of letters that you have to read before finding 14
  * consecutive different letters
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/6]]
  */
class AdventOfCodeDay6Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {
    def findStartOfPacket(in: String): Long =
      in.sliding(4).indexWhere(_.toSet.size == 4) + 4

    def findStartOfMessage(in: String): Long =
      in.sliding(14).indexWhere(_.toSet.size == 14) + 14
  }

  import Solution._

  describe("Example case") {
    it("should match the puzzle description") {
      findStartOfPacket("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 7
      findStartOfPacket("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 5
      findStartOfPacket("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 6
      findStartOfPacket("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 10
      findStartOfPacket("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 11
      findStartOfMessage("mjqjpqmgbljsphdztnvjfqwrcgsmlb") shouldBe 19
      findStartOfMessage("bvwbjplbgvbhsrlpgdmjqwftvncz") shouldBe 23
      findStartOfMessage("nppdvjthqldpwncqszvftbrmjlhg") shouldBe 23
      findStartOfMessage("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg") shouldBe 29
      findStartOfMessage("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw") shouldBe 26
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day6Input.txt")
    it("should have answers") {
      findStartOfPacket(input.head) shouldBe 1582
      findStartOfMessage(input.head) shouldBe 3588
    }
  }
}
