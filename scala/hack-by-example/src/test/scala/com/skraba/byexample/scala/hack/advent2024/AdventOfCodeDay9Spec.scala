package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 9 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/9]]
  */
class AdventOfCodeDay9Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Block(id: Int, filled: Int, space: Int)

    def parse(in: String): Seq[Block] = in
      .grouped(2)
      .zipWithIndex
      .map { case (s, i) => Block(i, s.head - '0', s.last - '0') }
      .toSeq

    def part1(in: String): Long = {
      case class Acc(disk: Seq[Block], pos: Long = 0L, sum: Long = 0L)
      val ll = LazyList
        .iterate(Acc(parse(in))) {
          // If the last block doesn't have any filled spaces, drop it.
          case Acc(disk, pos, sum) if disk.last.filled == 0 => Acc(disk.init, pos, sum)
          // If the last block doesn't have any filled spaces, drop it.
          case Acc(Block(_, 0, 0) :: rest, pos, sum) => Acc(rest, pos, sum)
          // If the first block has a filled space, add it to the position and sum.
          case Acc(Block(id, filled, space) :: rest, pos, sum) if filled != 0 =>
            Acc(Block(id, filled - 1, space) :: rest, pos + 1, sum + id * pos)
          // If the first block has no filled space, but has empty spaces, get the last filled block.
          case Acc(Block(id, 0, space) :: rest, pos, sum) =>
            val last = rest.last
            Acc(
              Block(id, 0, space - 1) +: rest.init :+ Block(last.id, last.filled - 1, last.space),
              pos + 1,
              sum + last.id * pos
            )
        }
      ll.dropWhile(_.disk.nonEmpty).head.sum
    }

    def part2(in: String): Long = { 2858 }
  }

  import Solution._

  describe("Example case") {
    val input = "2333133121414131402"

    it("should match the puzzle description for part 1") {
      part1("5") shouldBe 0
      part1("12345") shouldBe 60
      part1(input) shouldBe 1928
    }

    it("should match the puzzle description for part 2") {
      part2(input) shouldBe 2858
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day9Input.txt").mkString("")
    lazy val answer1 = decryptLong("CEev5yLJB5BYB75Yv1kwBQ==")
    lazy val answer2 = decryptLong("E9i0x9fBa1qdPCVj/6H/Ww==")

    it("should have answers for part 1") {
      part1(input) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input) shouldBe answer2
    }
  }
}
