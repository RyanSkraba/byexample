package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec
import scala.collection.mutable

/** =Advent of Code 2024 Day 18 Solutions in scala=
  *
  * Input: A list of coordinates in the form x,y corresponding to a position on a grid that can't be traversed.
  *
  * Part 1: Taking the first N coordinates in the list, find the minimum cost to traverse a grid the grid from upper
  * left to lower right.
  *
  * Part 2: When adding the coordinates one by one, find the first coordinate such that a path no longer exists from
  * upper left to lower right.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/18]]
  */
class AdventOfCodeDay18Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def part1(size: Int, dx: Int, in: String*): Long = {
      val corrupt = in.take(size).map(_.split(',')).map { case Array(x, y) => y.toInt * dx + x.toInt }.toSet

      // The memo contains the minimum cost to reach the given position (so far).
      val memoCost: mutable.Map[Int, Long] = mutable.Map.empty
      // The queue contains the current path, the head position on the path and the current cost.
      val queue = mutable.Queue(Set(0) -> 0)

      while (queue.nonEmpty) {
        val (path, current) = queue.dequeue()

        val cost = path.size
        if (current == dx * dx - 1) {
          // The first solution we find will also be the minimum cost.
          return cost - 1
        } else if (cost < memoCost.getOrElse(current, Long.MaxValue)) {
          // It's not a solution, but we've either never been here before or it's lower than the previous visit
          memoCost(current) = cost
          val next = Seq(
            if (current % dx == dx - 1) 0 else 1,
            if (current / dx == dx - 1) 0 else dx,
            if (current % dx == 0) 0 else -1,
            if (current / dx == 0) 0 else -dx
          ).filterNot(_ == 0)
            .map(_ + current)
            .filterNot(path)
            .filterNot(corrupt)
          // Add all the next possible steps to the queue.  They'll be trimmed if they're more costly.
          queue.enqueueAll(next.map(c => path + c -> c))
        }
      }
      Long.MaxValue
    }

    def part2(minSuccess: Int, dx: Int, in: String*): String = {
      @tailrec
      def binarySearch(success: Int, failure: Int): Int = {
        if (failure == success + 1) return failure
        val mid = (success + failure) / 2
        if (part1(mid, dx, in: _*) == Long.MaxValue) binarySearch(success, mid)
        else binarySearch(mid, failure)
      }
      in(binarySearch(minSuccess, in.length) - 1)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """5,4
        |4,2
        |4,5
        |3,0
        |2,1
        |6,3
        |2,4
        |1,5
        |0,6
        |3,3
        |2,6
        |5,1
        |1,2
        |5,5
        |2,5
        |6,5
        |1,4
        |0,4
        |6,4
        |1,1
        |6,1
        |1,0
        |0,5
        |1,6
        |2,0
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(12, 7, input: _*) shouldBe 22
    }

    it("should match the puzzle description for part 2") {
      part2(12, 7, input: _*) shouldBe "6,1"
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day18Input.txt")
    lazy val answer1 = decryptLong("2gaOqI36WxeUceFs0JhSHA==")
    lazy val answer2 = decrypt("zkxYC47Xf5uMu0PG25waiQ==")

    it("should have answers for part 1") {
      part1(1024, 71, input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(1024, 71, input: _*) shouldBe answer2
    }
  }
}
