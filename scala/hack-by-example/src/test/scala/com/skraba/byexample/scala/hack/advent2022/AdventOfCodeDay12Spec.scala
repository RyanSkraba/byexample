package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 12 Solutions in scala=
  *
  * Input: A height map where each lower-case character corresponds to a height,
  * and a start and end point marked with 'S' and 'E' at heights 'a' and 'z'
  * respectively.
  *
  * Part 1: Find the length of the shortest path from 'S' to 'E'
  *
  * Part 2: Find the length of the any path from 'a' to 'E'
  *
  * @see
  *   Rephrased from https://adventofcode.com/2022/day/12
  */
class AdventOfCodeDay12Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** A string containing all of the character height as rows concatenated. */
    case class Terrain(
        map: String,
        h: Int,
        w: Int,
        src: Int,
        dst: Int
    ) {

      def part1(): Long = bfs(true)(src)

      def part2(): Long = {
        val dx = bfs(false)
        (for (i <- map.indices if map(i) == 'a')
          yield dx(i)).min
      }

      def bfs(stopAtSource: Boolean = true): Array[Int] = {
        // The distances that have been calculated from the end
        val dx: Array[Int] = Array.fill(map.length)(Int.MaxValue)
        // The positions to search next (this can contain invalid values)
        val bfs = scala.collection.mutable.Queue((dst, dst))

        while (bfs.nonEmpty) {
          // Get the next position to calculate and the position we're coming from
          val (pos, prev) = bfs.dequeue()
          // If it's out of bounds or not in the same row or column as the previous, just ignore it.
          if (
            pos >= 0 && pos < map.length && (prev % w == pos % w || prev / w == pos / w)
          ) {
            // If it's not accessible from the previous position, just ignore it.
            if (map(prev) - map(pos) <= 1) {
              // If the previous position was already calculated, then it can't be any closer to the end, since this is a breadth first search, so ignore it.
              if (dx(pos) == Int.MaxValue) {
                // Set the position as one farther than the previous (or zero at the destination).
                dx(pos) = if (dst == pos) 0 else dx(prev) + 1
                // If we've found the source, great, we know the number of steps
                if (stopAtSource && pos == src) return dx
                // Try looking at the north, south, east and west now.
                bfs.enqueue(
                  (pos - w, pos),
                  (pos + w, pos),
                  (pos - 1, pos),
                  (pos + 1, pos)
                )
              }
            }
          }
        }

        // Return all of the distances found
        dx
      }
    }

    object Terrain {
      def apply(in: String*): Terrain = {
        val map = in.mkString
        val src = map.indexOf('S')
        val dst = map.indexOf('E')
        Terrain(
          map.updated(src, 'a').updated(dst, 'z'),
          in.length,
          in.headOption.map(_.length).getOrElse(0),
          src,
          dst
        )
      }
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Sabqponm
        |abcryxxl
        |accszExk
        |acctuvwj
        |abdefghi
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description") {
      val map = Terrain(input: _*)
      map.src shouldBe 0
      map.dst shouldBe 21

      map.part1() shouldBe 31
      map.part2() shouldBe 29
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day12Input.txt").filter(_.nonEmpty)
    it("should have answers") {
      val map = Terrain(input: _*)
      map.src shouldBe 2860
      map.dst shouldBe 2980
      map.w shouldBe 143
      map.h shouldBe 41

      map.part1() shouldBe 462
      map.part2() shouldBe 451
    }
  }
}
