package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2024 Day 12 Solutions in scala=
  *
  * Input: A 2D garden plot where each cell has a vegetable represented by a character. A region is a contiguous group
  * of the same vegetable.
  *
  * Part 1: Find the cost of all fences around all regions if each region's fence price is the region's area multiplied
  * by the region's perimeter (internal and external).
  *
  * Part 1: Find the cost of all fences around all regions if each region's fence price is the region's area multiplied
  * by the number of straight sides in the region's shape (internal and external).
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/12]]
  */
class AdventOfCodeDay12Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** UnionFind starts with N disjoint sets each containing an id, and calling [[union()]] joins sets together. */
    class UnionFind(var count: Int) {

      /** A tree of IDs, where an ID points to itself if it is the root, or to another if it is joined to it. */
      val id: Array[Int] = (0 until count).toArray

      /** The number of objects in the tree rooted at N. */
      val sz: Array[Int] = Array.fill(count)(1)

      /** Return the root component identifier for N. */
      def find(n: Int): Int = LazyList.iterate(n)(id).find(x => x == id(x)).get

      /** Return true if the two numbers are in the same component. */
      def connected(n1: Int, n2: Int): Boolean = find(n1) == find(n2)

      /** Join the two components together. */
      def union(n1: Int, n2: Int): Unit = {
        val c1 = find(n1)
        val c2 = find(n2)
        if (c1 == c2) return
        // Make the smaller root point to the larger one.
        if (sz(c1) < sz(c2)) { id(c1) = c2; sz(c2) += sz(c1); sz(c1) = 0 }
        else { id(c2) = c1; sz(c1) += sz(c2); sz(c2) = 0 }
        count -= 1
      }
    }

    def part1(in: String*): Long = {

      val dx = in.head.length
      val uf = new UnionFind(in.length * dx)

      // For a root component in the UnionFind, this stores the perimeter.
      val perim: Array[Int] = Array.fill(uf.count)(0)

      for (y <- in.indices; x <- in(y).indices; plot = y * dx + x; veg = in(y)(x)) {
        // Vegetables to the north and west.
        val nVeg = if (y == 0) '+' else in(y - 1)(x)
        val wVeg = if (x == 0 || plot % dx == 0) '+' else in(y)(x - 1)

        if (veg != nVeg && veg != wVeg) {
          // If this is a new vegetable, it starts with a perimeter of 4
          perim(plot) = 4
        } else if (veg != nVeg) {
          // If it's ONLY connected to the west, update the UF and add 2 to the perimeter.
          uf.union(plot - 1, plot)
          perim(uf.find(plot)) += 2
        } else if (veg != wVeg) {
          // Likewise if the north is it's only connection.
          uf.union(plot - dx, plot)
          perim(uf.find(plot)) += 2
        } else if (uf.connected(plot - dx, plot - 1)) {
          // If it's connected to both, update the UF and the perimeter doesn't change.
          uf.union(plot - dx, plot)
        } else {
          // If it's connected to both, but THEY  aren't connected, then the new perimeter is the sum of the two.
          perim(plot) = perim(uf.find(plot - dx)) + perim(uf.find(plot - 1))
          uf.union(plot - dx, plot - 1)
          uf.union(plot - dx, plot)
          perim(uf.find(plot)) = perim(plot)
        }
      }

      (for (plot <- uf.sz.indices if uf.sz(plot) != 0) yield uf.sz(plot) * perim(plot)).sum
    }

    def part2(in: String*): Long = ???
  }

  import Solution._

  describe("Example case") {
    val input1 =
      """AAAA
        |BBCD
        |BBCC
        |EEEC
        |""".trim.stripMargin.split("\n")

    val input2 =
      """OOOOO
        |OXOXO
        |OOOOO
        |OXOXO
        |OOOOO
        |""".trim.stripMargin.split("\n")

    val input =
      """RRRRIICCFF
        |RRRRIICCCF
        |VVRRRCCFFF
        |VVRCCCJFFF
        |VVVVCJJCFE
        |VVIVCCJJEE
        |VVIIICJJEE
        |MIIIIIJJEE
        |MIIISIJEEE
        |MMMISSJEEE""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1 mini") {
      part1(input1: _*) shouldBe 140
      part1(input2: _*) shouldBe 772
      part1("""AABB
              |ABBA
              |AAAA
              |""".trim.stripMargin.split("\n"): _*) shouldBe 184
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 1930
    }

    ignore("should match the puzzle description for part 2 mini") {
      part2(input1: _*) shouldBe 80
      part2("""EEEEE
              |EXXXX
              |EEEEE
              |EXXXX
              |EEEEE
              |""".trim.stripMargin.split("\n"): _*) shouldBe 236
      part2("""AAAAAA
              |AAABBA
              |AAABBA
              |ABBAAA
              |ABBAAA
              |AAAAAA
              |""".trim.stripMargin.split("\n"): _*) shouldBe 368
    }

    ignore("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 1206
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day12Input.txt")
    lazy val answer1 = decryptLong("jtlTzHmU2K6Xzq+KaBe7Mw==")
    lazy val answer2 = decryptLong("U9BZNCixKWAgOXNrGyDe5A==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    ignore("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
