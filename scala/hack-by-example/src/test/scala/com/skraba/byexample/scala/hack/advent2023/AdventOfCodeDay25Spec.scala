package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.collection.mutable
import scala.util.Random

/** =Advent of Code 2023 Day 25 Solutions in scala=
  *
  * Input: A connected graph of nodes (undirected edges) that can be cut into two by removing three edges.
  *
  * Part 1: After cutting the graph in two, the product of the sizes of each disjoint component.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/25]]
  */
class AdventOfCodeDay25Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Graph[T](v: Set[T], e: Seq[(T, T)]) {

      /** Karger's algorithm for a minimum cut. This is randomized and may need to be run several times to find the
        * optimal minimum.
        *
        * @see
        *   [[https://en.wikipedia.org/wiki/Karger%27s_algorithm]]
        * @return
        *   the vertices on the left then right of the graph, along with the edges connecting them that makes a minimum
        *   cut.
        */
      def karger(): (Set[T], Set[T], Seq[(T, T)]) = {
        val rnd = new Random()

        // Start with each vertex in its own vertex set, and a complete list of edges.
        val vSets: mutable.Set[mutable.Set[T]] =
          mutable.Set.from(v.map(mutable.Set(_)))
        var remaining: Seq[(T, T)] = e

        // Continue to collapse nodes on randomly selected edges until only 2 vertex sets remain
        while (vSets.size > 2) {
          // Pick an edge to collapse
          val edge = remaining(rnd.nextInt(remaining.length))

          val vs1 = vSets.find(_.contains(edge._1)).get
          val vs2 = vSets.find(_.contains(edge._2)).get
          val vsCollapsed = vs1 ++ vs2
          vSets.remove(vs1)
          vSets.remove(vs2)
          vSets.add(vsCollapsed)

          remaining = remaining.filterNot(e => vsCollapsed.contains(e._1) && vsCollapsed.contains(e._2))
        }

        (vSets.head.toSet, vSets.last.toSet, remaining)
      }
    }
    object Graph {
      def from(in: String*): Graph[String] = {
        val edges: Seq[(String, String)] = in
          .map(_.split("[: ]+").toList)
          .flatMap(_ match {
            case src :: dsts => dsts.map(dst => dst -> src)
          })
        val vertices = edges.map(_._1).toSet ++ edges.map(_._2).toSet
        Graph(vertices, edges)
      }
    }

    def part1(in: String*): Long = {
      val g = Graph.from(in: _*)

      val (left, right, edges) =
        LazyList.continually(g.karger()).dropWhile(_._3.size != 3).head

      left.size * right.size
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """jqt: rhn xhk nvd
        |rsh: frs pzl lsr
        |xhk: hfx
        |cmg: qnr nvd lhk bvb
        |rhn: xhk bvb hfx
        |bvb: xhk hfx
        |pzl: lsr hfx nvd
        |qnr: nvd
        |ntq: jqt hfx bvb xhk
        |nvd: lhk
        |lsr: lhk
        |rzs: qnr cmg lsr rsh
        |frs: qnr lhk lsr
        |""".trim.stripMargin.split("\n")

    it("should create a graph") {
      val g = Graph.from(input: _*)
      g.v should have size 15
      g.e should have size 33
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 54
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day25Input.txt")
    lazy val answer1 = decryptLong("tngkDUZCABasAAD/+wr3ow==")

    it("should have answers for part 1 (10-90 seconds)", Slow) {
      // This could probably be improved by using an algorithm that finds the cut
      // deterministically.
      part1(input: _*) shouldBe answer1
    }
  }
}
