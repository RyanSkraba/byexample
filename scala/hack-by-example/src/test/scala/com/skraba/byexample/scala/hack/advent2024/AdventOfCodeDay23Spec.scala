package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2024 Day 23 Solutions in scala=
  *
  * Input: A list of edges that connect vertices (computers in a network).
  *
  * Part 1: Find how many triangles in the graph contain at least one vertex starting with the letter "t".
  *
  * Part 2: Find the maximum clique (fully connected component) in the graph (NP-hard problem).
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/23]]
  */
class AdventOfCodeDay23Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def parse(in: String*): Seq[Seq[String]] = in.map(_.split("-")).map(_.toSeq)

    def part1(in: String*): Long = {
      // Edge list in both directions
      val all = parse(in: _*)
      val all2 = all ++ all.map(_.reverse)

      // The list of neighbours for each vertex starting with a t
      val ts: Map[String, Set[String]] =
        all2.filter(_.head.startsWith("t")).groupMapReduce(_.head)(l => Set(l.last))(_ ++ _)

      // For each t vertex, find any links between two of its neighbours
      ts.map { case (tVal, neighbours) =>
        val linksBetweenNeighbours: Seq[Seq[String]] = all.filter(_.forall(neighbours))
        // If the neighbour also is a tVal, it should only be counted once.  Use the smallest value.
        linksBetweenNeighbours.count(_.forall(neighbour => !neighbour.startsWith("t") || neighbour < tVal))
      }.sum
    }

    /** Algorithm for looking up the max cliques in a graph. */
    def bronKerbosch(
        retained: Set[String],
        potential: Set[String],
        excluded: Set[String],
        graph: Map[String, Set[String]]
    ): Set[Set[String]] = {
      if (potential.isEmpty && excluded.isEmpty) Set(retained)
      else {
        val xGrow = mutable.HashSet() ++ excluded
        val pReduce = mutable.HashSet() ++ potential
        potential.flatMap { v =>
          bronKerbosch(retained + v, graph(v) intersect (pReduce -= v), graph(v) intersect (xGrow += v), graph)
        }
      }
    }

    /** Modification to only pick the maximum clique. */
    def bronKerboschMax(
        retained: Set[String],
        potential: Set[String],
        excluded: Set[String],
        graph: Map[String, Set[String]]
    ): Set[String] = {
      if (potential.isEmpty && excluded.isEmpty) retained
      else {
        // These are mutable to improve performance as the vertices in potential are checked
        val xGrow = mutable.HashSet() ++ excluded
        val pReduce = mutable.HashSet() ++ potential
        potential
          .flatMap { v =>
            bronKerbosch(retained + v, graph(v) intersect (pReduce -= v), graph(v) intersect (xGrow += v), graph)
          }
          .maxBy(_.size)
      }
    }

    def part2(in: String*): String = {
      // The graph represented as a map from a vertex to its neighbours
      val graph = parse(in: _*).flatMap(x => Seq(x, x.reverse)).groupMapReduce(_.head)(l => Set(l.last))(_ ++ _)

      // Find the maximum clique using Bron-Kerbosch
      bronKerboschMax(Set.empty, graph.keySet, Set.empty, graph).toSeq.sorted.mkString(",")
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """kh-tc
        |qp-kh
        |de-cg
        |ka-co
        |yn-aq
        |qp-ub
        |cg-tb
        |vc-aq
        |tb-ka
        |wh-tc
        |yn-cg
        |kh-ub
        |ta-co
        |de-co
        |tc-td
        |tb-wq
        |wh-td
        |ta-ka
        |td-qp
        |aq-cg
        |wq-ub
        |ub-vc
        |de-ta
        |wq-aq
        |wq-vc
        |wh-yn
        |ka-de
        |kh-ta
        |co-tc
        |wh-qp
        |tb-vc
        |td-yn
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 7
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe "co,de,ka,ta"
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day23Input.txt")
    lazy val answer1 = decryptLong("7X0r/mSttOC3CnLb39kuSQ==")
    lazy val answer2 = decrypt("o8RNCdJQad55wReDyTPYOQCzCHIGDToNw9ao1ffF7SopxFOVZpLbZU209bpQpWws")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
