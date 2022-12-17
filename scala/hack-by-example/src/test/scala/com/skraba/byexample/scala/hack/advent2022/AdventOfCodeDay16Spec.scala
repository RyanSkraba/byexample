package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.{Seq, mutable}

/** =Advent of Code 2022 Day 16 Solutions in scala=
  *
  * Input: A list of valves and tunnels.
  *
  * Part 1: Moving to a different valve, or opening the valve you are at takes 1
  * minutes. What is the most pressure you can release in 30 minutes?
  *
  * Part 2: What is the most pressure you can release if you have an elephant
  * helping you?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/16]]
  */
class AdventOfCodeDay16Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Valve(rate: Long, dst: Seq[String])

    def floydWarshall[T](
        vs: Iterable[T],
        edgeF: T => Iterator[T]
    ): Map[(T, T), Long] = {
      // Set up the distances between all vertices
      // If there's an initial edge, the distance is 1
      // The distance to itself is 0
      val dv = mutable.Map[(T, T), Long]().withDefaultValue(Long.MaxValue) ++
        vs.map(src => (src -> src, 0L)) ++
        (for (src <- vs; dst <- edgeF(src)) yield (src -> dst) -> 1L)

      // Apply all intermediate paths in all combinations
      for (mid <- vs; src <- vs; dst <- vs)
        if (
          dv(src -> mid) != Long.MaxValue &&
          dv(mid -> dst) != Long.MaxValue &&
          dv(src -> mid) + dv(mid -> dst) < dv(src -> dst)
        ) dv += (src -> dst) -> (dv(src -> mid) + dv(mid -> dst))

      dv.toMap
    }

    val Scan =
      """Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r

    def parse(in: String*): Map[String, Valve] = in.map {
      case Scan(name, rate, dst) =>
        name -> Valve(rate.toLong, dst.split("[, ]+").sorted)
    }.toMap

    def part1(
        valves: Map[String, Valve],
        lazyDv: Option[Map[(String, String), Long]] = None,
        lazyDsts: Option[Set[String]] = None,
        time: Long = 30,
        src: String = "AA",
        total: Long = 0
    ): Long = {
      if (time < 0) return total
      val dv = lazyDv.getOrElse(
        floydWarshall[String](valves.keys, valves(_).dst.iterator)
      )
      val dsts = lazyDsts.getOrElse(valves.filter(_._2.rate != 0).keys.toSet)
      (dsts
        .map(dst => (dst, dv(src -> dst)))
        .filter(_._2 < time + 2)
        .map(move => {
          val newTime = time - move._2 - 1
          part1(
            valves = valves,
            lazyDv = Some(dv),
            lazyDsts = Some(dsts - move._1),
            time = newTime,
            src = move._1,
            total = total + newTime * valves(move._1).rate
          )
        }) + total).max
    }

    def part2(
        valves: Map[String, Valve],
        lazyDv: Option[Map[(String, String), Long]] = None,
        lazyDsts: Option[Set[String]] = None,
        time: Long = 26,
        goals: Seq[(String, Long)] = Seq("AA" -> 0, "AA" -> 0),
        total: Long = 0
    ): Long = {
      if (time < 0) return total

      // The goals can be the same if and only if we're setting up the first destination.
      val init = goals.head == goals(1)

      // Lazy value initialization
      val dv = lazyDv.getOrElse(
        floydWarshall[String](valves.keys, valves(_).dst.iterator)
      )
      val dsts = lazyDsts.getOrElse(valves.filter(_._2.rate != 0).keys.toSet)

      val minDv = goals.minBy(_._2)._2
      if (minDv != 0L)
        return part2(
          valves = valves,
          lazyDv = Some(dv),
          lazyDsts = Some(dsts),
          time = time - minDv,
          goals = goals.map(goal => goal._1 -> (goal._2 - minDv)),
          total = total
        )

      // at least one of the goals is 0
      val newTotal = total + (time - 1) * goals.map {
        case (dst, 0) => valves(dst).rate
        case _        => 0L
      }.sum

      // if both goals are 0 then choose two new goals
      val moves = if (goals.head._2 == 0 && goals(1)._2 == 0) {
        val myDsts = dsts
          .map(x => (x, dv(goals.head._1 -> x))) + ((goals.head._1, time + 10))
        val elDsts =
          dsts.map(x => (x, dv(goals(1)._1 -> x))) + ((goals(1)._1, time + 10))
        val moves =
          for (
            myDst <- myDsts;
            elDst <- elDsts if elDst._1 != myDst._1
          )
            yield Seq(myDst, elDst)

        if (!init) moves else moves.filter(g => g.head._1 > g(1)._1)
      } else {
        for (newDst <- dsts + "")
          yield goals.map {
            case (oldDst, 0) if newDst.isEmpty => (oldDst, time + 10)
            case (oldDst, 0)                   => (newDst, dv(oldDst -> newDst))
            case (oldDst, oldDv)               => (oldDst, oldDv - 1)
          }
      }

      // if only one goal is zero
      val totals = moves.map(newGoals =>
        part2(
          valves = valves,
          lazyDv = Some(dv),
          lazyDsts = Some(dsts -- newGoals.map(_._1)),
          time = if (init) time else time - 1,
          goals = newGoals,
          total = newTotal
        )
      )

      (totals + newTotal).max
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
        |Valve BB has flow rate=13; tunnels lead to valves CC, AA
        |Valve CC has flow rate=2; tunnels lead to valves DD, BB
        |Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
        |Valve EE has flow rate=3; tunnels lead to valves FF, DD
        |Valve FF has flow rate=0; tunnels lead to valves EE, GG
        |Valve GG has flow rate=0; tunnels lead to valves FF, HH
        |Valve HH has flow rate=22; tunnel leads to valve GG
        |Valve II has flow rate=0; tunnels lead to valves AA, JJ
        |Valve JJ has flow rate=21; tunnel leads to valve II
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description part1") {
      part1(parse(input: _*)) shouldBe 1651
    }

    it("should match the puzzle description part2") {
      part2(parse(input: _*)) shouldBe 1707
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day16Input.txt")

    it("should have answers to part 1 (1 second)") {
      part1(valves = parse(input: _*)) shouldBe 1724
    }

    it("should have answers to part 2 (57 minutes)") {
      part2(valves = parse(input: _*)) shouldBe 2283
    }
  }
}
