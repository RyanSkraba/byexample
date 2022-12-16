package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

import scala.collection.{SortedSet, mutable}

/** =Advent of Code 2022 Day 16 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
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

    case class ValvesState(
        current: String = "AA",
        flow: Long = 0,
        activated: Set[String] = SortedSet.empty
    )

    case class Valves(
        state: ValvesState = ValvesState(),
        time: Long = 0,
        total: Long = 0,
        all: Map[String, Valve],
        distances: Map[(String, String), Long],
        nonzero: Iterable[String],
        visited: Set[ValvesState] = Set.empty,
        history: String = ""
    ) {

      lazy val current: Valve = all(state.current)

      private def mkHistoryString: String = {
        s"== Minute ${time + 1} ==\n" +
          (
            if (state.activated.isEmpty) "No valves are open.\n"
            else
              s"Valves ${state.activated.mkString(", ")} are open, releasing ${state.flow} pressure.\n"
          )
      }

      def go(dst: String): Valves = {
        val d = distances(state.current, dst)
        copy(
          state = state.copy(current = dst),
          visited = visited + state,
          time = time + d,
          total = total + state.flow * d,
          history = history + mkHistoryString + s"You move to valve $dst.\n\n"
        )
      }

      def open(): Valves = {
        if (state.activated.contains(state.current))
          throw new IllegalArgumentException("Opening an already opened valve!")
        copy(
          state = state.copy(
            flow = state.flow + current.rate,
            activated = state.activated + state.current
          ),
          visited = visited + state,
          time = time + 1,
          total = total + state.flow,
          history =
            history + mkHistoryString + s"You open valve ${state.current}.\n\n"
        )
      }

      def part1(remaining: Int): Valves = {
        if (remaining - time == 0)
          this
        else {
          val moveChoice =
            nonzero
              .filter(!state.activated(_))
              .filter(distances(state.current, _) < (remaining - time)) // lte
              .map(go)
              .map(_.open())
          val stayChoice = Some(
            copy(
              time = remaining,
              total = total + state.flow * (remaining - time)
            )
          )
          val next = (moveChoice ++ stayChoice).map(_.part1(remaining))
          if (next.isEmpty) this else next.maxBy(_.total)
        }
      }

      def part1Unoptimized(remaining: Int): Valves = {
        if (remaining - time <= 0)
          this // this should never be negative, but just in case
        else {
          //          val openChoice =
          //            if (!state.activated(state.current) && current.rate > 0)
          //              Some(open())
          //            else None
          val openChoice = None
          val moveChoice =
            nonzero
              .filter(!state.activated(_))
              .filter(
                distances(state.current, _) <= (remaining - time - 1)
              ) // lte
              .map(go)
              .map(_.open())
          // .filter(vs => !visited.contains(vs.state))
          val stayChoice = Some(
            copy(
              time = remaining,
              total = total + state.flow * (remaining - time)
            )
          )

          val x0 = nonzero.toSeq.filter(!state.activated(_))
          val x1 =
            x0.filter(distances(state.current, _) <= (remaining - time - 1))
          val x2 = x1.map(go)
          val x3 = x2.map(_.open())
          // val x4 = x3.filter(vs => !visited.contains(vs.state))

          val x5 = x0.map(distances(state.current, _))

          // is this necessary now?
          val choices = openChoice ++ moveChoice ++ stayChoice
          if (choices.isEmpty) this
          else choices.map(_.part1(remaining)).maxBy(_.total)
        }
      }

      def part1SingleSteps(remaining: Int): Valves = {
        if (remaining == 0) this
        else {
          val openChoice =
            if (!state.activated(state.current) && current.rate > 0)
              Some(open())
            else None
          val moveChoice =
            current.dst.map(go).filter(vs => !visited.contains(vs.state))
          val choices = openChoice ++ moveChoice
          if (choices.isEmpty) this
          else choices.map(_.part1(remaining - 1)).maxBy(_.total)
        }
      }

      def part1AlwaysOpen(remaining: Int): Valves = {
        if (remaining == 0) this
        else if (!state.activated(state.current) && current.rate > 0)
          open().part1(remaining - 1)
        else {
          current.dst.map(go).filter(vs => !visited.contains(vs.state))
          val choices =
            current.dst.map(go).filter(vs => !visited.contains(vs.state))
          if (choices.isEmpty) this
          else choices.map(_.part1(remaining - 1)).maxBy(_.total)
        }
      }
    }

    object Valves {
      val Scan =
        """Valve (..) has flow rate=(\d+); tunnels? leads? to valves? (.+)""".r

      def apply(in: String*): Valves = {
        val all = in.map { case Scan(name, rate, dst) =>
          name -> Valve(rate.toLong, dst.split("[, ]+").sorted)
        }.toMap
        val distances = floydWarshall[String](all.keys, all(_).dst.iterator)
        val nonzero = all.filter(_._2.rate != 0).keys
        Valves(all = all, distances = distances, nonzero = nonzero)
      }
    }

    def floydWarshall[T](
        vs: Iterable[T],
        edgeF: T => Iterator[T]
    ): Map[(T, T), Long] = {
      // Set up the distances between all vertices
      // If there's an initial edge, the distance is 1
      // The distance to itself is 0
      val d = mutable.Map[(T, T), Long]().withDefaultValue(Long.MaxValue) ++
        vs.map(src => (src -> src, 0L)) ++
        (for (src <- vs; dst <- edgeF(src)) yield (src -> dst) -> 1L)

      // Apply all intermediate paths in all combinations
      for (mid <- vs; src <- vs; dst <- vs)
        if (
          d(src -> mid) != Long.MaxValue &&
          d(mid -> dst) != Long.MaxValue &&
          d(src -> mid) + d(mid -> dst) < d(src -> dst)
        ) d += (src -> dst) -> (d(src -> mid) + d(mid -> dst))

      d.toMap
    }

    def floydWarshallWithEdgeWeights[T](
        vs: Iterable[T],
        edgeFn: T => Iterator[(T, Long)]
    ): Map[(T, T), Long] = {
      // Set up the distances between all vertices
      // The distance to itself is 0
      val d = mutable.Map[(T, T), Long]().withDefaultValue(Long.MaxValue) ++
        vs.map(src => (src -> src, 0L)) ++
        (for (src <- vs; (dst, weight) <- edgeFn(src))
          yield (src -> dst) -> weight)

      // Apply all intermediate paths in all combinations
      for (mid <- vs; src <- vs; dst <- vs)
        if (
          d(src -> mid) != Long.MaxValue &&
          d(mid -> dst) != Long.MaxValue &&
          d(src -> mid) + d(mid -> dst) < d(src -> dst)
        ) d += (src -> dst) -> (d(src -> mid) + d(mid -> dst))

      d.toMap
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

    val valves = Valves(input: _*)

    it("set up the valves correctly") {
      valves.all.size shouldBe 10

      valves.all("AA").rate shouldBe 0
      valves.all("AA").dst shouldBe Seq("BB", "DD", "II")

      valves.all("JJ").rate shouldBe 21
      valves.all("JJ").dst shouldBe Seq("II")
    }

    it("moves between valves correctly") {
      val moves = valves.go("DD").open().go("CC").go("BB").open().go("AA")

      moves.all shouldBe valves.all
      moves.state shouldBe ValvesState("AA", 33, Set("BB", "DD"))
      moves.visited shouldBe Set(
        ValvesState("AA", 0, Set()),
        ValvesState("DD", 0, Set()),
        ValvesState("DD", 20, Set("DD")),
        ValvesState("CC", 20, Set("DD")),
        ValvesState("BB", 20, Set("DD")),
        ValvesState("BB", 33, Set("BB", "DD"))
      )
      moves.time shouldBe 6
      moves.total shouldBe 93
      moves.current shouldBe Valve(0, Seq("BB", "DD", "II"))

      moves.history.trim shouldBe """== Minute 1 ==
        |No valves are open.
        |You move to valve DD.
        |
        |== Minute 2 ==
        |No valves are open.
        |You open valve DD.
        |
        |== Minute 3 ==
        |Valves DD are open, releasing 20 pressure.
        |You move to valve CC.
        |
        |== Minute 4 ==
        |Valves DD are open, releasing 20 pressure.
        |You move to valve BB.
        |
        |== Minute 5 ==
        |Valves DD are open, releasing 20 pressure.
        |You open valve BB.
        |
        |== Minute 6 ==
        |Valves BB, DD are open, releasing 33 pressure.
        |You move to valve AA.
        |""".stripMargin.trim
    }

    it("plays the short game correctly") {
      val games = (0 to 5).map(valves.part1)

      games(0).total shouldBe 0
      games(1).total shouldBe 0
      games(2).total shouldBe 0
      games(3).state.flow shouldBe 20 // Went to DD
      games(3).total shouldBe 20
      games(4).state.flow shouldBe 23 // Went to DD, then activated EE
      games(4).total shouldBe 40
      games(5).state.flow shouldBe 23
      games(5).total shouldBe 63
    }

    it("should match the puzzle description") {
      val part1 = valves.part1(30)
      part1.total shouldBe 1651
      // part2(input: _*) shouldBe 200
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day16Input.txt")
    val valves = Valves(input: _*)

    it("should have answers to part 1 (90 seconds)", Slow) {
      valves.part1(30).total shouldBe 1724
    }

    it("it should have answers to part 2 (XXXXX seconds)", Slow) {
      valves.part1(30).total shouldBe 1724
    }
  }
}
