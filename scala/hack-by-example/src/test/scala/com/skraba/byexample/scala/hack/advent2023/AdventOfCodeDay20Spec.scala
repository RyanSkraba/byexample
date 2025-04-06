package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2023 Day 20 Solutions in scala=
  *
  * Input: A list of modules that take input pulses and send output pulses. A pulse is either HIGH or LOW. Each module
  * has a name and potentially a state. There is one stateless "broadcast" module that just emits the same input pulse
  * to every downstream module.
  *
  * A FlipFlop ignores all HIGH pulses, but toggles its state (initially off) when it receives a LOW pulse, then sends a
  * LOW or HIGH pulse if it is off or on (respectively). The pulse is sent to all downstream modules.
  *
  * A Conjunction knows all of its inputs, and on receiving a pulse, sends a LOW pulse if last pulse received from all
  * of its inputs was HIGH (including the current input). Otherwise it emits a LOW pulse.
  *
  * A button sends a LOW pulse to the "broadcast" module.
  *
  * Part 1: The button has been pushed 1000 times. Calculate the product of the total HIGH and LOW pulses sent through
  * the system.
  *
  * Part 2: How many times does the button need to be pushed in order for the rx module to receive a LOW pulse?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/20]]
  */
class AdventOfCodeDay20Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** @param ff
      *   The names of the FlipFlops in an on state.
      * @param cnj
      *   Keyed on a Conjunction module name, the names of the upstream modules that have last sent a HIGH value.
      */
    case class States(
        ff: Set[String] = Set(),
        cnj: Map[String, Set[String]] = Map().withDefaultValue(Set())
    )

    /** A signal in the system, with a source, destination and whether the pulse was HIGH
      */
    type Signal = (String, String, Boolean)

    trait Module {
      val name: String
      val out: Seq[String]
      def pulse(s: Signal, ss: States): (States, Seq[Signal])
    }

    object Module {
      def apply(in: String): Module = {
        val Array(name, output, _*) = in.split(" -> ")
        val outputs = output.split("[, ]+")
        if (name.startsWith("%"))
          FlipFlop(name.drop(1), outputs)
        else if (name.startsWith("&")) {
          // The expected number of inputs needs to be updated later.
          Conjunction(name.drop(1), 0, outputs)
        } else
          Broadcaster(name, outputs)
      }

      def from(in: String*): Map[String, Module] =
        in
          .map(Module.apply)
          .map {
            case Conjunction(name, _, out) =>
              Conjunction(name, in.count(_.contains(name)) - 1, out)
            case module => module
          }
          .map(m => m.name -> m)
          .toMap
    }

    case class Broadcaster(
        override val name: String,
        override val out: Seq[String]
    ) extends Module {
      override def pulse(s: Signal, ss: States): (States, Seq[Signal]) =
        (ss, out.map((name, _, s._3)))
    }

    case class FlipFlop(
        override val name: String,
        override val out: Seq[String]
    ) extends Module {

      override def pulse(s: Signal, ss: States): (States, Seq[Signal]) = {
        if (s._3) (ss, Seq.empty)
        else {
          val on = ss.ff(name)
          (
            ss.copy(ff = if (on) ss.ff - name else ss.ff + name),
            out.map((name, _, !on))
          )
        }
      }

    }

    case class Conjunction(
        override val name: String,
        size: Int,
        override val out: Seq[String]
    ) extends Module {

      override def pulse(s: Signal, ss: States): (States, Seq[Signal]) = {
        val pre = ss.cnj.getOrElse(name, Set())
        val post = if (s._3) pre + s._1 else pre - s._1
        (
          ss.copy(cnj =
            if (post.isEmpty) ss.cnj.removed(name)
            else ss.cnj + (name -> post)
          ),
          out.map((name, _, post.size != size))
        )
      }
    }

    /** Push the button once and wait for the system to settle.
      * @param ms
      *   The input modules.
      * @param ss0
      *   The given state of the system
      * @return
      *   The number of times the HIGH pulse was emitted, the number of times the LOW pulse was emitted, and the last
      *   state of the system.
      */
    def push1(ms: Map[String, Module], ss0: States): (Long, Long, States) = {
      val signals = mutable.Queue(("button", "broadcaster", false))

      var low = 0L
      var high = 0L
      var ss = ss0

      while (signals.nonEmpty) {
        val current: Signal = signals.dequeue()
        if (current._3) high = high + 1
        else low = low + 1

        if (ms.contains(current._2)) {
          val (nextSs, next) = {
            ms(current._2).pulse(current, ss)
          }

          ss = nextSs
          signals.enqueueAll(next)
        }
      }

      (low, high, ss)
    }

    /** Push the button once and wait for the system to settle.
      * @param ms
      *   The input modules.
      * @param ss0
      *   The given state of the system
      * @param p
      *   A predicate to evaluate on any signal sent.
      * @return
      *   Whether the predicate ever returned true during, and the last state of the system after it has settled
      */
    def push1(
        ms: Map[String, Module],
        ss0: States,
        p: Signal => Boolean
    ): (Boolean, States) = {
      val signals = mutable.Queue(("button", "broadcaster", false))

      var exists = false
      var ss = ss0

      while (signals.nonEmpty) {
        val current: Signal = signals.dequeue()

        if (!exists)
          exists = p(current)

        if (ms.contains(current._2)) {
          val (nextSs, next) = {
            ms(current._2).pulse(current, ss)
          }

          ss = nextSs
          signals.enqueueAll(next)
        }
      }

      (exists, ss)
    }

    def part1(in: String*): Long = {
      val ms = Module.from(in: _*);

      val mash = LazyList.iterate((0L, 0L, States())) { case (low, high, ss) =>
        val (nextLow, nextHigh, nextSs) = push1(ms, ss)
        (low + nextLow, high + nextHigh, nextSs)
      }

      val (low, high, _) = mash.drop(1000).head
      low * high
    }

    def part2(in: String*): Long = {
      val ms = Module.from(in: _*);

      // This is the only module that outputs to rx, and it is a conjunction
      val toRx = ms.values.filter(_.out.contains("rx"))
      require(toRx.size == 1)
      require(toRx.head.isInstanceOf[Conjunction])

      // These are the modules that are input to the conjunction.
      // They must all output HIGH at least once before rx will receive
      // a LOW.
      val toToRx = ms.values.filter(_.out.contains(toRx.head.name)).map(_.name)

      // This finds how many times you have to mash the button before the named
      // module emits a HIGH.
      def findEmitHigh(name: String): Long =
        LazyList
          .iterate((false, States())) { case (_, ss) =>
            push1(ms, ss, s => s._1 == name && s._3)
          }
          .zipWithIndex
          .dropWhile(!_._1._1)
          .head
          ._2

      // ??? Huh, is there a cycle?  Just try multiplying them?
      toToRx.map(findEmitHigh).product
      // I'm not entirely sure why this works
    }
  }

  import Solution._

  describe("Example case") {
    val input1a =
      """broadcaster -> a, b, c
        |%a -> b
        |%b -> c
        |%c -> inv
        |&inv -> a
        |""".trimSplit

    val input1b =
      """broadcaster -> a
        |%a -> inv, con
        |&inv -> b
        |%b -> con
        |&con -> output
        |""".trimSplit

    def testMod(src: String, dst: Module, h: Boolean, ss: States) = {
      val (ssNext, out) = dst.pulse((src, dst.name, h), ss)
      out.map(_._2) shouldBe dst.out
      val highOut = out.exists(_._3)
      out.count(_._3 == highOut) shouldBe out.length
      (ssNext, highOut)
    }

    it("should correctly signal a Broadcaster") {
      val m = Module("x -> a, b")
      val ss = States()
      m shouldBe Broadcaster("x", Seq("a", "b"))
      testMod("zz", m, h = false, ss) shouldBe (ss, false)
      testMod("zz", m, h = true, ss) shouldBe (ss, true)
    }

    it("should correctly signal a FlipFlop") {
      val m = Module("%x -> a, b")
      val ssOff = States()
      val ssOn = States(Set("x"))
      m shouldBe FlipFlop("x", Seq("a", "b"))
      m.pulse(("zz", m.name, true), ssOff) shouldBe (ssOff, Seq.empty)
      m.pulse(("zz", m.name, true), ssOn) shouldBe (ssOn, Seq.empty)
      testMod("zz", m, h = false, ssOff) shouldBe (ssOn, true)
      testMod("zz", m, h = false, ssOn) shouldBe (ssOff, false)
    }

    it("should correctly signal a Conjunction") {
      val mRaw = Module("&x -> a, b")
      val ssOff = States()
      val ssOnM = States(cnj = Map("x" -> Set("m")))
      val ssOnN = States(cnj = Map("x" -> Set("n")))
      val ssOnMN = States(cnj = Map("x" -> Set("m", "n")))
      mRaw shouldBe Conjunction("x", 0, Seq("a", "b"))

      val m = Conjunction("x", 2, Seq("a", "b"))
      testMod("m", m, h = true, ssOff) shouldBe (ssOnM, true)
      testMod("n", m, h = true, ssOff) shouldBe (ssOnN, true)
      testMod("m", m, h = true, ssOnM) shouldBe (ssOnM, true)
      testMod("n", m, h = true, ssOnM) shouldBe (ssOnMN, false)
      testMod("m", m, h = true, ssOnN) shouldBe (ssOnMN, false)
      testMod("n", m, h = true, ssOnN) shouldBe (ssOnN, true)
      testMod("m", m, h = true, ssOnMN) shouldBe (ssOnMN, false)
      testMod("n", m, h = true, ssOnMN) shouldBe (ssOnMN, false)

      testMod("m", m, h = false, ssOff) shouldBe (ssOff, true)
      testMod("n", m, h = false, ssOff) shouldBe (ssOff, true)
      testMod("m", m, h = false, ssOnM) shouldBe (ssOff, true)
      testMod("n", m, h = false, ssOnM) shouldBe (ssOnM, true)
      testMod("m", m, h = false, ssOnN) shouldBe (ssOnN, true)
      testMod("n", m, h = false, ssOnN) shouldBe (ssOff, true)
      testMod("m", m, h = false, ssOnMN) shouldBe (ssOnN, true)
      testMod("n", m, h = false, ssOnMN) shouldBe (ssOnM, true)
    }

    it("should match the puzzle description for part 1a") {
      part1(input1a: _*) shouldBe 32000000
    }

    it("should match the puzzle description for part 1b") {
      part1(input1b: _*) shouldBe 11687500
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day20Input.txt")
    lazy val answer1 = decryptLong("Fzug7L0uubwtOApqmZZu4g==")
    lazy val answer2 = decryptLong("Qppe/BsqwExrILvatNTp5w==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
