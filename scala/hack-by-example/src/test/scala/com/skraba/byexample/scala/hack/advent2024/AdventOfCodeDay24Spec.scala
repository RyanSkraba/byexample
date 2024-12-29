package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 24 Solutions in scala=
  *
  * Input: A list of starting signals, followed by a list of signals that are calculated from the starting signals using
  * the bit operations AND, OR, and XOR.
  *
  * Part 1: Calculate the signals for the z bits, and assemble them into a Z-register.
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/24]]
  */
class AdventOfCodeDay24Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Gate(op: String, src1: String, src2: String) {
      def calculate(gates: Map[String, Gate], x: Long, y: Long): Long = {
        val operand1 =
          if (src1.startsWith("x")) (x >> src1.tail.toInt) % 2L
          else if (src1.startsWith("y")) (y >> src1.tail.toInt) % 2L
          else gates.get(src1).map(_.calculate(gates, x, y)).getOrElse(0L)
        val operand2 =
          if (src2.startsWith("x")) (x >> src2.tail.toInt) % 2L
          else if (src2.startsWith("y")) (y >> src2.tail.toInt) % 2L
          else gates.get(src2).map(_.calculate(gates, x, y)).getOrElse(0L)
        op match {
          case "AND" => operand1 & operand2
          case "OR"  => operand1 | operand2
          case "XOR" => operand1 ^ operand2
        }
      }
    }

    /** Parse the input into gates and initial signals. The initial signals are all X and Y, so store them in a Long
      * register.
      */
    def parse(in: String*): (Long, Long, Map[String, Gate]) = {
      // Collect the gates and initial values
      val gates: Seq[(String, Either[Gate, Long])] = in.map(_.split("( -> | |: )")).flatMap {
        case Array(src1, op, src2, dst)        => Some(dst -> Left(Gate(op, src1, src2)))
        case Array(bit, value) if value == "1" => Some(bit -> Right(1L << bit.tail.takeWhile(_.isDigit).toInt))
        case _                                 => None
      }
      // X and Y registers can be calculated from the initial signals
      val x = gates.filter(_._1.startsWith("x")).flatMap(_._2.toOption).sum
      val y = gates.filter(_._1.startsWith("y")).flatMap(_._2.toOption).sum
      // All other gates
      (x, y, gates.collect { case (src -> Left(gate)) => src -> gate }.toMap)
    }

    def part1(in: String*): Long = {
      val (x, y, gates) = parse(in: _*)
      // All the z bits.
      val zs = gates.filter(_._1.startsWith("z")).map { case (dst, gate) => dst -> gate.calculate(gates, x, y) }
      // Reassemble the Z register from all of the non-zero bits.
      zs.filter(_._2 != 0).foldLeft(0L) { case (acc, (zbit, _)) => acc | (1L << zbit.tail.toInt) }
    }

    def part2(in: String*): Long = 200
  }

  import Solution._

  describe("Example case") {
    val inputMini =
      """x00: 1
        |x01: 1
        |x02: 1
        |y00: 0
        |y01: 1
        |y02: 0
        |
        |x00 AND y00 -> z00
        |x01 XOR y01 -> z01
        |x02 OR y02 -> z02
        |""".trim.stripMargin.split("\n")

    val input =
      """x00: 1
        |x01: 0
        |x02: 1
        |x03: 1
        |x04: 0
        |y00: 1
        |y01: 1
        |y02: 1
        |y03: 1
        |y04: 1
        |
        |ntg XOR fgs -> mjb
        |y02 OR x01 -> tnw
        |kwq OR kpj -> z05
        |x00 OR x03 -> fst
        |tgd XOR rvg -> z01
        |vdt OR tnw -> bfw
        |bfw AND frj -> z10
        |ffh OR nrd -> bqk
        |y00 AND y03 -> djm
        |y03 OR y00 -> psh
        |bqk OR frj -> z08
        |tnw OR fst -> frj
        |gnj AND tgd -> z11
        |bfw XOR mjb -> z00
        |x03 OR x00 -> vdt
        |gnj AND wpb -> z02
        |x04 AND y00 -> kjc
        |djm OR pbm -> qhw
        |nrd AND vdt -> hwm
        |kjc AND fst -> rvg
        |y04 OR y02 -> fgs
        |y01 AND x02 -> pbm
        |ntg OR kjc -> kwq
        |psh XOR fgs -> tgd
        |qhw XOR tgd -> z09
        |pbm OR djm -> kpj
        |x03 XOR y03 -> ffh
        |x00 XOR y04 -> ntg
        |bfw OR bqk -> z06
        |nrd XOR fgs -> wpb
        |frj XOR qhw -> z04
        |bqk OR frj -> z07
        |y03 OR x01 -> nrd
        |hwm AND bqk -> z03
        |tgd XOR rvg -> z12
        |tnw OR pbm -> gnj
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1 mini") {
      part1(inputMini: _*) shouldBe 4
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 2024
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 200
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day24Input.txt")
    lazy val answer1 = decryptLong("2qG0v8u9cPKHglRg/BoD8A==");
    lazy val answer2 = decryptLong("U9BZNCixKWAgOXNrGyDe5A==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
