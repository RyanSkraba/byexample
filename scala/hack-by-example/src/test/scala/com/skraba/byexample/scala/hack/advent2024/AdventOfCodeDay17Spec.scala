package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 17 Solutions in scala=
  *
  * Input: A CPU state with registers A, B, C, and a program.
  *
  * Part 1: Apply the program to the CPU state, where the program is a sequence of operations, one of which outputs an
  * integer. Find the output integer list.
  *
  * Part 2: Find a new CPU state with a new value for register A that will cause the program to output its own program.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/17]]
  */
class AdventOfCodeDay17Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class CPU(a: Long, b: Long, c: Long, program: Seq[Int], i: Int = 0, out: Seq[Int] = Seq.empty) {
      lazy val literal: Int = program(i + 1)

      lazy val combo: Long = literal match {
        case lit if lit <= 3 => lit
        case 4               => a
        case 5               => b
        case 6               => c
      }

      val ops: Seq[CPU => CPU] = Seq(
        cpu => cpu.copy(a = cpu.a >>> cpu.combo, i = cpu.i + 2), // adv
        cpu => cpu.copy(b = cpu.b ^ cpu.literal, i = cpu.i + 2), // bxl
        cpu => cpu.copy(b = cpu.combo % 8, i = cpu.i + 2), // bst
        cpu => cpu.copy(i = if (cpu.a == 0) cpu.i + 2 else cpu.literal), // jnz
        cpu => cpu.copy(b = cpu.b ^ cpu.c, i = cpu.i + 2), // bxc
        cpu => cpu.copy(i = cpu.i + 2, out = cpu.out :+ (cpu.combo % 8).toInt), // out
        cpu => cpu.copy(b = cpu.a >>> cpu.combo, i = cpu.i + 2), // bdv
        cpu => cpu.copy(c = cpu.a >>> cpu.combo, i = cpu.i + 2) // cdv
      )

      lazy val next: CPU = program.lift(i).map(ops).map(_(this)).getOrElse(copy(i = -1))

      lazy val endOutput: Seq[Int] = LazyList.iterate(this)(_.next).takeWhile(_.i != -1).last.out
    }

    def parse(in: String): CPU = {
      in.split("\n") match {
        case Array(a, b, c, _, program) =>
          CPU(
            a = a.split(" ").last.toInt,
            b = b.split(" ").last.toInt,
            c = c.split(" ").last.toInt,
            program = program.split(" ").last.split(",").map(_.toInt).toSeq
          )
      }
    }

    def part1(in: String): String = parse(in).endOutput.mkString(",")

    def part2brute(in: String): Long = {
      val cpu = parse(in)
      for (test <- 0 to Int.MaxValue) {
        val ll =
          LazyList
            .iterate(cpu.copy(a = test))(_.next)
            .takeWhile(_.i != -1)
            .map(_.out)
            .takeWhile(out => out.isEmpty || out.size <= cpu.program.size && out.last == cpu.program(out.size - 1))
        if (ll.nonEmpty && ll.last.size == cpu.program.size)
          return test
      }
      -1
    }

    // By examination:
    // The 16 Int program is a loop that continues until A is zero.
    // The only state that matters at the start of the loop is the value of A.
    // A is shifted right by 3 at the end of the loop.
    // We output one integer for each iteration of the loop, and it's value is given by:
    def loopOutput(xor1: Int, xor2: Int, a: Long): Long = a % 8 ^ xor1 ^ a >> (a % 8 ^ xor1) ^ xor2
    // Note that the XOR term  a % 8 only uses the last 3 bits of the value of A.
    // And the term a >> (a % 8 ^ xor1) shifts A from 0 to 7 bits to the right.
    // Since only the last 3 bits of the output are used, that the output only depends on
    // the last 10 bits of A.

    /** Optimized for the program, doesn't finish in a reasonable time. */
    def part2bruteOptimized(in: String): Long = {
      val cpu = parse(in)
      var test = 0L
      val (xor1, xor2) = (cpu.program(3), cpu.program(11))
      while (test < Long.MaxValue) {
        val ll0 = LazyList
          .iterate(test -> loopOutput(xor1, xor2, test)) { case (a, _) => (a >> 3) -> loopOutput(xor1, xor2, a >> 3) }
          .takeWhile(_ != (0L, 0L))
          .map(_._2.toInt % 8)
          .zipWithIndex

        val ll = ll0.takeWhile { case (out, i) => out == cpu.program(i) }
        if (ll.size == cpu.program.size)
          return test
        test += 1
      }
      -1
    }

    def part2(in: String): Long = {
      val cpu = parse(in)

      // Use the xor values from my program to calculate the next output value.
      val (xor1, xor2) = (cpu.program(3), cpu.program(11))

      // For every possible output, find the ten-bit values that could have produced it if they were the least significant bits of register A..
      val outputToTenBit: Map[Long, IndexedSeq[Long]] =
        (0L until 1024).map(a => loopOutput(xor1, xor2, a) % 8 -> a).groupMap(_._1)(_._2)

      // Long-winded way of doing a DFS!
      val solutions =
        for (
          c0 <- outputToTenBit(cpu.program.head);
          // Find only the possible patterns that overlap 7 bits with the previous
          c1 <- outputToTenBit(cpu.program(1)) if c0 >> 3L == (c1 & 127L);
          c2 <- outputToTenBit(cpu.program(2)) if c1 >> 3L == (c2 & 127L);
          c3 <- outputToTenBit(cpu.program(3)) if c2 >> 3L == (c3 & 127L);
          c4 <- outputToTenBit(cpu.program(4)) if c3 >> 3L == (c4 & 127L);
          c5 <- outputToTenBit(cpu.program(5)) if c4 >> 3L == (c5 & 127L);
          c6 <- outputToTenBit(cpu.program(6)) if c5 >> 3L == (c6 & 127L);
          c7 <- outputToTenBit(cpu.program(7)) if c6 >> 3L == (c7 & 127L);
          c8 <- outputToTenBit(cpu.program(8)) if c7 >> 3L == (c8 & 127L);
          c9 <- outputToTenBit(cpu.program(9)) if c8 >> 3L == (c9 & 127L);
          c10 <- outputToTenBit(cpu.program(10)) if c9 >> 3L == (c10 & 127L);
          c11 <- outputToTenBit(cpu.program(11)) if c10 >> 3L == (c11 & 127L);
          c12 <- outputToTenBit(cpu.program(12)) if c11 >> 3L == (c12 & 127L);
          c13 <- outputToTenBit(cpu.program(13)) if c12 >> 3L == (c13 & 127L);
          c14 <- outputToTenBit(cpu.program(14)) if c13 >> 3L == (c14 & 127L);
          c15 <- outputToTenBit(cpu.program(15)) if c14 >> 3L == (c15 & 127L)
        ) yield Seq(c14, c13, c12, c11, c10, c9, c8, c7, c6, c5, c4, c3, c2, c1, c0).foldLeft(c15) {
          // Reassemble the patterns three bits at a time to remove the overlapping regions.
          case (acc, c) => (acc << 3L) + (c & 7)
        }

      solutions.head
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """Register A: 729
        |Register B: 0
        |Register C: 0
        |
        |Program: 0,1,5,4,3,0
        |""".trim.stripMargin

    val input2 =
      """Register A: 2024
        |Register B: 0
        |Register C: 0
        |
        |Program: 0,3,5,4,3,0
        |""".trim.stripMargin

    it("should match the puzzle description for part 1") {
      part1(input) shouldBe "4,6,3,5,6,3,5,2,1,0"
    }

    it("should match the puzzle description for part 2") {
      part2brute(input2) shouldBe 117440
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day17Input.txt").mkString("\n")
    lazy val answer1 = decrypt("kp9XhkLRyEmw5BJmR9khvyKWTnExVnetqsFrcRv9OIo=")
    lazy val answer2 = decryptLong("79E74lyUzhx2gwfS8BXLuA==")

    it("should have answers for part 1") {
      part1(input) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input) shouldBe answer2
    }
  }
}
