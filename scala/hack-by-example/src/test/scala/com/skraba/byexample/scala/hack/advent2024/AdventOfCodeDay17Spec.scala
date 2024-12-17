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
    }

    def parse(in: String): CPU = {
      in.split("\n") match {
        case Array(a, b, c, _, program) =>
          CPU(
            a = a.split(" ").last.toInt,
            b = b.split(" ").last.toInt,
            c = c.split(" ").last.toInt,
            program = program.split(" ").last.split(",").map(_.toInt)
          )
      }
    }

    def part1(in: String): String = {
      val cpu = parse(in)
      LazyList.iterate(cpu)(_.next).filter(_.i == -1).head.out.mkString(",")
    }

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

    def part2(in: String): Long = {
      ???
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
    lazy val answer2 = 200

    it("should have answers for part 1") {
      part1(input) shouldBe answer1
    }

    ignore("should have answers for part 2") {
      part2(input) shouldBe answer2
    }
  }
}
