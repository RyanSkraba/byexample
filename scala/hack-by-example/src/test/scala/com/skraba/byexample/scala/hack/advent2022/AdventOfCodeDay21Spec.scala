package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util._
import scala.util.matching.Regex

/** =Advent of Code 2022 Day 21 Solutions in scala=
  *
  * Input: A list of monkeys and the operations they perform.
  *
  * Part 1: Find the value of the root monkey.
  *
  * Part 2: If the root monkey wants to make sure their two friends have the
  * same value, what should the "humn" monkey yell?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/21]]
  */
class AdventOfCodeDay21Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    trait Monkey {
      val id: String
      def value(monkeys: Map[String, Monkey]): Long
      def balance(monkeys: Map[String, Monkey], expected: Long): Long = expected
    }

    case class YellMonkey(id: String, value: Long) extends Monkey {
      override def value(monkeys: Map[String, Monkey]): Long = value
    }

    /** Just throws an exception when it tries to calculate, used to determine
      * where the human is located.
      */
    case class BrokenMonkey(id: String) extends Monkey {
      override def value(monkeys: Map[String, Monkey]): Long =
        throw new IllegalArgumentException()
    }

    case class JobMnk(id: String, lId: String, rId: String, op: Char)
        extends Monkey {

      override def value(monkeys: Map[String, Monkey]): Long =
        if (op == '+') monkeys(lId).value(monkeys) + monkeys(rId).value(monkeys)
        else if (op == '-')
          monkeys(lId).value(monkeys) - monkeys(rId).value(monkeys)
        else if (op == '*')
          monkeys(lId).value(monkeys) * monkeys(rId).value(monkeys)
        else if (op == '/')
          monkeys(lId).value(monkeys) / monkeys(rId).value(monkeys)
        else if (op == '=')
          if (monkeys(lId).value(monkeys) == monkeys(rId).value(monkeys)) 1
          else 0
        else throw new IllegalArgumentException(s"Unexpected op $op")

      override def balance(
          monkeys: Map[String, Monkey],
          expected: Long
      ): Long = {
        val left = monkeys(lId)
        val right = monkeys(rId)

        (Try(left.value(monkeys)), Try(right.value(monkeys))) match {
          case (Success(lVal), Failure(_)) if op == '=' =>
            right.balance(monkeys, lVal)
          case (Failure(_), Success(rVal)) if op == '=' =>
            left.balance(monkeys, rVal)
          case (Success(lVal), Failure(_)) if op == '+' =>
            right.balance(monkeys, expected - lVal)
          case (Failure(_), Success(rVal)) if op == '+' =>
            left.balance(monkeys, expected - rVal)
          case (Success(lVal), Failure(_)) if op == '-' =>
            right.balance(monkeys, lVal - expected)
          case (Failure(_), Success(rVal)) if op == '-' =>
            left.balance(monkeys, expected + rVal)
          case (Success(lVal), Failure(_)) if op == '*' =>
            right.balance(monkeys, expected / lVal)
          case (Failure(_), Success(rVal)) if op == '*' =>
            left.balance(monkeys, expected / rVal)
          case (Success(lVal), Failure(_)) if op == '/' =>
            right.balance(monkeys, lVal / expected)
          case (Failure(_), Success(rVal)) if op == '/' =>
            left.balance(monkeys, expected * rVal)
        }
      }
    }

    val Yell: Regex = """(.+): (\d+)""".r
    val Job: Regex = """(.+): (.+) (.) (.+)""".r

    def parse(in: String): Monkey = in match {
      case Yell(id, value)     => YellMonkey(id, value.toLong)
      case Job(id, m1, op, m2) => JobMnk(id, m1, m2, op.head)
    }

    def part1(in: String*): Long = {
      val monkeys = in.map(parse).map(m => (m.id, m)).toMap
      monkeys("root").value(monkeys)
    }

    def part2(in: String*): Long = {
      val monkeys = in.map(parse).map(m => (m.id, m)).toMap
      val root: JobMnk = monkeys("root") match { case root: JobMnk => root }
      // Find the root and ask it to balance its two sides
      root.copy(op = '=').balance(monkeys + ("humn" -> BrokenMonkey("humn")), 0)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """root: pppw + sjmn
        |dbpl: 5
        |cczh: sllz + lgvd
        |zczc: 2
        |ptdq: humn - dvpt
        |dvpt: 3
        |lfqf: 4
        |humn: 5
        |ljgn: 2
        |sjmn: drzm * dbpl
        |sllz: 4
        |pppw: cczh / lfqf
        |lgvd: ljgn * ptdq
        |drzm: hmdt - zczc
        |hmdt: 32
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should parse monkeys correctly") {
      parse(input.head) shouldBe JobMnk("root", "pppw", "sjmn", '+')
      parse(input(1)) shouldBe YellMonkey("dbpl", 5L)

      val monkeys = input.map(parse).map(m => (m.id, m)).toMap
      monkeys("dbpl").value(monkeys) shouldBe 5L
      monkeys("hmdt").value(monkeys) shouldBe 32L
      monkeys("zczc").value(monkeys) shouldBe 2L
      monkeys("drzm").value(monkeys) shouldBe 30L
    }

    it("should match the puzzle description") {
      part1(input: _*) shouldBe 152
      part2(input: _*) shouldBe 301
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day21Input.txt")
    it("should have answers") {
      part1(input: _*) shouldBe 84244467642604L
      part2(input: _*) shouldBe 3759569926192L
    }
  }
}
