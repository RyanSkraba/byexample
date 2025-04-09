package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 13 Solutions in scala=
  *
  * Input: TODO
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/13]]
  */
class AdventOfCodeDay13Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Signal(value: Either[Int, Seq[Signal]]) extends Ordered[Signal] {
      override def compare(that: Signal): Int = {
        (this.value, that.value) match {
          case (Left(v1), Left(v2)) => v1 - v2
          case (Left(_), _)         => Signal(Right(Seq(this))).compareTo(that)
          case (_, Left(_))         => this.compareTo(Signal(Right(Seq(that))))
          case (Right(xs1 :: xs1T), Right(xs2 :: xs2T)) =>
            val cmp = xs1.compareTo(xs2)
            if (cmp != 0) cmp
            else Signal(Right(xs1T)).compareTo(Signal(Right(xs2T)))
          case (Right(xs1), Right(xs2)) => xs1.size - xs2.size
        }
      }
    }

    object Signal {
      def apply(in: String): Signal = pop(Seq.empty, in)._1.head

      private def pop(acc: Seq[Signal], in: String): (Seq[Signal], String) = {
        if (in.head == ',') pop(acc, in.tail)
        else if (in.head == '[') {
          var remaining = in.tail
          var popped = Seq.empty[Signal]
          while (remaining.head != ']') {
            val (x, y) = pop(popped, remaining)
            popped = x
            remaining = y
          }
          (acc :+ Signal(Right(popped)), remaining.tail)
        } else if (in.head.isDigit) {
          val (num, inRest) = in.span(_.isDigit)
          (acc :+ Signal(Left(num.toInt)), inRest)
        } else pop(acc, in.tail)
      }
    }

    def mkString(s: Signal, start: String = "<", sep: String = "|", end: String = ">"): String = {
      s.value match {
        case Left(v)   => v.toString
        case Right(xs) => s"$start${xs.map(mkString(_, start, sep, end)).mkString(sep)}$end"
      }
    }

    def part1(in: String*): Long = in
      .map(Signal(_))
      .grouped(2)
      .zipWithIndex
      .filter { case (Seq(one, two), _) => one <= two }
      .map(_._2 + 1)
      .sum

    def part2(in: String*): Long = {
      val decoders = Seq(Signal("[[2]]"), Signal("[[6]]"))
      val sorted = (decoders ++ in.map(Signal(_))).sorted
      (sorted.indexOf(decoders.head) + 1) * (sorted.indexOf(decoders(1)) + 1)
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """[1,1,3,1,1]
        |[1,1,5,1,1]
        |
        |[[1],[2,3,4]]
        |[[1],4]
        |
        |[9]
        |[[8,7,6]]
        |
        |[[4,4],4,4]
        |[[4,4],4,4,4]
        |
        |[7,7,7,7]
        |[7,7,7]
        |
        |[]
        |[3]
        |
        |[[[]]]
        |[[]]
        |
        |[1,[2,[3,[4,[5,6,7]]]],8,9]
        |[1,[2,[3,[4,[5,6,0]]]],8,9]
        |""".trimSplit.filter(_.nonEmpty)

    it("should test the parse function") {
      val signals = input.map(Signal(_))
      signals.map(mkString(_)) shouldBe Seq(
        "<1|1|3|1|1>",
        "<1|1|5|1|1>",
        "<<1>|<2|3|4>>",
        "<<1>|4>",
        "<9>",
        "<<8|7|6>>",
        "<<4|4>|4|4>",
        "<<4|4>|4|4|4>",
        "<7|7|7|7>",
        "<7|7|7>",
        "<>",
        "<3>",
        "<<<>>>",
        "<<>>",
        "<1|<2|<3|<4|<5|6|7>>>>|8|9>",
        "<1|<2|<3|<4|<5|6|0>>>>|8|9>"
      )
    }

    it("should match the puzzle description") {
      part1(input: _*) shouldBe 13
      part2(input: _*) shouldBe 140
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day13Input.txt").filter(_.nonEmpty)
    it("should have answers") {
      part1(input: _*) shouldBe decryptLong("6PJgwNLQmr3ndmMnzV8RNw==")
      part2(input: _*) shouldBe decryptLong("3WWv/Yrlie3sUPbGW/g3mg==")
    }
  }
}
