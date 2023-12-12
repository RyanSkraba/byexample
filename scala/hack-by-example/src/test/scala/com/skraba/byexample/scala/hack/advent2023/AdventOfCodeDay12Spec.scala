package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 12 Solutions in scala=
  *
  * Input: Given a list of springs where . is working, # is damaged and ? is
  * unknown, plus a checklist of contiguous damaged springs separated by working
  * springs.
  *
  * Part 1: For each line, find how many ways ? can be interpreted to match the
  * checklist. The answer is the sum of these.
  *
  * Part 2: The same problem, but with each line repeated 5 times and the
  * checklist repeated 5 times.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/12]]
  */
class AdventOfCodeDay12Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    case class Springs(in: String, check: Seq[Int]) {

      lazy val damaged: Int = in.count(_ == '#')
      lazy val unknowns: Int = in.count(_ == '?')
      lazy val checkSum: Int = check.sum

      def count(
          memo: collection.mutable.Map[Springs, Long] = collection.mutable.Map()
      ): Long =
        memo.getOrElseUpdate(
          this,
          if (check.isEmpty && damaged == 0) 1
          else if (check.isEmpty) 0
          else if (in.isEmpty) 0
          else if (in.length < checkSum + check.length - 1) 0
          else if (unknowns + damaged < checkSum) 0
          else if (in.head == '?')
            Springs(in.tail.dropWhile(_ == '.'), check).count(memo) + Springs(
              in.updated(0, '#'),
              check
            ).count(memo)
          else if (in.take(check.head).contains('.')) 0
          else if (in.length > check.head && in(check.head) == '#') 0
          else
            Springs(in.drop(check.head + 1).dropWhile(_ == '.'), check.tail)
              .count(memo)
        )
    }

    /** Parses each line into a Springs representation
      * @param repeat
      *   The number of times to repeat the
      * @param line
      *   a line of input, with the damaged and safe springs along with the
      *   check
      * @return
      */
    def parse(repeat: Int)(line: String): Springs = {
      val Array(ss, crc, _*) = line.split(" ")
      Springs(
        Seq
          .fill(repeat)(ss)
          .mkString("?")
          .dropWhile(_ == '.')
          .reverse
          .dropWhile(_ == '.')
          .reverse
          .replaceAll("\\.+", "\\."),
        Seq.fill(repeat)(crc.split(",").map(_.toInt)).flatten
      )
    }

    def part1(in: String*): Long = in.map(parse(1)).map(_.count()).sum

    def part2(in: String*): Long = in.map(parse(5)).map(_.count()).sum
  }

  import Solution._

  describe("Example case") {
    val input =
      """???.### 1,1,3
        |.??..??...?##. 1,1,3
        |?#?#?#?#?#?#?#? 1,3,1,6
        |????.#...#... 4,1,1
        |????.######..#####. 1,6,5
        |?###???????? 3,2,1
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {

      parse(1)("... 1").count() shouldBe 0
      parse(1)("#.. 1").count() shouldBe 1
      parse(1)(".#. 1").count() shouldBe 1
      parse(1)("..# 1").count() shouldBe 1
      parse(1)("#?. 1").count() shouldBe 1
      parse(1)("??? 1").count() shouldBe 3
      parse(1)("#?? 1").count() shouldBe 1

      parse(1)("???.### 1,1,3").count() shouldBe 1
      parse(1)(".??..??...?##. 1,1,3").count() shouldBe 4
      parse(1)("?#?#?#?#?#?#?#? 1,3,1,6").count() shouldBe 1
      parse(1)("????.#...#... 4,1,1").count() shouldBe 1
      parse(1)("????.######..#####. 1,6,5").count() shouldBe 4
      parse(1)("?###???????? 3,2,1").count() shouldBe 10

      part1(input: _*) shouldBe 21
    }

    it("should match the puzzle description for part 2") {
      parse(5)("???.### 1,1,3").count() shouldBe 1
      parse(5)(".??..??...?##. 1,1,3").count() shouldBe 16384
      parse(5)("?#?#?#?#?#?#?#? 1,3,1,6").count() shouldBe 1
      parse(5)("????.#...#... 4,1,1").count() shouldBe 16
      parse(5)("????.######..#####. 1,6,5").count() shouldBe 2500
      parse(5)("?###???????? 3,2,1").count() shouldBe 506250

      part2(input: _*) shouldBe 525152
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day12Input.txt")
    lazy val answer1 = decryptLong("4dNFIWMlv35C0TxdkqKu6A==")
    lazy val answer2 = decryptLong("PJo5PrdkDnGBXTXgqfv0qg==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
