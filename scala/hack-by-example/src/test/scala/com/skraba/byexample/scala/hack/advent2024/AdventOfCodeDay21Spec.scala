package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2024 Day 21 Solutions in scala=
  *
  * Input: A list of codes to enter into a numeric keypad, with a specific layout. A robot will push the buttons on the
  * numeric pad, and it is controlled by a keypad with directional buttons.
  *
  * Part 1: There are two robots between you and the numeric keypad. Find the number of button presses you need to push
  * on the robot keypad closest to you to eventually have the code entered on the numeric keypad. Sum the product of the
  * numeric part of each code by the number of button presses it takes to enter it.
  *
  * Part 2: Same thing, but there are 25 robots between you and the numeric keypad.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/21]]
  */
class AdventOfCodeDay21Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** To move from the button on the src position of the string to the button at the dst position, these directions
      * need to be applied.
      *
      * {{{
      * +---+---+---+       +---+---+
      * | 7 | 8 | 9 |       | ^ | A |
      * +---+---+---+   +---+---+---+
      * | 4 | 5 | 6 |   | < | v | > |
      * +---+---+---+   +---+---+---+
      * | 1 | 2 | 3 |
      * +---+---+---+
      *     | 0 | A |
      *     +---+---+
      * }}}
      */
    def fromTo(src: Int, dst: Int, buttons: String): (String, String) = buttons(src).toString + buttons(dst) -> {
      val dx = dst % 3 - src % 3
      val dy = dst / 3 - src / 3
      val x = if (dx < 0) "<" * math.abs(dx) else ">" * dx
      val y = if (dy < 0) "^" * math.abs(dy) else "v" * dy
      // Always avoid passing by the 'X' button (an invalid spot).
      if (buttons(src + dy * 3) == 'X') x + y
      else if (buttons(src + dx) == 'X') y + x
      else if (x.startsWith("<")) x + y // A bit by trial and error.
      else y + x
    }

    val NumButtons: String = "789456123X0A"
    lazy val NumPad: Map[String, String] =
      (for (src <- NumButtons.indices; dst <- NumButtons.indices) yield fromTo(src, dst, NumButtons)).toMap

    val DirButtons: String = "X^A<v>"
    lazy val DirPad: Map[String, String] =
      (for (src <- DirButtons.indices; dst <- DirButtons.indices) yield fromTo(src, dst, DirButtons)).toMap

    def numKey(in: String): Iterator[String] = ("A" + in).sliding(2).map(NumPad).map(_ + "A")
    def dirKey(in: String): Iterator[String] = ("A" + in).sliding(2).map(DirPad).map(_ + "A")

    def part1(in: String*): Long = {
      in.map(code => code.filter(_.isDigit).toLong * dirKey(dirKey(numKey(code).mkString).mkString).mkString.length).sum
    }

    def part2(robots: Int, in: String*): Long = {
      // Memo from a syllable (ending with A) and the number of desired iterations to the number of keystrokes
      // necessary to enter it.  ("^^>A", 4) -> is means four more robots need to be manipulated and the desired end
      // sequence is "^^>A"
      lazy val syllableToIterations: ((String, Int)) => Long = {
        val memo = new mutable.HashMap[(String, Int), Long]()
        key =>
          memo.getOrElseUpdate(
            key,
            key match {
              case (code, 0) => code.length
              case (code, n) => dirKey(code).map(syllableToIterations(_, n - 1)).sum
            }
          )
      }
      in.map(code => code.filter(_.isDigit).toLong * numKey(code).map(syllableToIterations(_, robots)).sum).sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """029A
        |980A
        |179A
        |456A
        |379A
        |""".trimSplit

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 126384
    }

    it("should solve part1 with part 2") {
      part2(2, input: _*) shouldBe 126384
    }

    it("should match the puzzle description for part 2") {
      part2(25, input: _*) shouldBe 154115708116294L
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day21Input.txt")
    lazy val answer1 = decryptLong("jbymwwx5o4KzWYqMd+1G7A==")
    lazy val answer2 = decryptLong("0YLzi62GPTcb7grfxLgkaA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should solve part1 with part 2") {
      part2(2, input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(25, input: _*) shouldBe answer2
    }
  }
}
