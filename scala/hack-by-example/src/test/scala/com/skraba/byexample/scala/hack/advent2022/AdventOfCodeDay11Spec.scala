package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2022 Day 11 Solutions in scala=
  *
  * Input: Monkeys throwing things to each other according to their rules
  *
  * Part 1: The multiplication of the two most active monkeys
  *
  * Part 2: The multiplication of the two most active monkeys, with a slight modification to the rules
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/11]]
  */
class AdventOfCodeDay11Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class Monkey(modVal: Int, modZero: Int, modNonZero: Int, worryFn: Long => Long, items: Seq[Long]) {
      def toQueue: mutable.Queue[Long] = mutable.Queue(items: _*)
    }

    case class Monkeys(ms: Seq[Monkey]) {
      lazy val lcm: Long = ms.map(_.modVal.toLong).product

      lazy val counts: Array[Long] = Array.ofDim[Long](ms.length)

      lazy val queues: mutable.Buffer[mutable.Queue[Long]] =
        ms.map(_.toQueue).toBuffer

      def processMonkey(times: Int, worryFactor: Int): Monkeys = {
        for (_ <- 0 until times)
          for (i <- ms.indices) {
            while (queues(i).nonEmpty) {
              val worry = queues(i).dequeue()
              counts(i) = counts(i) + 1
              val newWorry =
                if (worryFactor == 1) ms(i).worryFn(worry) % lcm
                else ms(i).worryFn(worry) / worryFactor
              val dstMnk =
                if (newWorry % ms(i).modVal == 0) ms(i).modZero
                else ms(i).modNonZero
              queues(dstMnk).enqueue(newWorry)
            }
          }
        this
      }
    }

    def part1(monkeys: Monkey*): Long = {
      Monkeys(monkeys)
        .processMonkey(20, 3)
        .counts
        .sorted
        .reverse
        .take(2)
        .product
    }

    def part2(monkeys: Monkey*): Long = {
      Monkeys(monkeys)
        .processMonkey(10000, 1)
        .counts
        .sorted
        .reverse
        .take(2)
        .product
    }
  }

  import Solution._

  describe("Example case") {
    val example =
      """Monkey 0:
        |  Starting items: 79, 98
        |  Operation: new = old * 19
        |  Test: divisible by 23
        |    If true: throw to monkey 2
        |    If false: throw to monkey 3
        |
        |Monkey 1:
        |  Starting items: 54, 65, 75, 74
        |  Operation: new = old + 6
        |  Test: divisible by 19
        |    If true: throw to monkey 2
        |    If false: throw to monkey 0
        |
        |Monkey 2:
        |  Starting items: 79, 60, 97
        |  Operation: new = old * old
        |  Test: divisible by 13
        |    If true: throw to monkey 1
        |    If false: throw to monkey 3
        |
        |Monkey 3:
        |  Starting items: 74
        |  Operation: new = old + 3
        |  Test: divisible by 17
        |    If true: throw to monkey 0
        |    If false: throw to monkey 1
        |""".trimSplit.filter(_.nonEmpty)

    val monkeys = List(
      Monkey(23, 2, 3, w => w * 19, Seq(79, 98)),
      Monkey(19, 2, 0, w => w + 6, Seq(54, 65, 75, 74)),
      Monkey(13, 1, 3, w => w * w, Seq(79, 60, 97)),
      Monkey(17, 0, 1, w => w + 3, Seq(74))
    )

    it("should match the puzzle description") {
      val mnks = Monkeys(monkeys)

      mnks.processMonkey(1, 3)
      mnks.queues.head shouldBe Seq(20, 23, 27, 26)
      mnks.queues(1) shouldBe Seq(2080, 25, 167, 207, 401, 1046)
      mnks.queues(2) shouldBe empty
      mnks.queues(3) shouldBe empty

      // An entire round 2
      mnks.processMonkey(1, 3)
      mnks.queues.head shouldBe Seq(695, 10, 71, 135, 350)
      mnks.queues(1) shouldBe Seq(43, 49, 58, 55, 362)
      mnks.queues(2) shouldBe empty
      mnks.queues(3) shouldBe empty

      // 18 more rounds
      mnks.processMonkey(18, 3)
      mnks.queues.head shouldBe Seq(10, 12, 14, 26, 34)
      mnks.queues(1) shouldBe Seq(245, 93, 53, 199, 115)
      mnks.queues(2) shouldBe empty
      mnks.queues(3) shouldBe empty

      mnks.counts shouldBe Seq(101, 95, 7, 105)

      part1(monkeys: _*) shouldBe 10605
    }

    it("should match the part2 description") {
      val mnks = Monkeys(monkeys)

      mnks.processMonkey(1, 1)
      mnks.counts shouldBe Seq(2, 4, 3, 6)

      mnks.processMonkey(19, 1)
      mnks.counts shouldBe Seq(99, 97, 8, 103)

      mnks.processMonkey(980, 1)
      mnks.counts shouldBe Seq(5204, 4792, 199, 5192)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(10419, 9577, 392, 10391)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(15638, 14358, 587, 15593)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(20858, 19138, 780, 20797)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(26075, 23921, 974, 26000)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(31294, 28702, 1165, 31204)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(36508, 33488, 1360, 36400)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(41728, 38268, 1553, 41606)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(46945, 43051, 1746, 46807)

      mnks.processMonkey(1000, 1)
      mnks.counts shouldBe Seq(52166, 47830, 1938, 52013)

      part2(monkeys: _*) shouldBe 2713310158L
    }

  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day11Input.txt").filter(_.nonEmpty)

    // Manually hardcode the monkeys
    val monkeys = List(
      Monkey(11, 2, 7, w => w * 3, Seq(50, 70, 54, 83, 52, 78)),
      Monkey(7, 0, 2, w => w * w, Seq(71, 52, 58, 60, 71)),
      Monkey(3, 7, 5, w => w + 1, Seq(66, 56, 56, 94, 60, 86, 73)),
      Monkey(5, 6, 4, w => w + 8, Seq(83, 99)),
      Monkey(17, 1, 0, w => w + 3, Seq(98, 98, 79)),
      Monkey(13, 6, 3, w => w + 4, Seq(76)),
      Monkey(19, 4, 1, w => w * 17, Seq(52, 51, 84, 54)),
      Monkey(2, 5, 3, w => w + 7, Seq(82, 86, 91, 79, 94, 92, 59, 94))
    )

    it("should have answers") {
      val mnks = Monkeys(monkeys)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(6, 5, 10, 2, 3, 9, 7, 16)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(11, 12, 17, 18, 15, 22, 12, 21)
      mnks.queues shouldBe Seq(
        Seq(28812, 7203, 3, 3, 15, 16, 5, 5, 5, 5, 5, 4, 4, 4),
        Seq(28, 28, 28, 28, 17532),
        Seq(),
        Seq(12, 10, 8, 8, 11, 10, 4, 35, 5, 5, 8393, 1071, 11),
        Seq(),
        Seq(562, 562, 3470, 1454),
        Seq(),
        Seq()
      )

      mnks.processMonkey(1, 3)
      mnks.queues shouldBe Seq(
        Seq(3, 3, 3, 3, 2, 5, 2, 2, 120, 3),
        Seq(28, 28, 15866),
        Seq(),
        Seq(188, 188, 1158, 486, 2403, 3, 3, 7, 7, 3, 3, 3, 31, 31, 31, 31),
        Seq(),
        Seq(9606, 4, 4, 4, 4, 4, 11384114),
        Seq(),
        Seq()
      )
      mnks.counts shouldBe Seq(25, 17, 22, 31, 25, 26, 15, 40)

      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(35, 20, 25, 47, 37, 34, 19, 52)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(47, 24, 30, 65, 53, 38, 21, 66)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(63, 26, 32, 78, 64, 45, 23, 82)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(74, 28, 32, 99, 85, 47, 23, 93)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(97, 28, 32, 108, 94, 51, 23, 116)
      mnks.processMonkey(1, 3)
      mnks.counts shouldBe Seq(106, 28, 32, 133, 117, 53, 25, 125)

      part1(monkeys: _*) shouldBe decryptLong("DYUeQPdffRo1bKOYLlPMCA==")
      part2(monkeys: _*) shouldBe decryptLong("IpX/OQ3vfYaEILYZ9iBSIQ==")
    }
  }

}
