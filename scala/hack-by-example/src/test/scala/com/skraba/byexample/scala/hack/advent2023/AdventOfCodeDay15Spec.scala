package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** =Advent of Code 2023 Day 15 Solutions in scala=
  *
  * Input: A long string of inputs separated by commas, indicating which lens of focal length 1 to 9 should be put or
  * removed in boxes to direct the light through.
  *
  * Part 1: For each command, calculate the hash code with the specific numbers we've been supplied, and sum them
  * together.
  *
  * Part 2: Each command either puts a lens in a box, replaces a lens or removes it. The box is indicated by the
  * hashcode of the label, where the character - removes that labeled lens, and =NN adds or replaces the lens with that
  * label with a new one with the specified focal length. Calculate the focusing power after all the commands are
  * applied.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/15]]
  */
class AdventOfCodeDay15Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    def hash(in: String): Int =
      in.foldLeft(0)((acc, in) => (acc + in) * 17 % 256)

    def part1(in: String*): Long = in.head.split(",").map(hash).sum

    /** The focusing power of a lense is the index of the box (from 1) times the slow (from 1) times the focal length.
      * This calculates the sum over all boxes.
      */
    def focusingPower(boxes: Seq[Seq[(String, Int)]]): Long = {
      boxes.zipWithIndex.flatMap { case (b, i1) =>
        b.map(_._2).zipWithIndex.map { case (focalLength, i2) =>
          (i1 + 1L) * focalLength * (i2 + 1)
        }
      }.sum
    }

    /** This implementation applies the rules sequentially to the boxes as an accumulator.
      */
    def part2a(in: String*): Long = {
      focusingPower(
        in.head.split(",").foldLeft(Seq.fill(256)(Seq[(String, Int)]())) { (boxes, cmdStr) =>
          val (label, cmd) = cmdStr.span(c => c != '=' && c != '-')
          val boxId = hash(label)
          val box = boxes(boxId)
          val slotId = box.indexWhere(_._1 == label)
          if (cmd == "-" && slotId != -1) {
            // Remove the box if it exists.
            boxes.updated(boxId, box.patch(slotId, Seq.empty, 1))
          } else if (cmd.startsWith("=")) {
            val lens = label -> cmd.drop(1).toInt
            if (slotId != -1) {
              // Add the box at the existing slot
              boxes.updated(boxId, box.updated(slotId, lens))
            } else {
              // Add the box to the end
              boxes.updated(boxId, box.appended(lens))
            }
          } else
            boxes
        }
      )
    }

    def part2b(in: String*): Long = {
      // Convert all the incoming commands to the form  "label" -> focalLength or -1 to remove
      val cmds = in.head
        .split(",")
        .map(_.span(c => c != '=' && c != '-'))
        .map {
          case (label, "-")         => label -> -1
          case (label, focalLength) => label -> focalLength.drop(1).toInt
        }

      // Starting from the back, filter out all of the labels that are followed by a removal
      // For all non-removed lens, set the focal length to the last value
      val removed = mutable.Set[String]()
      val focalLength = mutable.HashMap[String, Int]()
      val filtered = cmds.reverse.flatMap {
        case (label, -1)                  => removed.add(label); None
        case (label, _) if removed(label) => None
        case x if !focalLength.contains(x._1) =>
          focalLength(x._1) = x._2; Some(x)
        case x => Some(x._1 -> focalLength(x._1))
      }.reverse

      // The distinct method removes all subsequent values so only the first label is found
      filtered.distinct
        .map(x => (hash(x._1), x._2))
        .groupBy(_._1)
        .values
        .flatMap(_.zipWithIndex)
        .map { case ((boxId, focalLength), slotId) =>
          (boxId + 1L) * (slotId + 1L) * focalLength
        }
        .sum
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7
        |""".trim.stripMargin.split("\n")

    it("should hash") {
      hash("H") shouldBe 200
      hash("HA") shouldBe 153
      hash("HAS") shouldBe 172
      hash("HASH") shouldBe 52
      hash("rn=1") shouldBe 30
      hash("cm-") shouldBe 253
      hash("qp=3") shouldBe 97
      hash("cm=2") shouldBe 47
      hash("qp-") shouldBe 14
      hash("pc=4") shouldBe 180
      hash("ot=9") shouldBe 9
      hash("ab=5") shouldBe 197
      hash("pc-") shouldBe 48
      hash("pc=6") shouldBe 214
      hash("ot=7") shouldBe 231
    }

    it("should calculate focusing power") {
      focusingPower(
        Seq(
          Seq("rn" -> 1, "cm" -> 2),
          Nil,
          Nil,
          Seq("ot" -> 7, "ab" -> 5, "pc" -> 6)
        )
      ) shouldBe 145
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 1320
    }

    it("should match the puzzle description for part 2 (first solution)") {
      part2a(input: _*) shouldBe 145
    }

    it("should match the puzzle description for part 2 (second solution)") {
      part2b(input: _*) shouldBe 145
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day15Input.txt")
    lazy val answer1 = decryptLong("mgwU+Y12UdBxEIf/oCYKBA==")
    lazy val answer2 = decryptLong("m3WNHPlDTrpw8FoIVT2z6A==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should match the puzzle description for part 2 (first solution)") {
      part2a(input: _*) shouldBe answer2
    }

    it("should match the puzzle description for part 2 (second solution)") {
      part2b(input: _*) shouldBe answer2
    }
  }
}
