package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** =Advent of Code 2024 Day 9 Solutions in scala=
  *
  * Input: A string of digits that denotes a hard disk usage. Every two characters is a file denoting the number of
  * filled blocks the file takes, and the number of empty blocks after it.
  *
  * Part 1: Take filled blocks from the last files and move them to the empty spaces between the first files until all
  * the blocks are filled contiguously. Calculate the checksum: for each block, multiply its position by the original
  * file number it came from (both positions and files are zero-indexed).
  *
  * Part 2: Avoid fragmenting files! Take the last file and move it to the first space where it fits (if any). Repeat
  * until all files have been moved at MOST once, and calculate the checksum.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/9]]
  */
class AdventOfCodeDay9Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    case class File(id: Int, filled: Int, space: Int)

    def parse(in: String): Seq[File] = in
      .grouped(2)
      .zipWithIndex
      .map { case (s, i) => File(i, s.head - '0', s.last - '0') }
      .toSeq

    def part1(in: String): Long = {
      case class Acc(disk: Seq[File], pos: Long = 0L, sum: Long = 0L)
      val ll = LazyList
        .iterate(Acc(parse(in))) {
          // If the last block doesn't have any filled spaces, drop it.
          case Acc(disk, pos, sum) if disk.last.filled == 0 => Acc(disk.init, pos, sum)
          // If the last block doesn't have any filled spaces, drop it.
          case Acc(File(_, 0, 0) :: rest, pos, sum) => Acc(rest, pos, sum)
          // If the first block has a filled space, add it to the position and sum.
          case Acc(File(id, filled, space) :: rest, pos, sum) if filled != 0 =>
            Acc(File(id, filled - 1, space) :: rest, pos + 1, sum + id * pos)
          // If the first block has no filled space, but has empty spaces, get the last filled block.
          case Acc(File(id, 0, space) :: rest, pos, sum) =>
            val last = rest.last
            Acc(
              File(id, 0, space - 1) +: rest.init :+ File(last.id, last.filled - 1, last.space),
              pos + 1,
              sum + last.id * pos
            )
        }
      ll.dropWhile(_.disk.nonEmpty).head.sum
    }

    /** Add the n integers starting from start */
    def sumn(start: Long, n: Int): Long = n * start + n * (n - 1) / 2

    /** Same as part1 but the current position jumps ahead and uses Vector for fast last access. */
    def part1quick(in: String): Long = {
      case class Acc(disk: Vector[File], pos: Long = 0L, sum: Long = 0L)
      val ll = LazyList
        .iterate(Acc(parse(in).toVector)) {
          // If the last block doesn't have any filled spaces, drop it.
          case Acc(disk, pos, sum) if disk.last.filled == 0 => Acc(disk.init, pos, sum)
          // If the first block has no length (either filled or spaces), drop it.
          case Acc(disk @ Seq(File(_, 0, 0), _*), pos, sum) => Acc(disk.tail, pos, sum)
          // If the first block has a filled space, add it to the position and sum.
          case Acc(disk @ Seq(File(id, filled, space), _*), pos, sum) if filled != 0 =>
            Acc(File(id, 0, space) +: disk.tail, pos + filled, sum + id * sumn(pos, filled))
          // If the first block has no filled space, but has empty spaces, get the last filled block.
          case Acc(disk @ Seq(File(id, 0, space), _*), pos, sum) =>
            val last = disk.tail.last
            val moved = last.filled min space
            Acc(
              File(id, 0, space - moved) +: disk.tail.init :+ File(last.id, last.filled - moved, last.space),
              pos + moved,
              sum + last.id * sumn(pos, moved)
            )
        }
      ll.dropWhile(_.disk.nonEmpty).head.sum
    }

    def part2(in: String): Long = {

      /** Adapt the input to include the original position of each block. */
      case class FileWithPos(id: Int, filled: Int, space: Int, pos: Int) {
        lazy val checksum: Long = id * sumn(pos, filled)
      }
      val bx: Vector[FileWithPos] = parse(in)
        .foldLeft(Seq.empty[FileWithPos]) { case (acc, b) =>
          acc :+ FileWithPos(b.id, b.filled, b.space, acc.lastOption.map(b => b.pos + b.space + b.filled).getOrElse(0))
        }
        .toVector

      /** Accumulate the sequence of blocks, the sum so far, and the maxId to consider moving */
      case class Acc(disk: Vector[FileWithPos], sum: Long = 0L, maxId: Int = Int.MaxValue)
      val ll = LazyList
        .iterate(Acc(bx)) {
          // If the last block has already been moved, then count it and drop it.
          case Acc(disk, sum, maxId) if disk.lastOption.exists(_.id >= maxId) =>
            Acc(disk.init, sum + disk.last.checksum, maxId)
          // If the space in the first block can't fit any other block, then it can be counted and dropped too.
          case Acc(disk @ Seq(b, _*), sum, maxId) if b.space == 0 || !disk.tail.exists(_.filled <= b.space) =>
            Acc(disk.tail, sum + b.checksum, maxId)
          // Otherwise either drop or move the last block to a new position.
          case Acc(disk, sum, _) =>
            val last = disk.last
            disk.init.indexWhere(_.space >= last.filled) match {
              case -1 => Acc(disk.init, sum + last.checksum, last.id)
              case fitsAt =>
                val fitsBlock = disk(fitsAt)
                val newDisk = disk.init.patch(
                  fitsAt,
                  Seq(
                    fitsBlock.copy(space = 0),
                    last.copy(space = fitsBlock.space - last.filled, pos = fitsBlock.pos + fitsBlock.filled)
                  ),
                  1
                )
                Acc(newDisk, sum, last.id)
            }
        }

      ll.dropWhile(_.disk.nonEmpty).head.sum
    }
  }

  import Solution._

  describe("Example case") {
    val input = "2333133121414131402"

    it("should match the puzzle description for part 1") {
      part1("5") shouldBe 0
      part1("12345") shouldBe 60
      part1(input) shouldBe 1928
      part1quick("5") shouldBe 0
      part1quick("12345") shouldBe 60
      part1quick(input) shouldBe 1928
    }

    it("should match the puzzle description for part 2") {
      part2(input) shouldBe 2858
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day9Input.txt").mkString("")
    lazy val answer1 = decryptLong("CEev5yLJB5BYB75Yv1kwBQ==")
    lazy val answer2 = decryptLong("qWWcpD6mu5P+WJV1r89Knw==")

    it("should have answers for part 1 (4 seconds)", Slow) {
      part1(input) shouldBe answer1
    }

    it("should have answers for part 1") {
      part1quick(input) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input) shouldBe answer2
    }
  }
}
