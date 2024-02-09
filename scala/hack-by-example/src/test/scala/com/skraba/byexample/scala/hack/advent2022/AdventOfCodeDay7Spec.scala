package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable
import scala.util.matching.Regex

/** =Advent of Code 2022 Day 7 Solutions in scala=
  *
  * Input: A console output listing files in the filesystem
  *
  * Part 1: Get the size of every subdirectory and count the sum of all of them with size> 100000 (note that this counts
  * some files more than once).
  *
  * Part 2: If the total disk size is 7M bytes and we need 3M bytes available, what is the smallest subdirectory that
  * can be deleted to free up enough space?
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/7]]
  */
class AdventOfCodeDay7Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    val ReCd: Regex = """\$ cd (.+)""".r
    val ReFile: Regex = """(\d+) (.+)""".r

    case class Dir(
        name: String,
        size: Long = 0,
        files: Seq[(String, Long)] = Nil,
        subs: Seq[Dir] = Nil
    ) {
      def flatten(): Seq[Dir] = this +: subs.flatMap(_.flatten())
    }

    def parse(stdin: mutable.Buffer[String], in: Dir = Dir("")): Dir = {
      var d = in
      while (stdin.nonEmpty) {
        stdin.remove(0) match {
          case ReCd(name) if name == ".." => return d
          case ReCd(name) =>
            val sub = parse(stdin, Dir(name))
            d = d.copy(
              size = d.size + sub.size,
              subs = d.subs :+ sub
            )
          case ReFile(size, name) =>
            d = d.copy(
              size = d.size + size.toLong,
              files = d.files :+ (name, size.toLong)
            )
          case _ => // Ignore other commands as unnecessary
        }
      }
      d
    }

    def part1(dirs: Seq[Dir], n: Long): Long = {
      dirs.foldLeft(0L) { (sum, d) =>
        (if (d.size > n) sum else sum + d.size) + part1(d.subs, n)
      }
    }

    def part2(root: Dir, minSize: Long): Long = {
      root.flatten().filter(_.size >= minSize).minBy(_.size).size
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """$ cd /
        |$ ls
        |dir a
        |14848514 b.txt
        |8504156 c.dat
        |dir d
        |$ cd a
        |$ ls
        |dir e
        |29116 f
        |2557 g
        |62596 h.lst
        |$ cd e
        |$ ls
        |584 i
        |$ cd ..
        |$ cd ..
        |$ cd d
        |$ ls
        |4060174 j
        |8033020 d.log
        |5626152 d.ext
        |7214296 k
        |""".stripMargin.split("\n")

    it("should match the puzzle description") {
      // Drop the first artificial Dir
      val root = parse(input.toBuffer).subs.head
      root.name shouldBe "/"
      root.size shouldBe 48381165
      root.files should have size 2
      root.files.head shouldBe ("b.txt", 14848514)
      root.files(1) shouldBe ("c.dat", 8504156)
      root.subs should have size 2

      root.subs.head.name shouldBe "a"
      root.subs.head.size shouldBe 94853
      root.subs.head.files should have size 3
      root.subs.head.files.map(_._1) shouldBe Seq("f", "g", "h.lst")
      root.subs.head.files.map(_._2) shouldBe Seq(29116, 2557, 62596)
      root.subs.head.subs shouldBe Seq(Dir("e", 584, List(("i", 584))))

      root.subs(1).name shouldBe "d"
      root.subs(1).size shouldBe 24933642
      root.subs(1).files.map(_._1) shouldBe Seq("j", "d.log", "d.ext", "k")
      root.subs(1).files.map(_._2) shouldBe Seq(
        4060174,
        8033020,
        5626152,
        7214296
      )
      root.subs(1).subs shouldBe empty

      part1(root.subs, 100000) shouldBe 95437
      part2(root, 30000000 - (70000000 - root.size)) shouldBe 24933642
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day7Input.txt")
    it("should have answers") {
      val root = parse(input.toBuffer).subs.head
      part1(root.subs, 100000) shouldBe decryptLong("PmSVPFReCpuUfoUxHnatzQ==")

      root.size shouldBe decryptLong("b3TkteyegwfIDDAmeKcOUg==")
      part2(
        root,
        30000000 - (70000000 - root.size)
      ) shouldBe decryptLong("zTbCXe3N/Av04hE13TdiMw==")
    }
  }
}
