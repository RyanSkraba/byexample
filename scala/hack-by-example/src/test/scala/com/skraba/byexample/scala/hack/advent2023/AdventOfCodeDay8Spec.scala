package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 8 Solutions in scala=
  *
  * Input: A string containing directions (in the form of Ls and Rs), followed by a map of nodes (three letters) that
  * each have a right path and a left path that they go to.
  *
  * Part 1: Starting at AAA follow the directions until you reach ZZZ. Count the number of steps required to get there.
  *
  * Part 2: Putting a ghost at every node that ends with an A, each of them follow the directions until they all are
  * simultaneously at a node that ends with Z. Count the number of steps until this is true.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/8]]
  */
class AdventOfCodeDay8Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** Parses the input
      * @return
      *   Two structures: the string containing the directions (exclusively L and R) and a map keyed on string position
      *   names, returning a map keyed on direction character and the next position that direction takes you.
      */
    def parse(in: String*): (String, Map[String, Map[Char, String]]) = {
      (
        in.head,
        in.tail
          .filter(_.nonEmpty)
          .map(_.split("[\\s(,)=]+"))
          .map(xs => xs.head -> Map('L' -> xs(1), 'R' -> xs(2)))
          .toMap
      )
    }

    def part1(in: String*): Long = {
      val (dir, map) = parse(in: _*)
      // Start at position "AAA"', iterate through the map
      val path = LazyList.iterate(("AAA", 0L)) { case (pos, total) =>
        (map(pos)(dir((total % dir.length).toInt)), total + 1)
      }
      // Drop the entire path
      path.dropWhile(_._1 != "ZZZ").head._2
    }

    def part2slow(in: String*): Long = {
      val (dir, map) = parse(in: _*)
      // The starting position of the ghosts
      val starting = map.keys.filter(_.endsWith("A"))
      // This is the same logic as part1 but updating ALL starting positions.
      val path = LazyList.iterate((starting, 0L)) { case (pos, total) =>
        (pos.map(p => map(p)(dir((total % dir.length).toInt))), total + 1)
      }
      path.dropWhile(_._1.exists(!_.endsWith("Z"))).head._2
    }

    def part2analysis(in: String*): Long = {
      val (dir, map) = parse(in: _*)

      println(s"There are ${dir.length} instructions in the path")

      // Get all of the paths for the ghosts as in part1, but don't evaluate them yet
      val starting = map.keys.filter(_.endsWith("A")).toSeq.sorted
      val paths = for (start <- starting) yield LazyList.iterate((start, 0L)) { case (pos, total) =>
        (map(pos)(dir((total % dir.length).toInt)), total + 1)
      }

      // Path lengths
      val pathLengths = for (path <- paths) yield {
        val end = path.dropWhile(!_._1.endsWith("Z"))
        val total = end.head._2
        println(
          s"Ghost starting at ${path.head._1} finishes at ${end.head._1} in $total moves, next stop ${end(1)._1}"
        )
        // Huh, the start node and the end node have the same destinations in reversed order
        println("    " + map(path.head._1))
        println("    " + map(end.head._1))
        total
      }

      // What if you only take right turns?
      val rpaths = for (start <- starting) yield LazyList.iterate((start, 0L)) { case (pos, total) =>
        (map(pos)('R'), total + 1)
      }
      val rpathLengths = rpaths.map(_.dropWhile(!_._1.endsWith("Z")).head._2)

      println("With paths: " + pathLengths)
      println("     Right: " + rpathLengths)

      for (path <- paths) {
        val duplicates =
          path.scanLeft((Seq.empty[String], Option[(String, Long)](null))) {
            case (acc, x) if !acc._1.contains(x._1) => (acc._1 :+ x._1, None)
            case (acc, x)                           => (acc._1, Some(x))
          }

        val firstDuplicate = duplicates.find(_._2.nonEmpty).get

        println(
          s"Ghost starting at ${path.head._1} has first duplicate at ${firstDuplicate._2}"
        )
      }

      // All of the path lengths are divisible by the length of the instructions, and they're all primes...
      val pathLengthDiv = pathLengths.map(_.toDouble / dir.length)
      println(s"Loops around directions: $pathLengthDiv")
      (dir.length * pathLengthDiv.product).toLong
    }

    def part2(in: String*): Long = {
      val (dir, map) = parse(in: _*)
      // Get all of the paths for the ghosts as in part1 individually
      val starting = map.keys.filter(_.endsWith("A")).toSeq.sorted
      val paths = for (start <- starting) yield LazyList.iterate((start, 0L)) { case (pos, total) =>
        (map(pos)(dir((total % dir.length).toInt)), total + 1)
      }
      // The individual path lengths FOR THIS CASE are all multiples of the direction length, and prime.
      val lengths =
        for (path <- paths) yield path.dropWhile(!_._1.endsWith("Z")).head._2
      dir.length * lengths.map(_ / dir.length).product
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """RL
        |
        |AAA = (BBB, CCC)
        |BBB = (DDD, EEE)
        |CCC = (ZZZ, GGG)
        |DDD = (DDD, DDD)
        |EEE = (EEE, EEE)
        |GGG = (GGG, GGG)
        |ZZZ = (ZZZ, ZZZ)
        |""".trim.stripMargin.split("\n")

    val input2 =
      """LLR
        |
        |AAA = (BBB, BBB)
        |BBB = (AAA, ZZZ)
        |ZZZ = (ZZZ, ZZZ)
        |""".trim.stripMargin.split("\n")

    val input3 =
      """LR
        |
        |11A = (11B, XXX)
        |11B = (XXX, 11Z)
        |11Z = (11B, XXX)
        |22A = (22B, XXX)
        |22B = (22C, 22C)
        |22C = (22Z, 22Z)
        |22Z = (22B, 22B)
        |XXX = (XXX, XXX)
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 2
      part1(input2: _*) shouldBe 6
    }

    it("should match the puzzle description for part 2") {
      part2slow(input3: _*) shouldBe 6
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day8Input.txt")
    lazy val answer1 = decryptLong("f4mowYe7byDUnMPCAXjwPQ==")
    lazy val answer2 = decryptLong("5z0YTtoGBun9E/nppFeL+A==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
