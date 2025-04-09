package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2023 Day 5 Solutions in scala=
  *
  * Input: Lists of numeric range mappings that transform (for example) seed ids to soil ids, then soil ids to
  * fertilizer ids.
  *
  * Part 1: For a list of seeds, apply all of the functions until you arrive at a location id, then select the smallest.
  *
  * Part 1: For a list of large seed _ranges_, apply all of the functions until you arrive at location ids, then select
  * the smallest.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/5]]
  */
class AdventOfCodeDay5Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    /** Parse the input strings into three data structures:
      *
      *   - The list of numbers corresponding to seeds in the first line.
      *   - A map of all the ranges found in the input, keyed on the source type.
      *   - A list of strings corresponding to the input types
      */
    def parse(
        in: String*
    ): (Seq[Long], Map[String, Array[Array[Long]]], Seq[String]) = {
      // First line is the seeds, the rest are maps
      val (seedLine, restLines) = in.splitAt(1)
      val maps = restLines
        .mkString("\n")
        .split("\n\n")
        .map(_.trim.split("\n").splitAt(1))
        .map { case (mapName, values) =>
          (
            mapName.head.split("(-to-|\\s)").take(2),
            values.map(_.split("\\s").map(_.toLong))
          )
        }

      // Clean up the seeds values
      val seeds = seedLine.head.split("\\s").toSeq.drop(1).map(_.toLong)

      // Get the maps by name
      val mapByName = maps.map(x => (x._1.head, x._2)).toMap
      val mapTo = maps.map(x => (x._1.head, x._1(1))).toMap
      val mapNames = LazyList
        .iterate(Option("seed"))(_.flatMap(mapTo.get))
        .takeWhile(_.nonEmpty)
        .flatten

      (seeds, mapByName, mapNames)
    }

    /** For part 1, given an input number and a map of ranges, find the output number
      */
    def part1FindInRange(in: Long, ranges: Array[Array[Long]]): Long = {
      for (range <- ranges if in >= range(1) && in < range(1) + range(2)) {
        return in - range(1) + range.head
      }
      in
    }

    def part1(in: String*): Long = {
      val (seeds, mapByName, mapNames) = parse(in: _*)
      val lookups = mapNames.dropRight(1)
      val locations =
        for (seed <- seeds) yield lookups.foldLeft(seed) { (acc, in) =>
          part1FindInRange(acc, mapByName(in))
        }
      locations.min
    }

    /** Helper object to store an interval. If an integer is in this interval, this can be applied as a function to map
      * it from the source interval to the destination interval.
      * @param src
      *   The start index in the interval
      * @param len
      *   The length of the interval
      * @param dst
      *   If being used as a map, the start index of the destination interval
      */
    case class Interval(src: Long, len: Long, dst: Long) {

      /** The last position of the interval. */
      lazy val srcLast: Long = src + len - 1

      def inSrc(in: Long): Boolean = in >= src && in <= srcLast

      def apply(in: Long): Long = if (inSrc(in)) in - src + dst else in
    }

    /** Helper object that stores a sequence of intervals. */
    case class Intervals(ivs: Seq[Interval]) {

      def apply(in: Long): Long = {
        for (iv <- ivs) if (iv.inSrc(in)) return iv(in)
        in
      }

      def map(
          in: (Long, Long),
          iv: Seq[Interval] = ivs,
          acc: Set[(Long, Long)] = Set.empty
      ): Set[(Long, Long)] = {
        // Stop recursion if the input interval doesn't make sense
        if (iv.isEmpty || in._2 < iv.head.src) acc + in
        else if (in._1 > iv.head.srcLast) map(in, iv.tail, acc)
        else if (in._1 < iv.head.src)
          map(iv.head.src -> in._2, iv, acc + (in._1 -> (iv.head.src - 1)))
        else if (in._1 >= iv.head.src && in._2 <= iv.head.srcLast)
          acc + (iv.head(in._1) -> iv.head(in._2))
        else
          map(
            iv.head.srcLast + 1 -> in._2,
            iv.tail,
            acc + (iv.head(in._1) -> iv.head(iv.head.srcLast))
          )
      }
    }

    def from(in: Array[Array[Long]]): Intervals = {
      Intervals(in.map((iv: Array[Long]) => Interval(iv(1), iv(2), iv.head)).sortBy(_.src).toSeq)
    }

    def part2slow(in: String*): Long = {
      val (seeds, mapByName, _) = parse(in: _*)
      val locations =
        for (
          seedRange <- seeds.grouped(2);
          seed <- seedRange.head until (seedRange.head + seedRange(1))
        ) yield {
          val soil = part1FindInRange(seed, mapByName("seed"))
          val fertilizer = part1FindInRange(soil, mapByName("soil"))
          val water = part1FindInRange(fertilizer, mapByName("fertilizer"))
          val light = part1FindInRange(water, mapByName("water"))
          val temperature = part1FindInRange(light, mapByName("light"))
          val humidity = part1FindInRange(temperature, mapByName("temperature"))
          val location = part1FindInRange(humidity, mapByName("humidity"))
          location
        }
      locations.min
    }

    def part2fast(in: String*): Long = {
      val (seeds, mapByName, _) = parse(in: _*)
      val intervals = mapByName.view.mapValues(from).toMap
      val locations = for (seed <- seeds.grouped(2)) yield {
        val soil = intervals("seed").map(seed.head -> (seed.head + seed(1) - 1))
        val fertilizer = soil.flatMap(iv => intervals("soil").map(iv))
        val water = fertilizer.flatMap(iv => intervals("fertilizer").map(iv))
        val light = water.flatMap(iv => intervals("water").map(iv))
        val temperature = light.flatMap(iv => intervals("light").map(iv))
        val humidity =
          temperature.flatMap(iv => intervals("temperature").map(iv))
        val location = humidity.flatMap(iv => intervals("humidity").map(iv))
        location.minBy(_._1)._1
      }

      locations.min
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """seeds: 79 14 55 13
        |
        |seed-to-soil map:
        |50 98 2
        |52 50 48
        |
        |soil-to-fertilizer map:
        |0 15 37
        |37 52 2
        |39 0 15
        |
        |fertilizer-to-water map:
        |49 53 8
        |0 11 42
        |42 0 7
        |57 7 4
        |
        |water-to-light map:
        |88 18 7
        |18 25 70
        |
        |light-to-temperature map:
        |45 77 23
        |81 45 19
        |68 64 13
        |
        |temperature-to-humidity map:
        |0 69 1
        |1 0 69
        |
        |humidity-to-location map:
        |60 56 37
        |56 93 4
        |""".trimSplit

    it("should parse and find seeds") {
      val (seeds, mapByName, mapNames) = parse(input: _*)
      val seedMap: Array[Array[Long]] = mapByName("seed")

      seeds shouldBe Seq(79, 14, 55, 13)
      mapNames shouldBe Seq(
        "seed",
        "soil",
        "fertilizer",
        "water",
        "light",
        "temperature",
        "humidity",
        "location"
      )

      part1FindInRange(97, seedMap) shouldBe 99
      part1FindInRange(98, seedMap) shouldBe 50
      part1FindInRange(99, seedMap) shouldBe 51
      part1FindInRange(100, seedMap) shouldBe 100

      part1FindInRange(79, seedMap) shouldBe 81
      part1FindInRange(14, seedMap) shouldBe 14
      part1FindInRange(55, seedMap) shouldBe 57
      part1FindInRange(13, seedMap) shouldBe 13
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 35
    }

    it("should pass through ranges that do not overlap") {
      // Original interval contains one mapping.  All three digit numbers have 10K added.
      val a1 = Intervals(Seq(Interval(100, 900, 10100)))

      // simple values
      a1(55) shouldBe 55
      a1(100) shouldBe 10100
      a1(365) shouldBe 10365
      a1(999) shouldBe 10999
      a1(1000) shouldBe 1000

      // ranges that are not mapped
      a1.map(55L -> 95L) shouldBe Set(55 -> 95)
      a1.map(1000L -> 10000L) shouldBe Set(1000 -> 10000)
    }

    it("should pass through ranges that overlap") {
      // Original interval contains one mapping.  All three digit numbers have 10K added.
      val a1 = Intervals(Seq(Interval(100, 900, 10100)))

      // ranges that are not mapped
      a1.map(55L -> 195L) shouldBe Set(55 -> 99, 10100 -> 10195)
      a1.map(55L -> 1195L) shouldBe Set(55 -> 99, 10100 -> 10999, 1000 -> 1195)
      a1.map(955L -> 1195L) shouldBe Set(10955 -> 10999, 1000 -> 1195)
    }

    it("should match the puzzle description for part 2") {
      part2slow(input: _*) shouldBe 46
      part2fast(input: _*) shouldBe 46
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day5Input.txt")
    lazy val answer1 = decryptLong("qzS7PbnCytJLt5nckUT0RA==")
    lazy val answer2 = decryptLong("cwnqRZnKFFxlXowJtx+mVA==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2fast(input: _*) shouldBe answer2
    }
  }
}
