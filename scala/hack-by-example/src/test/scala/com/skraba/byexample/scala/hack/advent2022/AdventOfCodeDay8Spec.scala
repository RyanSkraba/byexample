package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.Random

/** =Advent of Code 2022 Day 8 Solutions in scala=
  *
  * Input: A map of tree heights in characters
  *
  * Part 1: Count the trees that have a clear view to the edge (all trees
  * between them and the edge of the map are strictly shorter)
  *
  * Part 2: Calculate the maximum scenery score in the map. From any tree, the
  * scenery score is the product of the number of trees between it and the
  * closest bigger-or-same tree to the north/south/east/west.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/8]]
  */
class AdventOfCodeDay8Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    def isVisibleNaive(in: String*): (Int, Int) => Boolean = {
      val maxR = in.length - 1
      val maxC = in.headOption.map(_.length - 1).getOrElse(0)
      (r, c) =>
        c == 0 || r == 0 || c == maxC || r == maxR || (0 until c).forall(
          in(r)(_) < in(r)(c)
        ) ||
          (c + 1 to maxC).forall(in(r)(_) < in(r)(c)) ||
          (0 until r).forall(in(_)(c) < in(r)(c)) ||
          (r + 1 to maxR).forall(in(_)(c) < in(r)(c))
    }

    def isVisibleWithMemo(in: String*): (Int, Int) => Boolean = {
      // The largest tree to the west or east of any position (not including that position)
      val w = in.map(_.scanLeft('0')(Seq(_, _).max))
      // East is padded, add one to the column
      val e = in.map(_.scanRight('0')(Seq(_, _).max))

      // Note that rows and columns are transposed for north and south
      val inT = in.transpose
      val n = inT.map(_.scanLeft('0')(Seq(_, _).max))
      // South is padded, add one to the row
      val s = inT.map(_.scanRight('0')(Seq(_, _).max))

      val maxR = in.length - 1
      val maxC = in.headOption.map(_.length - 1).getOrElse(0)

      (r, c) =>
        c == 0 || r == 0 || c == maxC || r == maxR ||
          Seq(w(r)(c), e(r)(c + 1), n(c)(r), s(c)(r + 1)).min < in(r)(c)
    }

    def visibilityMap(w: Int, h: Int, f: (Int, Int) => Boolean): String = {
      (for (c <- 0 until w)
        yield (for (r <- 0 until h)
          yield if (f(r, c)) '*' else ' ').mkString).mkString("\n")
    }

    def visibilityCount(w: Int, h: Int, f: (Int, Int) => Boolean): Long = {
      (for (r <- 0 until h; c <- 0 until w if f(r, c)) yield true).size
    }

    def maxSceneryScore(in: String*): Long = {
      val dx = in.length - 1
      val dy = in.head.length - 1
      (for (x <- 1 until dx; y <- 1 until dy) yield {
        val up = (1 until x).reverse.takeWhile(in(_)(y) < in(x)(y)).size + 1
        val down = (x + 1 until dx).takeWhile(in(_)(y) < in(x)(y)).size + 1
        val left = (1 until y).reverse.takeWhile(in(x)(_) < in(x)(y)).size + 1
        val right = (y + 1 until dy).takeWhile(in(x)(_) < in(x)(y)).size + 1
        up * down * left * right
      }).max
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """30373
        |25512
        |65332
        |33549
        |35390
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description") {
      visibilityCount(
        input.length,
        input.head.length,
        isVisibleNaive(input: _*)
      ) shouldBe 21
      visibilityCount(
        input.length,
        input.head.length,
        isVisibleWithMemo(input: _*)
      ) shouldBe 21
      maxSceneryScore(input: _*) shouldBe 8
    }
  }

  describe("Generated cases") {

    for (i <- 0 to 100) {

      val rnd = new Random(i)
      val w = rnd.nextInt(100)
      val h = rnd.nextInt(100)
      val example: Seq[String] =
        for (_ <- 0 until w)
          yield rnd.alphanumeric.filter(_.isDigit).take(h).mkString

      it(s"should match for seed $i ($h, $w)") {
        val map1 = visibilityMap(h, w, isVisibleNaive(example: _*))
        val map2 = visibilityMap(h, w, isVisibleWithMemo(example: _*))
        map1 shouldBe map2
      }
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day8Input.txt").filter(_.nonEmpty)
    it("should have answers") {
      visibilityCount(
        input.length,
        input.head.length,
        isVisibleNaive(input: _*)
      ) shouldBe 1818
      visibilityCount(
        input.length,
        input.head.length,
        isVisibleWithMemo(input: _*)
      ) shouldBe 1818
      maxSceneryScore(input: _*) shouldBe 368368
    }
  }
}
