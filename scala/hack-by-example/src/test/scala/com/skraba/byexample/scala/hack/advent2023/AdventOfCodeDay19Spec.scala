package com.skraba.byexample.scala.hack.advent2023

import com.skraba.byexample.scala.hack.advent2023.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** =Advent of Code 2023 Day 19 Solutions in scala=
  *
  * Input: A list of workflow functions, each with a name, that take a part and
  * check its ratings to decide whether it is accepted ("A"), rejected ("R") or
  * applied to another workflow function, followed by a list of parts and their
  * ratings.
  *
  * Part 1: Apply the functions to each of the parts to see if it's accepted or
  * rejected. Return the sum of all the ratings of all the accepted parts.
  *
  * Part 2: Ignoring the list of parts, find how many different combinations of
  * ratings could possibly be accepted (where the ratings are from 1 to 4000).
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2023/day/19]]
  */
class AdventOfCodeDay19Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    type Part = Map[Char, Long]
    type Fn = Part => String

    val FnRegex: Regex = raw"(.+)([<>])(\d+):(.+)".r

    def parse(in: String*): (Map[String, Seq[Seq[String]]], Seq[Part]) = {
      val (fnLines, partLines) = in.span(_.nonEmpty)

      val fn: Map[String, Seq[Seq[String]]] =
        fnLines
          .filter(_.nonEmpty)
          .map(_.split("[{},]").splitAt(1))
          .map { case (Array(head), fns) =>
            head ->
              fns.toSeq
                .map {
                  case FnRegex(attr, op, value, next) =>
                    Seq(attr, op, value, next)
                  case next => Seq(next)
                }
          }
          .toMap

      val part = partLines
        .filter(_.nonEmpty)
        .map(
          _.split("[{}=,]")
            .filter(_.nonEmpty)
            .grouped(2)
            .map(rating => rating.head.head -> rating(1).toLong)
            .toMap
        )

      (fn, part)
    }

    def part1(in: String*): Long = {

      val (fn, part) = parse(in: _*)

      // Parse the input into scala functions
      def go(fns: Seq[Part => Option[String]])(p: Part): String =
        fns.flatMap(_(p)).head
      val op: Map[String, Fn] = fn
        .map(x =>
          (
            x._1,
            go(
              x._2
                .map {
                  case Seq(attr, "<", value, next) =>
                    p: Part =>
                      if (p(attr.head) < value.toLong) Some(next) else None
                  case Seq(attr, ">", value, next) =>
                    p: Part =>
                      if (p(attr.head) > value.toLong) Some(next) else None
                  case Seq(next) => _: Part => Some(next)
                }
            )
          )
        )

      // For each part, find if the function returns an "A" or an "R".  This
      // relies on all workflow functions having a name longer than one
      // character, and the end states only being one character.
      val accepted = part.filter { p =>
        LazyList.iterate("in") { op(_)(p) }.dropWhile(_.length > 1).head == "A"
      }
      accepted.flatMap(_.values).sum
    }

    def part2(in: String*): Long = {
      val (fn, _) = parse(in: _*)

      /** The workflows are basically a tree where each edge is a constraint.
        * Work down the tree to find the most restrictive rating constraints
        * when you arrive at A
        * @param name
        *   The name of the current node to find the total possibilities of
        *   arriving at A below.
        * @param min
        *   The minimum value for each rating at the current node.
        * @param max
        *   The maximum value for each rating at the current node.
        * @return
        *   The maximum number of possible rating values that could arrive at an
        *   A below this node.
        */
      def dfs(
          name: String,
          min: Map[String, Int] = Map().withDefaultValue(1),
          max: Map[String, Int] = Map().withDefaultValue(4000)
      ): Long = {
        if (min.exists(x => max(x._1) < x._2)) {
          // If this node has an impossible constraint on a rating, trim it.
          0
        } else if (name == "A") {
          // If this node is an A, count the possible ratings that meet the constraints.
          "xmas"
            .map(_.toString)
            .map(attr => max(attr) - min(attr) + 1L)
            .product
        } else if (name == "R") 0
        else {
          // Each workflow functions at this node is an edge, so recurse into that edge.
          // The foldLeft collects the total and the rating constraints that should apply
          // to any *after* the one being looked at.
          fn(name)
            .foldLeft((0L, min, max)) {
              case ((total, rMin, rMax), Seq(attr, ">", value, next)) =>
                // To follow this edge, THIS node needs a new minimum for the given
                // attribute, while the remaining nodes need a new maximum
                val minNew =
                  rMin + (attr -> math.max(value.toInt + 1, rMin(attr)))
                val rMax2 = rMax + (attr -> math.min(value.toInt, rMax(attr)))
                (total + dfs(next, minNew, rMax), rMin, rMax2)
              case ((total, mn, mx), Seq(attr, "<", value, next)) =>
                val rMin2 = mn + (attr -> math.max(value.toInt, mn(attr)))
                val maxNew = mx + (attr -> math.min(value.toInt - 1, mx(attr)))
                (total + dfs(next, mn, maxNew), rMin2, mx)
              case ((total, rMin, rMax), Seq(next)) =>
                (total + dfs(next, rMin, rMax), rMin, rMax)
            }
            ._1
        }
      }

      dfs("in")
    }
  }

  import Solution._

  describe("Example case") {
    val input =
      """px{a<2006:qkq,m>2090:A,rfg}
        |pv{a>1716:R,A}
        |lnx{m>1548:A,A}
        |rfg{s<537:gd,x>2440:R,A}
        |qs{s>3448:A,lnx}
        |qkq{x<1416:A,crn}
        |crn{x>2662:A,R}
        |in{s<1351:px,qqz}
        |qqz{s>2770:qs,m<1801:hdj,R}
        |gd{a>3333:R,R}
        |hdj{m>838:A,pv}
        |
        |{x=787,m=2655,a=1222,s=2876}
        |{x=1679,m=44,a=2067,s=496}
        |{x=2036,m=264,a=79,s=2244}
        |{x=2461,m=1339,a=466,s=291}
        |{x=2127,m=1623,a=2188,s=1013}
        |
        |""".trim.stripMargin.split("\n")

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 19114
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 167409079868000L
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day19Input.txt")
    lazy val answer1 = decryptLong("kT98XKz8OgDXtxSh5VTabQ==")
    lazy val answer2 = decryptLong("W+pntYthqUVxjL31+ytOKw==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
