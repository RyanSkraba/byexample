package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 22 Solutions in scala=
  *
  * Input:
  *
  * Part 1:
  *
  * Part 2:
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/22]]
  */
class AdventOfCodeDay22Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    type Plan = Seq[String]

    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }

    import Dir._

    case class Pos(x: Int = 0, y: Int = 0, facing: Dir = East, plan: Plan) {

      lazy val c: Char = if (x >= 0 && y >= 0) plan(y)(x) else ' '

      // From this point, find the eastern wrapped point such that its west is my first wrapped around west.
      lazy val wrapE: Pos = copy(x = plan(y).indexWhere(_ == ' ', x))
      lazy val wrapS: Pos = copy(y = plan.indexWhere(_(x) == ' ', y))
      lazy val wrapW: Pos = copy(x = plan(y).lastIndexWhere(_ == ' ', x))
      lazy val wrapN: Pos = copy(y = plan.lastIndexWhere(_(x) == ' ', y))

      // If there's a point to the east, then return it (wrapping if necessary) or None if we can't move that direction
      lazy val e: Option[Pos] =
        Some(copy(x = x + 1))
          .filter(_.c != '#')
          .flatMap(p => if (p.c == ' ') wrapW.e else Some(p))

      lazy val s: Option[Pos] = Some(copy(y = y + 1))
        .filter(_.c != '#')
        .flatMap(p => if (p.c == ' ') wrapN.s else Some(p))

      lazy val w: Option[Pos] = Some(copy(x = x - 1))
        .filter(_.c != '#')
        .flatMap(p => if (p.c == ' ') wrapE.w else Some(p))

      lazy val n: Option[Pos] = Some(copy(y = y - 1))
        .filter(_.c != '#')
        .flatMap(p => if (p.c == ' ') wrapS.n else Some(p))

      // Move one movement in the direction we're facing, or None if blocked
      lazy val mv1: Option[Pos] =
        if (facing == North) n
        else if (facing == East) e
        else if (facing == South) s
        else w

      def move(adv: Int): Pos = {
        if (adv <= 0) this
        else mv1.map(_.move(adv - 1)).getOrElse(this)
      }
    }

    def parse(in: String*): Plan = {
      // Pad each line so the plan is a square and there's whitespace to the left and bottom of any place
      val width = 1 + in.maxBy(_.length).length
      in.map(row => row + " " * (width - row.length)) :+ (" " * width)
    }

    def part1(in: String*): Long = {
      val plan = parse(in.dropRight(1): _*)
      val pos0 = Pos(plan = plan, x = plan.head.indexOf('.'))

      val instructions: Seq[Either[Int, Char]] =
        in.last.split("((?<=\\D)|(?=\\D))").filter(_.nonEmpty).map {
          case adv if adv.head.isDigit => Left(adv.toInt)
          case turn                    => Right(turn.head)
        }

      val posEnd = instructions.foldLeft(pos0) {
        case (pos, Left(adv)) => pos.move(adv)
        case (pos, Right(turn)) if turn == 'R' =>
          pos.copy(facing = Dir((pos.facing.id + 1 + Dir.maxId) % Dir.maxId))
        case (pos, Right(turn)) if turn == 'L' =>
          pos.copy(facing = Dir((pos.facing.id - 1 + Dir.maxId) % Dir.maxId))
      }

      1000 * (posEnd.y + 1) + 4 * (posEnd.x + 1) + posEnd.facing.id
    }

    def part2(in: String*): Long = 200
  }

  import Solution._
  import Solution.Dir._

  describe("Example case") {
    val input =
      """        ...#
        |        .#..
        |        #...
        |        ....
        |...#.......#
        |........#...
        |..#....#....
        |..........#.
        |        ...#....
        |        .....#..
        |        .#......
        |        ......#.
        |
        |10R5L5R10L4R5L5
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should move around the board") {
      val plan = parse(input.dropRight(1): _*)

      // 16 wide and 12 tall
      // 0123456789012345
      //         0..#     0
      //         .#..     1
      //         #...     2
      //         ....     3
      // ...#.......#     4
      // ........#...     5
      // ..#....#....     6
      // ..........#.     7
      //         ...#.... 8
      //         .....#.. 9
      //         .#..1... 10
      //         ......#2 11

      val pos0 = Pos(plan = plan, x = plan.head.indexOf('.'))
      pos0.wrapE shouldBe pos0.copy(x = pos0.x + 4)
      pos0.wrapS shouldBe pos0.copy(y = pos0.y + 12)
      pos0.wrapW shouldBe pos0.copy(x = pos0.x - 1)
      pos0.wrapN shouldBe pos0.copy(y = pos0.y - 1)
      pos0.e shouldBe Some(pos0.copy(x = pos0.x + 1))
      pos0.s shouldBe Some(pos0.copy(y = pos0.y + 1))
      pos0.w shouldBe None
      pos0.n shouldBe Some(pos0.copy(y = 11))

      val pos1 = pos0.copy(x = 12, y = 10)
      pos1.wrapE shouldBe pos1.copy(x = 16)
      pos1.wrapS shouldBe pos1.copy(y = 12)
      pos1.wrapW shouldBe pos1.copy(x = 7)
      pos1.wrapN shouldBe pos1.copy(y = 7)
      pos1.e shouldBe Some(pos1.copy(x = pos1.x + 1))
      pos1.s shouldBe Some(pos1.copy(y = pos1.y + 1))
      pos1.w shouldBe Some(pos1.copy(x = pos1.x - 1))
      pos1.n shouldBe Some(pos1.copy(y = pos1.y - 1))
    }

    it("should match the puzzle description") {
      part1(input: _*) shouldBe 6032
      part2(input: _*) shouldBe 200
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day22Input.txt")
    it("should have answers") {
      part1(input: _*) shouldBe 164014
      part2(input: _*) shouldBe 200
    }
  }
}
