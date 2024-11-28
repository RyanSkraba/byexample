package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2022 Day 22 Solutions in scala=
  *
  * Input: A map with obstacles and instructions for moving around the map. The map is not square, but an unfolded box
  * (although we don't know that for part 1). Open spaces are periods, and blocked spaces are #. The last line is the
  * instructions to follow in the form of turns and number of spaces to advance.
  *
  * Part 1: The final coordinates after following the instructions, where we wrap around rows and columns.
  *
  * Part 2: The final coordinates after following the instructions, where we wrap the map around a box.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/22]]
  */
class AdventOfCodeDay22Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
    }
    import Dir._

    case class Plan(cc: Seq[String]) {
      var warp: Pos => Pos = p => p.wrapRowColumn

      def line(start: Int, fixed: Int, faceWidth: Int, dir: Dir): Seq[Pos] = {
        val fixedPos =
          if (dir == East || dir == South) fixed * faceWidth - 1
          else fixed * faceWidth

        if (dir == East || dir == West)
          (start * faceWidth until (start + 1) * faceWidth).map(y =>
            Pos(x = fixedPos, y = y, facing = dir, plan = this).advance
          )
        else
          (start * faceWidth until (start + 1) * faceWidth).map(x =>
            Pos(x = x, y = fixedPos, facing = dir, plan = this).advance
          )
      }
    }

    object Plan {}

    case class Pos(x: Int = 0, y: Int = 0, facing: Dir = East, plan: Plan) {

      lazy val c: Char = if (x >= 0 && y >= 0) plan.cc(y)(x) else ' '

      lazy val wrapRowColumn: Pos =
        if (facing == East) copy(x = plan.cc(y).lastIndexWhere(_ == ' ', x - 1))
        else if (facing == South)
          copy(y = plan.cc.lastIndexWhere(_(x) == ' ', y - 1))
        else if (facing == West)
          copy(x = plan.cc(y).indexWhere(_ == ' ', x + 1))
        else copy(y = plan.cc.indexWhere(_(x) == ' ', y + 1))

      lazy val advance: Pos =
        if (facing == East) copy(x = x + 1)
        else if (facing == South) copy(y = y + 1)
        else if (facing == West) copy(x = x - 1)
        else copy(y = y - 1)

      lazy val warpOpt: Option[Pos] = Some(advance)
        .filter(_.c != '#')
        .flatMap(p => if (p.c == ' ') plan.warp(p).warpOpt else Some(p))

      lazy val cw: Pos = copy(facing = Dir((facing.id + 1) % Dir.maxId))

      lazy val ccw: Pos =
        copy(facing = Dir((facing.id - 1 + Dir.maxId) % Dir.maxId))

      def advance(adv: Int): Pos = {
        if (adv <= 0) this
        else warpOpt.map(_.advance(adv - 1)).getOrElse(this)
      }

      def move(mv: String): Pos = mv match {
        case adv if adv.head.isDigit => advance(adv.toInt)
        case _ if mv.head == 'R'     => cw
        case _                       => ccw
      }
    }

    object Pos {
      def parse(in: String*): (Pos, Seq[String]) = {
        // Pad each line so the plan is a square and there's whitespace to the left and bottom of any place
        val width = 1 + in.maxBy(_.length).length
        val cc = in.dropRight(1).map(row => row + " " * (width - row.length)) :+ (" " * width)
        (
          Pos(x = cc.head.indexOf('.'), plan = Plan(cc)),
          in.last.split("((?<=\\D)|(?=\\D))").filter(_.nonEmpty)
        )
      }
    }

    def solve(initialPos: Pos, moves: Seq[String]): Long = {
      val posEnd = moves.foldLeft(initialPos) { case (pos, mv) => pos.move(mv) }
      1000 * (posEnd.y + 1) + 4 * (posEnd.x + 1) + posEnd.facing.id
    }
  }

  import Solution.Dir._
  import Solution._

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

      val (initE, _) = Pos.parse(input: _*)
      initE.advance shouldBe initE.copy(x = initE.x + 1)
      val initS = initE.cw
      val initW = initE.cw.cw
      val initN = initE.cw.cw.cw
      initS shouldBe initE.copy(facing = South)
      initW shouldBe initE.copy(facing = West)
      initN shouldBe initE.copy(facing = North)

      initE.cw.cw.cw.cw shouldBe initE
      initE.ccw shouldBe initN
      initE.ccw.ccw shouldBe initW
      initE.ccw.ccw.ccw shouldBe initS
      initE.ccw.ccw.ccw.ccw shouldBe initE

      initS.advance shouldBe initS.copy(y = initE.y + 1)
      initW.advance shouldBe initW.copy(x = initE.x - 1)
      initN.advance shouldBe initN.copy(y = initE.y - 1)

      initE.warpOpt shouldBe Some(initE.copy(x = 9))
      initS.warpOpt shouldBe Some(initS.copy(y = 1))
      initW.warpOpt shouldBe None
      initN.warpOpt shouldBe Some(initN.copy(x = 8, y = 11))
    }

    it("should warp as requested") {
      // The original map
      val (initE, _) = Pos.parse(input: _*)

      // Lines are created around the puzzle and can be used to map points
      val line2a = initE.plan.line(0, 2, 4, West)
      val line2b = initE.plan.line(1, 1, 4, North)

      val fold = line2a.zip(line2b.map(_.cw.cw)).toMap ++
        line2b.zip(line2a.map(_.cw.cw)).toMap

      initE.plan.warp = fold
      val initS = initE.cw
      val initW = initE.cw.cw
      val initN = initE.cw.cw.cw

      initE.warpOpt shouldBe Some(initE.copy(x = 9))
      initS.warpOpt shouldBe Some(initS.copy(y = 1))
      initW.warpOpt shouldBe Some(initS.copy(x = 4, y = 4))
      initN.copy(x = 4, y = 4).warpOpt shouldBe Some(
        initE.copy(x = 8, y = 0)
      )
    }

    it("should match the puzzle description for part 1") {
      val (init, moves) = Pos.parse(input: _*)
      solve(init, moves) shouldBe 6032
    }

    it("should match the puzzle description for part 2") {
      val (init, moves) = Pos.parse(input: _*)

      //             ┌─a1◄─┐
      //             │     │
      //             │     │
      //             ▼b1 c1▲
      //             │     │
      //             │     │
      // ┌─a2►─┬─b2►─┼─────┤
      // │     │     │     │
      // │     │     │     │
      // ▲g1   │     │   d1▲
      // │     │     │     │
      // │     │     │     │
      // └─f2◄─┴─e1◄─┼─────┼─d2►─┐
      //             │     │     │
      //             │     │     │
      //             ▼e2   │   c2▼
      //             │     │     │
      //             │     │     │
      //             └─f1►─┴─g2►─┘

      val a2 = init.plan.line(0, 1, 4, North)
      val b2 = init.plan.line(1, 1, 4, North)
      val a1 = init.plan.line(2, 0, 4, North).reverse
      val d2 = init.plan.line(3, 2, 4, North)

      val b1 = init.plan.line(0, 2, 4, West)
      val g1 = init.plan.line(1, 0, 4, West).reverse
      val e2 = init.plan.line(2, 2, 4, West)

      val f2 = init.plan.line(0, 2, 4, South).reverse
      val e1 = init.plan.line(1, 2, 4, South).reverse
      val f1 = init.plan.line(2, 3, 4, South)
      val g2 = init.plan.line(3, 3, 4, South)

      val c1 = init.plan.line(0, 3, 4, East).reverse
      val d1 = init.plan.line(1, 3, 4, East).reverse
      val c2 = init.plan.line(2, 4, 4, East)

      // Some tests on creating the lines that warp from and to each other
      b1.map(_.x).distinct shouldBe Seq(7)
      b1.map(_.y) shouldBe Seq(0, 1, 2, 3)
      b1.map(_.facing).distinct shouldBe Seq(West)
      b2.map(_.x) shouldBe Seq(4, 5, 6, 7)
      b2.map(_.y).distinct shouldBe Seq(3)
      b2.map(_.facing).distinct shouldBe Seq(North)
      d1.map(_.x).distinct shouldBe Seq(12)
      d1.map(_.y) shouldBe Seq(7, 6, 5, 4)
      d1.map(_.facing).distinct shouldBe Seq(East)
      f1.map(_.x) shouldBe Seq(8, 9, 10, 11)
      f1.map(_.y).distinct shouldBe Seq(12)
      f1.map(_.facing).distinct shouldBe Seq(South)

      init.plan.warp = Seq(a1, a2, b1, b2, c1, c2, d1, d2, e1, e2, f1, f2, g1, g2)
        .grouped(2)
        .flatMap { case Seq(x1, x2) =>
          x1.zip(x2.map(_.cw.cw)) ++ x2.zip(x1.map(_.cw.cw))
        }
        .toMap

      init.copy(8, 11, South).warpOpt shouldBe Some(init.copy(3, 7, North))
      init.copy(9, 11, South).warpOpt shouldBe Some(init.copy(2, 7, North))
      init.copy(10, 10, South).warpOpt shouldBe Some(init.copy(10, 11, South))
      init.copy(10, 11, South).warpOpt shouldBe Some(init.copy(1, 7, North))
      init.copy(1, 7, North).warpOpt shouldBe Some(init.copy(1, 6, North))
      init.copy(1, 6, North).warpOpt shouldBe Some(init.copy(1, 5, North))
      init.copy(10, 10, South).move("4") shouldBe init.copy(1, 5, North)

      // 0123456789012345
      //         >>v#     0
      //         .#v.     1
      //         #.v.     2
      //         ..v.     3
      // ...#..^...v#     4
      // .>>>>>^.#.>>     5
      // .^#....#....     6
      // .^........#.     7
      //         ...#..v. 8
      //         .....#v. 9
      //         .#v<<<<. 10
      //         ..v...#. 11

      val part2Pos = moves.scanLeft(init) { case (pos, mv) => pos.move(mv) }
      part2Pos.head shouldBe init
      part2Pos(1) shouldBe init.copy(10, 0, East) // 10
      part2Pos(2) shouldBe init.copy(10, 0, South) // R
      part2Pos(3) shouldBe init.copy(10, 5, South) // 5
      part2Pos(4) shouldBe init.copy(10, 5, East) // L
      part2Pos(5) shouldBe init.copy(14, 10, South) // 5
      part2Pos(6) shouldBe init.copy(14, 10, West) // R
      part2Pos(7) shouldBe init.copy(10, 10, West) // 10
      part2Pos(8) shouldBe init.copy(10, 10, South)
      part2Pos(9) shouldBe init.copy(1, 5, North) // 4
      part2Pos(10) shouldBe init.copy(1, 5, East) // R
      part2Pos(11) shouldBe init.copy(6, 5, East) // 5
      part2Pos(12) shouldBe init.copy(6, 5, North) // L
      part2Pos(13) shouldBe init.copy(6, 4, North) // 5

      solve(init, moves) shouldBe 5031
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day22Input.txt")

    it("should have answers for part 1") {
      val (init, moves) = Pos.parse(input: _*)
      solve(init, moves) shouldBe decryptLong("4b36ireVWWF6o//Erb/59w==")
    }

    it("should have answers for part 2") {
      val (init, moves) = Pos.parse(input: _*)

      //       ┌─b1►─┬─c1►─┐
      //       │     │     │
      //       │     │     │
      //       ▼d1   │   e1▼
      //       │     │     │
      //       │     │     │
      //       ├─────┼─f2◄─┘
      //       │     │
      //       │     │
      //       ▼a2 f1▲
      //       │     │
      //       │     │
      // ┌─a1►─┼─────┤
      // │     │     │
      // │     │     │
      // ▲d2   │   e2▲
      // │     │     │
      // │     │     │
      // ├─────┼─g2◄─┘
      // │     │
      // │     │
      // ▼b2 g1▲
      // │     │
      // │     │
      // └─c2►─┘

      val a1 = init.plan.line(0, 2, 50, North)
      val b1 = init.plan.line(1, 0, 50, North)
      val c1 = init.plan.line(2, 0, 50, North)

      val d1 = init.plan.line(0, 1, 50, West)
      val a2 = init.plan.line(1, 1, 50, West)
      val d2 = init.plan.line(2, 0, 50, West).reverse
      val b2 = init.plan.line(3, 0, 50, West)

      val c2 = init.plan.line(0, 4, 50, South)
      val g2 = init.plan.line(1, 3, 50, South).reverse
      val f2 = init.plan.line(2, 1, 50, South).reverse

      val e1 = init.plan.line(0, 3, 50, East)
      val f1 = init.plan.line(1, 2, 50, East).reverse
      val e2 = init.plan.line(2, 2, 50, East).reverse
      val g1 = init.plan.line(3, 1, 50, East).reverse

      init.plan.warp = Seq(f2, f1, e1, e2, a2, a1, g2, g1, d1, d2, b1, b2, c1, c2)
        .grouped(2)
        .flatMap { case Seq(lineA, lineB) =>
          lineA.zip(lineB.map(_.cw.cw)) ++ lineB.zip(lineA.map(_.cw.cw))
        }
        .toMap

      solve(init, moves) shouldBe decryptLong("rzTcnCPzZLATZudCfIpMZg==")
    }
  }
}
