package com.skraba.byexample.scala.hack.advent2024

import com.skraba.byexample.scala.hack.advent2024.AdventUtils._
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** =Advent of Code 2024 Day 15 Solutions in scala=
  *
  * Input: A map showing the positions of a robot and some boxes, as well as unmovable walls, followed by a list of
  * directions that the robot will wander around the warehouse, hitting walls and pushing boxes (when they can be
  * moved).
  *
  * Part 1: Find the GPS score of the boxes after all the directions are followed. The GPS score is the y-coordinate * 2
  * plus the x coordinate.
  *
  * Part 2: The map is actually double-wide, and the boxes are also twice as wide. Moving north can now push two boxes
  * if they're in the way. Find the GPS score of the boxes after all the directions are followed.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2024/day/15]]
  */
class AdventOfCodeDay15Spec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  object Solution {

    object Dir extends Enumeration {
      type Dir = Value
      val East, South, West, North = Value
      def apply(c: Char): Dir = c match { case '>' => East; case 'v' => South; case '<' => West; case '^' => North }
      def mv(dir: Dir, w: Int): Int = dir match { case East => 1; case South => w; case West => -1; case North => -w }
    }
    import Dir._

    /** Inspired from [[com.skraba.byexample.scala.hack.advent2024.AdventOfCodeDay6Spec.Solution.Plan]] */
    case class Plan(full: String, dx: Int, robot: Int = -1, boxes: Set[Int] = Set.empty, doubledBox: Boolean = false) {

      /** The height of the full plan */
      lazy val dy: Int = full.length / dx

      lazy val display: String = boxes
        .foldLeft(full) {
          case (p, b) if doubledBox => p.updated(b, '[').updated(b + 1, ']')
          case (p, b)               => p.updated(b, 'O')
        }
        .updated(if (robot > 0) robot else 0, '@')
        .grouped(dx)
        .mkString("\n")

      lazy val gpsScore: Long = boxes.map(p => p / dx * 100 + p % dx).sum

      /** Finds all instances of the character in the plan. */
      def find(c: Char): Iterator[Int] = new Iterator[Int] {
        var pos: Int = full.indexOf(c)
        def hasNext: Boolean = pos >= 0
        def next(): Int = { val res = pos; pos = full.indexOf(c, pos + 1); res }
      }

      /** Return the impacted positions when the robot at pos pushes in the given direction. */
      def doubleImpact(dir: Dir, pos: Int = robot, checked: Set[Int] = Set.empty): Set[Int] =
        dir match {
          case _ if checked(-1)               => Set(-1)
          case East if full(pos + 1) == '#'   => Set(-1) // If the current position is blocked by a wall
          case South if full(pos + dx) == '#' => Set(-1)
          case West if full(pos - 1) == '#'   => Set(-1)
          case North if full(pos - dx) == '#' => Set(-1)
          case East if !boxes(pos + 1)        => checked // Nothing is blocking the east/west position
          case West if !boxes(pos - 2)        => checked
          case East                           => doubleImpact(dir, pos + 2, checked + (pos + 1))
          case West                           => doubleImpact(dir, pos - 2, checked + (pos - 2))
          case North | South => // Going north or south is more complicated
            val mv = if (dir == South) dx else -dx

            val touching = Seq(pos + mv, pos + mv - 1).filter(boxes).filterNot(checked)
            if (touching.isEmpty) return checked

            val touched = touching.head

            if (full(touched + mv) == '#' || full(touched + mv + 1) == '#') return Set(-1)

            val one = doubleImpact(dir, touched, checked + touched)
            doubleImpact(dir, touched + 1, one) + pos
        }
    }

    def parse(doubled: Boolean, in: String*): (Plan, Seq[Dir]) = {
      ((in.span(_.nonEmpty) match {
        case (p, r) => (p, r.mkString.trim.map(Dir.apply))
      }) match {
        case (p, r) if doubled =>
          Plan(p.mkString.flatMap { case '@' => "@."; case 'O' => "O."; case c => s"$c$c" }, p.head.length * 2) -> r
        case (p, r) =>
          Plan(p.mkString, p.head.length) -> r
      }) match {
        case (p, r) =>
          p.copy(
            full = p.full.replaceAll("[^.#]", "."),
            robot = p.find('@').next(),
            boxes = p.find('O').toSet,
            doubledBox = doubled
          ) -> r
      }
    }

    def part1(in: String*): Long = {
      val (plan0, route) = parse(false, in: _*)

      val ll = LazyList.iterate(plan0 -> 0) { case (plan, step) =>
        val mv = Dir.mv(route(step % route.length), plan.dx)
        LazyList
          .from(plan.robot + mv, mv)
          .filterNot(plan.boxes)
          .head match {
          case p if plan.full(p) == '#' => plan -> (step + 1)
          case p if plan.boxes(plan.robot + mv) =>
            plan.copy(boxes = plan.boxes - (plan.robot + mv) + p, robot = plan.robot + mv) -> (step + 1)
          case p => plan.copy(robot = p) -> (step + 1)
        }
      }

      ll.dropWhile(_._2 != route.length).head._1.gpsScore
    }

    def part2(in: String*): Long = {
      val (plan0, route) = parse(true, in: _*)

      val ll = LazyList.iterate(plan0 -> 0) { case (plan, step) =>
        val dir = route(step % route.length)
        // The list of boxes to move, or contains -1 if one or more is blocked.
        val toMove = plan.doubleImpact(dir)
        if (toMove(-1)) plan -> (step + 1) // Can't move one of the boxes
        else {
          // Otherwise, move the robot and all the impacted boxes in the given direction
          val mv = Dir.mv(dir, plan.dx)
          plan.copy(
            boxes = plan.boxes -- toMove ++ toMove.filter(plan.boxes).map(_ + mv),
            robot = plan.robot + mv
          ) -> (step + 1)
        }
      }

      ll.dropWhile(_._2 != route.length).head._1.gpsScore
    }

  }

  import Solution._

  describe("Example case") {

    val inputMini =
      """########
        |#..O.O.#
        |##@.O..#
        |#...O..#
        |#.#.O..#
        |#...O..#
        |#......#
        |########
        |
        |<^^>>>vv<v>>v<<
        |""".trimSplit

    val input =
      """##########
        |#..O..O.O#
        |#......O.#
        |#.OO..O.O#
        |#..O@..O.#
        |#O#..O...#
        |#O..O..O.#
        |#.OO.O.OO#
        |#....O...#
        |##########
        |
        |<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
        |vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
        |><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
        |<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
        |^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
        |^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
        |>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
        |<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
        |^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
        |v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
        |""".trimSplit

    val inputMini2 =
      """#######
        |#...#.#
        |#.....#
        |#..OO@#
        |#..O..#
        |#.....#
        |#######
        |
        |<vv<<^^<<^^
        |""".trimSplit

    it("should match the puzzle description for part 1 mini") {
      part1(inputMini: _*) shouldBe 2028
    }

    it("should match the puzzle description for part 1") {
      part1(input: _*) shouldBe 10092
    }

    it("should match the puzzle description for part 2 mini") {
      part2(inputMini2: _*) shouldBe 618
    }

    it("should match the puzzle description for part 2") {
      part2(input: _*) shouldBe 9021
    }
  }

  describe("🔑 Solution 🔑") {
    lazy val input = puzzleInput("Day15Input.txt")
    lazy val answer1 = decryptLong("lVSEdkNTsuKnEVU1tWriPA==")
    lazy val answer2 = decryptLong("Em8z691ErPOLOopvY5fj4Q==")

    it("should have answers for part 1") {
      part1(input: _*) shouldBe answer1
    }

    it("should have answers for part 2") {
      part2(input: _*) shouldBe answer2
    }
  }
}
