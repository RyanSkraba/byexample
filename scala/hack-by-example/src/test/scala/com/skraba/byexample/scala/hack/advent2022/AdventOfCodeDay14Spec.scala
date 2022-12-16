package com.skraba.byexample.scala.hack.advent2022

import com.skraba.byexample.scala.hack.advent2022.AdventUtils.puzzleInput
import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

/** =Advent of Code 2022 Day 14 Solutions in scala=
  *
  * Input: A list of rock paths, and falling sand.
  *
  * Part 1: Count the number of units of falling sand before all further sand
  * falls into the abyss
  *
  * Part 2: Count the number of units of falling sand including a floor, until
  * the source of the sand is blocked.
  *
  * @see
  *   Rephrased from [[https://adventofcode.com/2022/day/14]]
  */
class AdventOfCodeDay14Spec
    extends AnyFunSpecLike
    with Matchers
    with BeforeAndAfterEach {

  object Solution {

    /** A cave system behind the waterfall. This automatically adjusts the view
      * so that the empty spaces are trimmed.
      * @param view
      *   Strings describing the cave where '.' is an empty space and all others
      *   are blocked.
      * @param ox
      *   The X offset of the view's top left corner from the actual
      *   coordinates.
      * @param oy
      *   The Y offset of the view's top left corner from the actual
      *   coordinates.
      */
    case class Cave(view: Seq[String], ox: Int, oy: Int) {

      /** The width and height of the view of the cave. */
      lazy val (dx: Int, dy: Int) =
        (view.headOption.map(_.length).getOrElse(0), view.length)

      /** A pretty view of the cave as a string */
      def mkString: String = view.mkString("\n")

      /** @return This cave with a floor */
      def withFloor(): Cave = copy(view = view :+ "#" * dx)

      @tailrec
      private final def dropping(x: Int, y: Int): (Cave, Int, Int) = {
        if (view(y)(x) != '.' && y == 0 && oy > 0)
          copy(view = "." * dx +: view, oy = oy - 1).dropping(x, y)
        else if (x == 0)
          copy(view.map(row => row.head + row), ox = ox - 1)
            .dropping(1, y)
        else if (x == dx - 1)
          copy(view.map(row => row + row.head)).dropping(x, y)
        else if (y == dy - 1) (this, x, Int.MinValue)
        else if (view(y + 1)(x) == '.') dropping(x, y + 1)
        else if (view(y + 1)(x - 1) == '.') dropping(x - 1, y + 1)
        else if (view(y + 1)(x + 1) == '.') dropping(x + 1, y + 1)
        else (copy(view = view.updated(y, view(y).updated(x, 'o'))), x, y)
      }

      def dropSand(dropX: Int = 500): Cave = {
        val (cave, _, y) = dropping(dropX - ox, 0)
        if (y == Int.MinValue) this
        else cave
      }

      def sands(dropX: Int = 500): Iterator[Cave] = {
        Stream
          .continually(0)
          .scanLeft(this) { (acc, _) => acc.dropSand(dropX) }
          .sliding(2)
          .takeWhile { case Seq(cave1, cave2) =>
            cave1 != cave2
          }
          .map(_(1))
      }
    }

    object Cave {
      def apply(in: String*): Cave = {
        val (coords, (offsetX, offsetY, dx, dy)) = parseRockCoordinates(in: _*)
        val plan = Array.fill[Char](dy, dx)('.')
        for (
          cs <- coords; Seq((x1, y1), (x2, y2)) <- cs.sliding(2);
          x <- (x1 min x2) to (x1 max x2); y <- (y1 min y2) to (y1 max y2)
        )
          plan(y)(x) = '#'
        Cave(plan.map(_.mkString), offsetX, offsetY)
      }

      /** @param in
        *   The rock plan describing the lines of rocks in the cavern.
        * @return
        *   A tuple containing the lines of rocks described in the plan, but
        *   also information to trim the view to only the visible parts,
        *   including (in order) an x and y offset to the highest, leftmost
        *   non-empty space and the width and height of the non-empty space. A
        *   margin is included.
        */
      def parseRockCoordinates(
          in: String*
      ): (Seq[Seq[(Int, Int)]], (Int, Int, Int, Int)) = {
        val m = 1

        val coordinates = in
          .map(_.split("(->|,)").map(_.trim.toInt))
          .map(_.grouped(2).toSeq)
          .map(_.map { case Array(x, y) => (x, y) })

        val (minX, maxX, minY, maxY) =
          coordinates.flatten.foldLeft((Int.MaxValue, 0, Int.MaxValue, 0)) {
            case ((minX, maxX, minY, maxY), (x, y)) =>
              (minX min x, maxX max x, minY min y, maxY max y)
          }

        (
          coordinates.map {
            _.map { case (x, y) =>
              (x - minX + m, y - minY + m)
            }
          },
          (minX - m, minY - m, maxX - minX + m * 2 + 1, maxY - minY + m * 2 + 1)
        )
      }
    }

    def part1(in: String*): Long = Cave(in: _*).sands().size

    def part2(in: String*): Long = Cave(in: _*).withFloor().sands().size

  }

  import Solution._

  describe("Example case") {
    val input =
      """498,4 -> 498,6 -> 496,6
        |503,4 -> 502,4 -> 502,9 -> 494,9
        |""".stripMargin.split("\n").filter(_.nonEmpty)

    it("should match the puzzle description") {

      // Parsing the coordinates
      val (coordinates, (offsetX, offsetY, dx, dy)) =
        Cave.parseRockCoordinates(input: _*)
      coordinates shouldBe Seq(
        Seq((5, 1), (5, 3), (3, 3)),
        Seq((10, 1), (9, 1), (9, 6), (1, 6))
      )
      offsetX shouldBe 493
      offsetY shouldBe 3
      dx shouldBe 12
      dy shouldBe 8

      val cave = Cave(input: _*)

      cave.mkString shouldBe
        """............
          |.....#...##.
          |.....#...#..
          |...###...#..
          |.........#..
          |.........#..
          |.#########..
          |............""".stripMargin
      cave.ox shouldBe 493
      cave.oy shouldBe 3
      cave.dx shouldBe 12
      cave.dy shouldBe 8

      val caves = cave.sands().toSeq
      caves should have size 24

      caves.head.mkString shouldBe
        """............
          |.....#...##.
          |.....#...#..
          |...###...#..
          |.........#..
          |.......o.#..
          |.#########..
          |............""".stripMargin
      caves(1).mkString shouldBe
        """............
          |.....#...##.
          |.....#...#..
          |...###...#..
          |.........#..
          |......oo.#..
          |.#########..
          |............""".stripMargin
      caves(2).mkString shouldBe
        """............
          |.....#...##.
          |.....#...#..
          |...###...#..
          |.........#..
          |......ooo#..
          |.#########..
          |............""".stripMargin
      caves(3).mkString shouldBe
        """............
          |.....#...##.
          |.....#...#..
          |...###...#..
          |.......o.#..
          |......ooo#..
          |.#########..
          |............""".stripMargin
      caves(22).mkString shouldBe
        """............
          |.......o....
          |......ooo...
          |.....#ooo##.
          |....o#ooo#..
          |...###ooo#..
          |.....oooo#..
          |....ooooo#..
          |.#########..
          |............""".stripMargin
      caves(23).mkString shouldBe
        """............
          |.......o....
          |......ooo...
          |.....#ooo##.
          |....o#ooo#..
          |...###ooo#..
          |.....oooo#..
          |..o.ooooo#..
          |.#########..
          |............""".stripMargin

      // This is the last sand

      val cavesF = cave.withFloor().sands().toSeq
      cavesF should have size 93

      cavesF(23).mkString shouldBe
        """............
          |.......o....
          |......ooo...
          |.....#ooo##.
          |....o#ooo#..
          |...###ooo#..
          |.....oooo#..
          |..o.ooooo#..
          |.#########..
          |............
          |############""".stripMargin
      cavesF(24).mkString shouldBe
        """.............
          |........o....
          |.......ooo...
          |......#ooo##.
          |.....o#ooo#..
          |....###ooo#..
          |......oooo#..
          |...o.ooooo#..
          |..#########..
          |.o...........
          |#############""".stripMargin
      cavesF(25).mkString shouldBe
        """..............
          |.........o....
          |........ooo...
          |.......#ooo##.
          |......o#ooo#..
          |.....###ooo#..
          |.......oooo#..
          |....o.ooooo#..
          |...#########..
          |.oo...........
          |##############""".stripMargin
      cavesF(26).mkString shouldBe
        """..............
          |.........o....
          |........ooo...
          |.......#ooo##.
          |......o#ooo#..
          |.....###ooo#..
          |.......oooo#..
          |....o.ooooo#..
          |...#########..
          |.ooo..........
          |##############""".stripMargin
      cavesF(53).mkString shouldBe
        """................
          |..........oo....
          |.........oooo...
          |........oooooo..
          |.......oo#ooo##.
          |......ooo#ooo#..
          |.....oo###ooo#..
          |....oooo.oooo#..
          |...oooooooooo#..
          |..ooo#########..
          |.ooooo..........
          |################""".stripMargin
      cavesF(54).mkString shouldBe
        """.................
          |..........oo.....
          |.........oooo....
          |........oooooo...
          |.......oo#ooo##..
          |......ooo#ooo#...
          |.....oo###ooo#...
          |....oooo.oooo#...
          |...oooooooooo#...
          |..ooo#########...
          |.ooooo.........o.
          |#################""".stripMargin
      cavesF(92).mkString shouldBe
        """...........o...........
          |..........ooo..........
          |.........ooooo.........
          |........ooooooo........
          |.......oo#ooo##o.......
          |......ooo#ooo#ooo......
          |.....oo###ooo#oooo.....
          |....oooo.oooo#ooooo....
          |...oooooooooo#oooooo...
          |..ooo#########ooooooo..
          |.ooooo.......ooooooooo.
          |#######################""".stripMargin

      part1(input: _*) shouldBe 24
      part2(input: _*) shouldBe 93
    }
  }

  describe("Solution") {
    val input = puzzleInput("Day14Input.txt").filter(_.nonEmpty)
    it("should have answers") {
      val (coordinates, (offsetX, offsetY, dx, dy)) =
        Cave.parseRockCoordinates(input: _*)
      coordinates should have size 126
      offsetX shouldBe 450
      offsetY shouldBe 13
      dx shouldBe 71
      dy shouldBe 155

      val cave = Cave(input: _*)
      cave.view.mkString("\n") shouldBe
        """.......................................................................
          |.............................................#............#............
          |.............................................##############............
          |.......................................................................
          |.......................................................................
          |..........................................#..#.........................
          |..........................................#..#.........................
          |.......................................####..###.......................
          |.......................................#.......#.......................
          |.......................................#.......#.......................
          |.......................................#.......#.......................
          |.......................................#.......#.......................
          |.......................................#.......#.......................
          |.......................................#########.......................
          |.......................................................................
          |.......................................................................
          |...............................................#...#...................
          |...............................................#...#...................
          |...............................................#...#...................
          |...........................................#####...#######.............
          |...........................................#.............#.............
          |...........................................#.............#.............
          |...........................................#.............#.............
          |...........................................#.............#.............
          |...........................................#.............#.............
          |...........................................###############.............
          |.......................................................................
          |.......................................................................
          |.......................................................................
          |.......................................................................
          |..........................................#............................
          |......................................#...#............................
          |......................................#...#............................
          |......................................#...#............................
          |......................................#.#.#............................
          |......................................#.#.#.#..........................
          |......................................#.#.#.#..........................
          |......................................#.#.#.#..........................
          |......................................#######..........................
          |.......................................................................
          |.....................................#............#....................
          |.....................................##############....................
          |.......................................................................
          |........................................................#..............
          |.........................................################..............
          |.......................................................................
          |.......................................................................
          |.......................................................#..#............
          |.......................................................#..#............
          |.......................................................#..#............
          |.................................................#######..##...........
          |.................................................#.........#...........
          |.................................................#.........#...........
          |.................................................#.........#...........
          |.................................................#.........#...........
          |.................................................#.........#...........
          |.................................................#.........#...........
          |.................................................#.........#...........
          |.................................................###########...........
          |.......................................................................
          |.......................................................................
          |.......................................................................
          |..............................................................#........
          |..............................................................#........
          |..............................................................#........
          |..............................................................#........
          |........................................................#.....#........
          |........................................................#.#...#........
          |........................................................#.#...#........
          |........................................................#.#.#.#........
          |........................................................#.#.#.#........
          |........................................................#######........
          |.......................................................................
          |.......................................................................
          |....................................................######.............
          |.......................................................................
          |.................................................######.######.........
          |.......................................................................
          |..............................................######.######.######.....
          |.......................................................................
          |...........................................######.######.######.######.
          |.......................................................................
          |.......................................................................
          |...................................#.....#.............................
          |...................................#.....#.............................
          |...................................#...#.#.............................
          |...................................#...#.#.............................
          |...................................#...#.#...#.........................
          |...................................#...#.#...#.........................
          |...................................#...#.#...#.#.......................
          |...................................#.#.#.#...#.#.......................
          |...................................#.#.#.#...#.#.......................
          |...................................#.#.#.#.#.#.#.......................
          |...................................#############.......................
          |.......................................................................
          |.......................................................................
          |.................................................#.#...................
          |.................................................#.#...................
          |.................................................#.#...................
          |.................................................#.#...................
          |.................................................#.#...................
          |...........................................#.#...#.#...................
          |...........................................#.#.#.#.#...................
          |...........................................#.#.#.#.#...................
          |...........................................#.#.#.#.#...................
          |...........................................#.#.#.#.#...................
          |...........................................#########...................
          |.......................................................................
          |.......................................................................
          |.......................................................................
          |..................................#....................................
          |..................................##################...................
          |.......................................................................
          |.......................................................................
          |..............................#.....#..................................
          |..............................#.....#..................................
          |..............................#.....#..................................
          |.........................######.....#########..........................
          |.........................#..................#..........................
          |.........................#..................#..........................
          |.........................#..................#..........................
          |.........................#..................#..........................
          |.........................#..................#..........................
          |.........................####################..........................
          |.......................................................................
          |.......................................................................
          |......................#####............................................
          |.......................................................................
          |.......................................................................
          |...................#####.#####.........................................
          |.......................................................................
          |.......................................................................
          |................#####.#####.#####......................................
          |.......................................................................
          |.......................................................................
          |............######.....................................................
          |.......................................................................
          |.......................................................................
          |.........######.######.................................................
          |.......................................................................
          |.......................................................................
          |......######.######.######.............................................
          |.......................................................................
          |.......................................................................
          |...######.######.######.######.........................................
          |.......................................................................
          |.......................................................................
          |.......#####...........................................................
          |.......................................................................
          |.......................................................................
          |....#####.#####........................................................
          |.......................................................................
          |.......................................................................
          |.#####.#####.#####.....................................................
          |.......................................................................""".stripMargin

      val endState = cave.sands().toSeq.last.mkString
      endState shouldBe
        """.......................................................................
          |..................................................o....................
          |.................................................ooo...................
          |................................................ooooo..................
          |...............................................ooooooo.................
          |..............................................ooooooooo................
          |.............................................#oooooooooo..#............
          |.............................................##############............
          |.......................................................................
          |...........................................oo..........................
          |..........................................#oo#.........................
          |..........................................#oo#o........................
          |.......................................####oo###.......................
          |.......................................#..oooo.#.......................
          |.......................................#.oooooo#.......................
          |.......................................#ooooooo#.......................
          |.......................................#ooooooo#.......................
          |.......................................#ooooooo#.......................
          |.......................................#########.......................
          |................................................o......................
          |...............................................ooo.....................
          |..............................................o#ooo#...................
          |.............................................oo#ooo#...................
          |............................................ooo#ooo#...................
          |...........................................#####ooo#######.............
          |...........................................#...ooooo.....#.............
          |...........................................#..ooooooo....#.............
          |...........................................#.ooooooooo...#.............
          |...........................................#ooooooooooo..#.............
          |...........................................#oooooooooooo.#.............
          |...........................................###############.............
          |.......................................................................
          |.......................................................................
          |.......................................................................
          |.........................................oo............................
          |........................................oo#o...........................
          |......................................#ooo#oo..........................
          |......................................#ooo#ooo.........................
          |......................................#ooo#oooo........................
          |......................................#o#o#ooooo.......................
          |......................................#o#o#o#oooo......................
          |......................................#o#o#o#ooooo.....................
          |......................................#o#o#o#oooooo....................
          |......................................#######ooooooo...................
          |............................................ooooooooo..................
          |.....................................#.....ooooooo#ooo.................
          |.....................................##############oooo................
          |..................................................oooooo...............
          |.................................................ooooooo#..............
          |.........................................################..............
          |.......................................................................
          |........................................................oo.............
          |.......................................................#oo#............
          |.......................................................#oo#............
          |.......................................................#oo#............
          |.................................................#######oo##...........
          |.................................................#.....oooo#...........
          |.................................................#....ooooo#...........
          |.................................................#...oooooo#...........
          |.................................................#..ooooooo#...........
          |.................................................#.oooooooo#...........
          |.................................................#ooooooooo#...........
          |.................................................#ooooooooo#...........
          |.................................................###########...........
          |.......................................................................
          |.......................................................................
          |.......................................................................
          |............................................................o.#........
          |...........................................................ooo#........
          |..........................................................oooo#........
          |.........................................................ooooo#........
          |........................................................#ooooo#........
          |........................................................#o#ooo#........
          |.......................................................o#o#ooo#........
          |......................................................oo#o#o#o#........
          |.....................................................ooo#o#o#o#........
          |....................................................oooo#######........
          |...................................................oooooo..............
          |..................................................oooooooo.............
          |.................................................ooo######o............
          |................................................ooooo....ooo...........
          |...............................................oo######.######.........
          |..............................................oooo.....................
          |.............................................o######.######.######.....
          |............................................ooo........................
          |...........................................######.######.######.######.
          |.......................................................................
          |.......................................................................
          |...................................#.....#.............................
          |...................................#.....#o............................
          |...................................#...#.#oo...........................
          |...................................#...#.#ooo..........................
          |...................................#...#.#ooo#.........................
          |...................................#...#.#ooo#o........................
          |...................................#...#.#ooo#o#.......................
          |...................................#.#.#.#ooo#o#.......................
          |...................................#.#.#.#ooo#o#.......................
          |...................................#.#.#.#o#o#o#.......................
          |...................................#############.......................
          |................................................o......................
          |...............................................ooo.....................
          |..............................................ooo#o#...................
          |.............................................oooo#o#...................
          |............................................ooooo#o#...................
          |...........................................oooooo#o#...................
          |..........................................ooooooo#o#...................
          |.........................................oo#o#ooo#o#...................
          |........................................ooo#o#o#o#o#...................
          |.......................................oooo#o#o#o#o#...................
          |......................................ooooo#o#o#o#o#...................
          |.....................................oooooo#o#o#o#o#...................
          |....................................ooooooo#########...................
          |...................................ooooooooo...........................
          |..................................ooooooooooo..........................
          |.................................ooooooooooooo.........................
          |................................oo#oooooooooooo........................
          |...............................ooo##################...................
          |..............................ooooo....................................
          |.............................ooooooo...................................
          |............................oo#ooooo#..................................
          |...........................ooo#ooooo#..................................
          |..........................oooo#ooooo#..................................
          |.........................######ooooo#########..........................
          |.........................#....ooooooo.......#..........................
          |.........................#...ooooooooo......#..........................
          |.........................#..ooooooooooo.....#..........................
          |.........................#.ooooooooooooo....#..........................
          |.........................#ooooooooooooooo...#..........................
          |.........................####################..........................
          |........................o..............................................
          |.......................ooo.............................................
          |......................#####............................................
          |.....................o.................................................
          |....................ooo................................................
          |...................#####.#####.........................................
          |..................o....................................................
          |.................ooo...................................................
          |................#####.#####.#####......................................
          |..............oo.......................................................
          |.............oooo......................................................
          |............######.....................................................
          |..................o....................................................
          |.................ooo...................................................
          |.........######.######.................................................
          |...............o.......................................................
          |..............ooo......................................................
          |......######.######.######.............................................
          |............o..........................................................
          |...........ooo.........................................................
          |...######.######.######.######.........................................
          |.........o.............................................................
          |........ooo............................................................
          |.......#####...........................................................
          |......o................................................................
          |.....ooo...............................................................
          |....#####.#####........................................................
          |...o...................................................................
          |..ooo..................................................................
          |.#####.#####.#####.....................................................
          |.......................................................................""".stripMargin

      endState.count(_ == 'o') shouldBe 672

      part1(input: _*) shouldBe 672
      part2(input: _*) shouldBe 26831
    }
  }
}
