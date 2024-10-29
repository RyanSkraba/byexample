package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec

import scala.reflect.io.{Directory, File}

/** Unit tests for [[SortTableTask]] */
class SortTableTaskSpec extends DocoptCliGoSpec(MarkdGo, Some(SortTableTask)) {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)
  // TODO(rskraba): Tmp should be in the DocoptCliGoSpec

  describe(s"${Cli.Cli} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOpt(Seq("file"))

    itShouldThrowOnMissingOptValue(Seq("--sortBy"))
  }

  /** Helper to extract a column from a matching table. */
  def extractColumn(in: String, extract: String = "Original", title: String = "To Sort"): Seq[Any] =
    Header
      .parse(in)
      .collectFirstRecursive({
        case tbl: Table if tbl.title == title =>
          val original = tbl.mds.head.cells.indexOf(extract)
          tbl.mds.tail.map(_(original max 0)).map(c => c.toIntOption.getOrElse(c))
      })
      .getOrElse(Seq.empty)

  describe("When parsing a very simple file") {
    val Simple = (Tmp / "simple").createDirectory()
    val BasicTable = """To Sort | A | B | Original
                       !---|----|---|---|
                       !z  | 10 | a | 0 |
                       !y  | 8  | b | 1 |
                       !y  | 6  | 10| 2 |
                       !x  | 7  | c | 3 |
                       !x  | 7  | d | 4 |
                       !w  | 1  | 1 | 5 |
                       !""".stripMargin('!')

    it("should sort on the first column by default") {
      val in = File(Simple / "basic.md")
      in.writeAll(BasicTable)
      withGoMatching(TaskCmd, in, "To Sort") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  | Original |
             !|---------|----|----|----------|
             !| w       | 1  | 1  | 5        |
             !| x       | 7  | c  | 3        |
             !| x       | 7  | d  | 4        |
             !| y       | 8  | b  | 1        |
             !| y       | 6  | 10 | 2        |
             !| z       | 10 | a  | 0        |
             !""".stripMargin('!')
      }
    }

    describe("when sorting on a single column") {
      val in = File(Simple / "basic1col.md")
      in.writeAll(BasicTable)

      for (col <- Seq("1", "A")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, in, "To Sort", "--sortBy", col) { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "A") shouldBe Seq(1, 10, 6, 7, 7, 8)
            extractColumn(sorted) shouldBe Seq(5, 0, 2, 3, 4, 1)
          }
        }

        it(s"should sort by column $col descending") {
          withGoMatching(TaskCmd, in, "To Sort", "--sortBy", col + ":desc") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "A") shouldBe Seq(8, 7, 7, 6, 10, 1)
            extractColumn(sorted) shouldBe Seq(1, 3, 4, 2, 0, 5)
          }
        }

        it(s"should sort by column $col numerically") {
          withGoMatching(TaskCmd, in, "To Sort", "--sortBy", col + ":num") { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "A") shouldBe Seq(1, 6, 7, 7, 8, 10)
            extractColumn(sorted) shouldBe Seq(5, 2, 3, 4, 1, 0)
          }
        }
      }

      for (col <- Seq("2", "B")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, in, "To Sort", "--sortBy", col) { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "B") shouldBe Seq(1, 10, "a", "b", "c", "d")
            extractColumn(sorted) shouldBe Seq(5, 2, 0, 1, 3, 4)
          }
        }
      }

      for (col <- Seq("0", "To Sort")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, in, "To Sort", "--sortBy", col) { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "To Sort") shouldBe Seq("w", "x", "x", "y", "y", "z")
            extractColumn(sorted) shouldBe Seq(5, 3, 4, 2, 1, 0)
          }
        }
      }

      for (col <- Seq("3", "Original")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, in, "To Sort", "--sortBy", col) { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted) shouldBe Seq(0, 1, 2, 3, 4, 5)
          }
        }
      }
    }

    describe("when sorting on multiple columns") {
      val in = File(Simple / "basic2col.md")
      in.writeAll(BasicTable)

      for (col <- Seq("0,1", "To Sort,A", "99,0,1", "0,99,1", "0,1,99")) {
        val sortBys = col.split(",").flatMap(Seq("--sortBy", _)).toSeq
        it(s"should sort by column $col") {
          withGoMatching(Seq(TaskCmd, in, "To Sort") ++ sortBys: _*) { case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "To Sort") shouldBe Seq("w", "x", "x", "y", "y", "z")
            extractColumn(sorted, "A") shouldBe Seq(1, 7, 7, 6, 8, 10)
            extractColumn(sorted) shouldBe Seq(5, 3, 4, 2, 1, 0)
          }
        }
      }

      it(s"should sort by column 0,2") {
        withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "0", "--sortBy", "2") { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe empty
          val sorted = in.slurp()
          extractColumn(sorted, "To Sort") shouldBe Seq("w", "x", "x", "y", "y", "z")
          extractColumn(sorted, "B") shouldBe Seq(1, "c", "d", 10, "b", "a")
          extractColumn(sorted) shouldBe Seq(5, 3, 4, 2, 1, 0)
        }
      }

      it(s"should sort by column 0,3") {
        withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "0", "--sortBy", "3") { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe empty
          val sorted = in.slurp()
          extractColumn(sorted, "To Sort") shouldBe Seq("w", "x", "x", "y", "y", "z")
          extractColumn(sorted) shouldBe Seq(5, 3, 4, 1, 2, 0)
        }
      }

      it(s"should sort by column 0,1,2,3") {
        withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "0", "--sortBy", "1", "--sortBy", "2", "--sortBy", "3") {
          case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted, "To Sort") shouldBe Seq("w", "x", "x", "y", "y", "z")
            extractColumn(sorted) shouldBe Seq(5, 3, 4, 2, 1, 0)
        }
      }

      it(s"should sort by column 3,2,1,0") {
        withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "3", "--sortBy", "2", "--sortBy", "1", "--sortBy", "0") {
          case (stdout, stderr) =>
            stderr shouldBe empty
            stdout shouldBe empty
            val sorted = in.slurp()
            extractColumn(sorted) shouldBe Seq(0, 1, 2, 3, 4, 5)
        }
      }
    }

    describe("when sorting a missing table") {
      val in = File(Simple / "basic_check_missing.md")
      in.writeAll(BasicTable)

      it(s"should ignore when specifying a missing table") {
        withGoMatching(TaskCmd, in, "Missing") { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe empty
          val sorted = in.slurp()
          extractColumn(sorted) shouldBe Seq(0, 1, 2, 3, 4, 5)
        }
      }

      it(s"should fail when specifying a missing table and --failOnMissing") {
        interceptGoIAEx(TaskCmd, in, "Missing", "--failOnMissing").getMessage shouldBe s"Table not found: 'Missing'"
      }
    }
  }

  describe("When several tables are in a file") {
    val in = File(Tmp / "multifile.md")
    in.writeAll("""
        |A  | B
        |---|---
        |2  | 1
        |1  | 2
        |
        |X  |Y
        |---|---
        |2  | 1
        |1  | 2
        |
        |A  | B
        |---|---
        |20 | 10
        |10 | 20
        |
        |A   | B
        |----|---
        |200 | 100
        |100 | 200
        |""".stripMargin)

    it(s"should ignore when specifying a missing table") {
      withGoMatching(TaskCmd, in, "A") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| A | B |
             !|---|---|
             !| 1 | 2 |
             !| 2 | 1 |
             !
             !| X | Y |
             !|---|---|
             !| 2 | 1 |
             !| 1 | 2 |
             !
             !| A  | B  |
             !|----|----|
             !| 20 | 10 |
             !| 10 | 20 |
             !
             !| A   | B   |
             !|-----|-----|
             !| 200 | 100 |
             !| 100 | 200 |
             !""".stripMargin('!')
      }
    }
  }

}
