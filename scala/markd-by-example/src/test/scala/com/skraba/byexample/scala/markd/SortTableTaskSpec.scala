package com.skraba.byexample.scala.markd
import com.tinfoiled.docopt4s.testkit.MultiTaskMainSpec
import com.tinfoiled.markd._

import scala.reflect.io.{Directory, File}

/** Unit tests for [[SortTableTask]] */
class SortTableTaskSpec extends MultiTaskMainSpec(MarkdGo, Some(SortTableTask)) {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)
  // TODO(rskraba): Tmp should be in the DocoptCliGoSpec

  describe(s"${Main.Name} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq.empty)
    itShouldThrowOnMissingOpt(Seq("file"))
  }

  /** Helper to extract a column from a matching table. */
  def extractColumns(in: String, extract: String = "Original", title: String = "To Sort"): Seq[Any] =
    Header
      .parse(in)
      .collectFirstRecursive({
        case tbl: Table if tbl.title == title =>
          val original = tbl.mds.head.cells.indexOf(extract)
          tbl.mds.tail.map(_(original max 0)).map(c => c.toIntOption.getOrElse(c))
      })
      .getOrElse(Seq.empty)

  def extractColumn(in: String, extract: String = "Original", title: String = "To Sort"): String =
    extractColumns(in, extract, title).map(_.toString).mkString("|")

  def generateTable(title: String, rows: Any*): Table = Table.from(
    Seq.fill(1)(Align.LEFT),
    TableRow.from(title) +: rows.map(_.toString).map(TableRow.from(_)): _*
  )

  val Basic: File = File(Tmp / "basic.md")
  Basic.writeAll("""To Sort | A | B | Original
                   !---|----|---|---|
                   !z  | 10 | a | 0 |
                   !y  | 8  | b | 1 |
                   !y  | 6  | 10| 2 |
                   !x  | 7  | c | 3 |
                   !x  | 7  | d | 4 |
                   !w  | 1  | 1 | 5 |
                   !X  | 7  | c | 6 |
                   !""".stripMargin('!'))

  describe("When overwriting a very simple file") {

    it("should sort on the first column by default") {
      val in = File(Tmp / "rewrite.md")
      in.writeAll(Basic.slurp())
      withGoMatching(TaskCmd, in, "To Sort") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """!| To Sort | A  | B  | Original |
             !|---------|----|----|----------|
             !| X       | 7  | c  | 6        |
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
      for (col <- Seq("0", "To Sort")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, Basic, "To Sort", col, "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            extractColumn(stdout, "To Sort") shouldBe "X|w|x|x|y|y|z"
            extractColumn(stdout) shouldBe "6|5|3|4|1|2|0"
          }
        }

        for (specifier <- Seq("i", "/i", "AlphaI", "ai"))
          it(s"should sort by column $col:$specifier ignoring case") {
            withGoMatching(TaskCmd, Basic, "To Sort", s"$col:$specifier", "-") { case (stdout, stderr) =>
              stderr shouldBe empty
              extractColumn(stdout, "To Sort") shouldBe "w|x|x|X|y|y|z"
              extractColumn(stdout) shouldBe "5|3|4|6|1|2|0"
            }
          }
      }

      for (col <- Seq("1", "A")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, Basic, "To Sort", col, "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            extractColumn(stdout, "A") shouldBe "1|10|6|7|7|7|8"
            extractColumn(stdout) shouldBe "5|0|2|3|4|6|1"
          }
        }

        for (specifier <- Seq("desc", "\\", "DescAlpha", "DESCa"))
          it(s"should sort by column $col:$specifier descending") {
            withGoMatching(TaskCmd, Basic, "To Sort", s"$col:$specifier", "-") { case (stdout, stderr) =>
              stderr shouldBe empty
              extractColumn(stdout, "A") shouldBe "8|7|7|7|6|10|1"
              extractColumn(stdout) shouldBe "1|3|4|6|2|0|5"
            }
          }

        for (specifier <- Seq("num", "#", "/NUM", "AscNum"))
          it(s"should sort by column $col:$specifier numerically") {
            withGoMatching(TaskCmd, Basic, "To Sort", s"$col:$specifier", "-") { case (stdout, stderr) =>
              stderr shouldBe empty
              extractColumn(stdout, "A") shouldBe "1|6|7|7|7|8|10"
              extractColumn(stdout) shouldBe "5|2|3|4|6|1|0"
            }
          }
      }

      for (col <- Seq("2", "B")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, Basic, "To Sort", col, "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            extractColumn(stdout, "B") shouldBe "1|10|a|b|c|c|d"
            extractColumn(stdout) shouldBe "5|2|0|1|3|6|4"
          }
        }
      }

      for (col <- Seq("3", "Original")) {
        it(s"should sort by column $col") {
          withGoMatching(TaskCmd, Basic, "To Sort", col, "-") { case (stdout, stderr) =>
            stderr shouldBe empty
            extractColumn(stdout) shouldBe "0|1|2|3|4|5|6"
          }
        }
      }
    }

    describe("when sorting on multiple columns") {

      for (col <- Seq("0,1", "To Sort,A", "99,0,1", "0,99,1", "0,1,99")) {
        it(s"should sort by column $col") {
          withGoMatching(Seq(TaskCmd, Basic, "To Sort") ++ col.split(",") :+ "-": _*) { case (stdout, stderr) =>
            stderr shouldBe empty
            extractColumn(stdout, "To Sort") shouldBe "X|w|x|x|y|y|z"
            extractColumn(stdout, "A") shouldBe "7|1|7|7|6|8|10"
            extractColumn(stdout) shouldBe "6|5|3|4|2|1|0"
          }
        }
      }

      it(s"should sort by column 0,2") {
        withGoMatching(TaskCmd, Basic, "To Sort", "0", "2", "-") { case (stdout, stderr) =>
          stderr shouldBe empty
          extractColumn(stdout, "To Sort") shouldBe "X|w|x|x|y|y|z"
          extractColumn(stdout, "B") shouldBe "c|1|c|d|10|b|a"
          extractColumn(stdout) shouldBe "6|5|3|4|2|1|0"
        }
      }

      it(s"should sort by column 0,3") {
        withGoMatching(TaskCmd, Basic, "To Sort", "0", "3", "-") { case (stdout, stderr) =>
          stderr shouldBe empty
          extractColumn(stdout, "To Sort") shouldBe "X|w|x|x|y|y|z"
          extractColumn(stdout) shouldBe "6|5|3|4|1|2|0"
        }
      }

      it(s"should sort by column 0,1,2,3") {
        withGoMatching(TaskCmd, Basic, "To Sort", "0", "1", "2", "3", "-") { case (stdout, stderr) =>
          stderr shouldBe empty
          extractColumn(stdout, "To Sort") shouldBe "X|w|x|x|y|y|z"
          extractColumn(stdout) shouldBe "6|5|3|4|2|1|0"
        }
      }

      it(s"should sort by column 3,2,1,0") {
        withGoMatching(TaskCmd, Basic, "To Sort", "3", "2", "1", "0", "-") { case (stdout, stderr) =>
          stderr shouldBe empty
          extractColumn(stdout) shouldBe "0|1|2|3|4|5|6"
        }
      }
    }

    describe("when sorting a missing table") {

      it(s"should fail when specifying a missing table") {
        interceptGoIAEx(TaskCmd, Basic, "Missing", "-").getMessage shouldBe s"Table not found: 'Missing'"
      }

      it(s"should ignore when specifying a missing table") {
        withGoMatching(TaskCmd, Basic, "Missing", "--ignore", "-") { case (stdout, stderr) =>
          stderr shouldBe empty
          extractColumn(stdout) shouldBe "0|1|2|3|4|5|6"
        }
      }

    }
  }

  describe("When several tables are in a file") {
    val MultiTable =
      """!| A | B |
         !|---|---|
         !| 2 | 1 |
         !| 1 | 2 |
         !
         !| X | Y |
         !|---|---|
         !| 4 | 3 |
         !| 3 | 4 |
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
    val Multi = File(Tmp / "multi.md")
    Multi.writeAll(MultiTable)

    it(s"should sort all the tables") {
      withGoMatching(TaskCmd, Multi, "A", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """!| A | B |
             !|---|---|
             !| 1 | 2 |
             !| 2 | 1 |
             !
             !| X | Y |
             !|---|---|
             !| 4 | 3 |
             !| 3 | 4 |
             !
             !| A  | B  |
             !|----|----|
             !| 10 | 20 |
             !| 20 | 10 |
             !
             !| A   | B   |
             !|-----|-----|
             !| 100 | 200 |
             !| 200 | 100 |
             !""".stripMargin('!')
      }
    }

    it(s"should sort the first table") {
      withGoMatching(TaskCmd, Multi, "A:0", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """!| A | B |
            !|---|---|
            !| 1 | 2 |
            !| 2 | 1 |
            !
            !| X | Y |
            !|---|---|
            !| 4 | 3 |
            !| 3 | 4 |
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

    it(s"should sort the second table") {
      withGoMatching(TaskCmd, Multi, "A:1", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """!| A | B |
            !|---|---|
            !| 2 | 1 |
            !| 1 | 2 |
            !
            !| X | Y |
            !|---|---|
            !| 4 | 3 |
            !| 3 | 4 |
            !
            !| A  | B  |
            !|----|----|
            !| 10 | 20 |
            !| 20 | 10 |
            !
            !| A   | B   |
            !|-----|-----|
            !| 200 | 100 |
            !| 100 | 200 |
            !""".stripMargin('!')
      }
    }

    it(s"should sort the third table") {
      withGoMatching(TaskCmd, Multi, "A:2", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """!| A | B |
              !|---|---|
              !| 2 | 1 |
              !| 1 | 2 |
              !
              !| X | Y |
              !|---|---|
              !| 4 | 3 |
              !| 3 | 4 |
              !
              !| A  | B  |
              !|----|----|
              !| 20 | 10 |
              !| 10 | 20 |
              !
              !| A   | B   |
              !|-----|-----|
              !| 100 | 200 |
              !| 200 | 100 |
              !""".stripMargin('!')
      }
    }

    for (table <- Seq("A:3", "A:-1", "A:99")) {
      it(s"should fail when specifying missing table number $table") {
        interceptGoIAEx(TaskCmd, Multi, table, "-").getMessage shouldBe s"Bad table specifier: '$table'"
        Multi.slurp() shouldBe MultiTable
      }

      it(s"should ignore when specifying missing table number $table") {
        withGoMatching(TaskCmd, Multi, table, "--ignore", "-") { case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe MultiTable
        }
      }
    }
  }

  describe("Alphabetical sorting") {
    it("should ignore accents") {
      val in = File(Tmp / "accents.md")
      in.writeAll(
        generateTable("To Sort", "Ä1", "À1", "Å0", "Ä0", "A0", "Ä1", "À1", "Ä0", "À1", "Á0", "Ä0").build().toString()
      )

      withGoMatching(TaskCmd, in, "To Sort", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        extractColumn(stdout, "To Sort") shouldBe "Å0|Ä0|A0|Ä0|Á0|Ä0|Ä1|À1|Ä1|À1|À1"
      }
    }

    it("should take case into account") {
      val in = File(Tmp / "accents.md")
      in.writeAll(
        generateTable("To Sort", "ä1", "à1", "Å0", "Ä0", "a0", "Ä1", "à1", "Ä0", "À1", "á0", "Ä0").build().toString()
      )

      withGoMatching(TaskCmd, in, "To Sort", "0", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        extractColumn(stdout, "To Sort") shouldBe "Å0|Ä0|Ä0|Ä0|Ä1|À1|a0|á0|ä1|à1|à1"
      }
    }

    it("should ignore case") {
      val in = File(Tmp / "accents.md")
      in.writeAll(
        generateTable("To Sort", "ä1", "à1", "Å0", "Ä0", "a0", "Ä1", "à1", "Ä0", "À1", "á0", "Ä0").build().toString()
      )

      withGoMatching(TaskCmd, in, "To Sort", "0:i", "-") { case (stdout, stderr) =>
        stderr shouldBe empty
        extractColumn(stdout, "To Sort") shouldBe "Å0|Ä0|a0|Ä0|á0|Ä0|ä1|à1|Ä1|à1|À1"
      }
    }

  }
}
