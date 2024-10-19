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

    def extractColumn(in: String, extract: String = "Original", title: String = "To Sort"): Seq[Any] =
      Header
        .parse(in)
        .collectFirstRecursive({
          case tbl: Table if tbl.title == title =>
            val original = tbl.mds.head.cells.indexOf(extract)
            tbl.mds.tail.map(_(original max 0)).map(c => c.toIntOption.getOrElse(c))
        })
        .getOrElse(Seq.empty)

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

    it("should ignore sorting on an out of bounds numeric column") {
      val in = File(Simple / "basic99.md")
      in.writeAll(BasicTable)
      File(Simple / "basic99.md").writeAll(BasicTable)
      withGoMatching(TaskCmd, Simple / "basic99.md", "To Sort", "--sortBy", "99") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        val sorted = in.slurp()
        extractColumn(sorted) shouldBe Seq(0, 1, 2, 3, 4, 5)
      }
    }
  }
}
