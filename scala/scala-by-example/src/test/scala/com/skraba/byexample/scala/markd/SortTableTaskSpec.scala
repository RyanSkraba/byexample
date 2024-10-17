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
    File(Simple / "basic.md").writeAll("""To Sort | A | B
        |---|---|---
        |3 | 2 | 3
        |2 | 3 | 1
        |1 | 1 | 2
        |""".stripMargin)

    it("should sort on the first column by default") {
      withGoMatching(TaskCmd, Simple / "basic.md", "To Sort") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        File(Simple / "basic.md").slurp() shouldBe
          """| To Sort | A | B |
            !|---------|---|---|
            !| 1       | 1 | 2 |
            !| 2       | 3 | 1 |
            !| 3       | 2 | 3 |
            !""".stripMargin('!')
      }
    }

    it("should sort on other columns") {
      withGoMatching(TaskCmd, Simple / "basic.md", "To Sort", "--sortBy", "1") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        File(Simple / "basic.md").slurp() shouldBe
          """| To Sort | A | B |
            !|---------|---|---|
            !| 1       | 1 | 2 |
            !| 3       | 2 | 3 |
            !| 2       | 3 | 1 |
            !""".stripMargin('!')
      }

      withGoMatching(TaskCmd, Simple / "basic.md", "To Sort", "--sortBy", "2") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        File(Simple / "basic.md").slurp() shouldBe
          """| To Sort | A | B |
            !|---------|---|---|
            !| 2       | 3 | 1 |
            !| 1       | 1 | 2 |
            !| 3       | 2 | 3 |
            !""".stripMargin('!')
      }
    }

    it("should ignore sorting on an out of bounds numeric column") {
      withGoMatching(TaskCmd, Simple / "basic.md", "To Sort", "--sortBy", "99") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        File(Simple / "basic.md").slurp() shouldBe
          """| To Sort | A | B |
            !|---------|---|---|
            !| 2       | 3 | 1 |
            !| 1       | 1 | 2 |
            !| 3       | 2 | 3 |
            !""".stripMargin('!')
      }
    }
  }
}
