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
    val BasicTable = """To Sort | A | B
          !---|----|---
          !z  | 10 | a
          !y  | 8  | b
          !y  | 6  | 10
          !x  | 7  | c
          !x  | 7  | d
          !w  | 1  | 1
          !""".stripMargin('!')

    it("should sort on the first column by default") {
      val in = File(Simple / "basic.md")
      in.writeAll(BasicTable)
      withGoMatching(TaskCmd, in, "To Sort") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !| y       | 8  | b  |
            !| y       | 6  | 10 |
            !| z       | 10 | a  |
            !""".stripMargin('!')
      }
    }

    it("should sort on other columns") {
      val in = File(Simple / "basic1.md")
      in.writeAll(BasicTable)
      withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "1") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| z       | 10 | a  |
            !| y       | 6  | 10 |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !| y       | 8  | b  |
            !""".stripMargin('!')
      }

      withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "2") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| y       | 6  | 10 |
            !| z       | 10 | a  |
            !| y       | 8  | b  |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !""".stripMargin('!')
      }
    }

    it("should sort on other columns by name") {
      val in = File(Simple / "basicA.md")
      in.writeAll(BasicTable)
      withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "A") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| z       | 10 | a  |
            !| y       | 6  | 10 |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !| y       | 8  | b  |
            !""".stripMargin('!')
      }

      withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "B") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| y       | 6  | 10 |
            !| z       | 10 | a  |
            !| y       | 8  | b  |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !""".stripMargin('!')
      }

      withGoMatching(TaskCmd, in, "To Sort", "--sortBy", "To Sort") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        in.slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !| y       | 6  | 10 |
            !| y       | 8  | b  |
            !| z       | 10 | a  |
            !""".stripMargin('!')
      }
    }

    it("should ignore sorting on an out of bounds numeric column") {
      val in = File(Simple / "basic99.md")
      in.writeAll(BasicTable)
      File(Simple / "basic99.md").writeAll(BasicTable)
      withGoMatching(TaskCmd, Simple / "basic99.md", "To Sort", "--sortBy", "99") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe empty
        File(Simple / "basic.md").slurp() shouldBe
          """| To Sort | A  | B  |
            !|---------|----|----|
            !| w       | 1  | 1  |
            !| x       | 7  | c  |
            !| x       | 7  | d  |
            !| y       | 8  | b  |
            !| y       | 6  | 10 |
            !| z       | 10 | a  |
            !""".stripMargin('!')
      }
    }
  }
}
