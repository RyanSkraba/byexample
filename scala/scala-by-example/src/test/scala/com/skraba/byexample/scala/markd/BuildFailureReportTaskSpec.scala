package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGoSpec

import scala.reflect.io.{Directory, File}

/** Unit tests for [[BuildFailureReportTask]] */
class BuildFailureReportTaskSpec extends DocoptCliGoSpec(MarkdGo, Some(BuildFailureReportTask)) {

  /** A temporary directory for playing with files. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  describe(s"${Cli.Cli} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnMissingOpt(Seq.empty)
  }

  describe("On parsing a file") {
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "failures.md").writeAll("# Flink Build Failures")

    it("should work") {
      withGoMatching(TaskCmd, Basic / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Jira
            |==============================================================================
            |
            |FLINK-12345 https://issues.apache.org/jira/browse/FLINK-12345
            |------------------------------------------------------------------------------
            |
            |* buildVersion jobAndStep jogLogLink
            |""".stripMargin
      }
    }
  }
}
