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
    File(Basic / "failures.md").writeAll("""# Flink Build Failures
        |
        |## YYYY-MM-DD
        |
        |### 1.99 build information http://buildLink.com
        |
        |job information http://joblink.com
        |JIRA-123 this is what happened
        |""".stripMargin)

    it("should work") {
      withGoMatching(TaskCmd, Basic / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Jira
            |==============================================================================
            |
            |JIRA-123 https://issues.apache.org/jira/browse/JIRA-123
            |------------------------------------------------------------------------------
            |
            |* 1.99 job information http://joblink.com
            |""".stripMargin
      }
    }
  }
}
