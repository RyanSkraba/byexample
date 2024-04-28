package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.BuildFailureReportTask.FailedBuild
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

  describe("Parsing build titles") {
    it("should return empty values for an empty string") {
      FailedBuild.parseBuildTitle("") shouldBe ("", "", "")
    }

    it("should assign 'abc' to the build version") {
      FailedBuild.parseBuildTitle("abc") shouldBe ("abc", "", "")
    }

    it("should assign a ' abc' to the build description") {
      FailedBuild.parseBuildTitle(" abc") shouldBe ("", "abc", "")
    }

    it("should assign a ' abc def' to the build description") {
      FailedBuild.parseBuildTitle(" abc def") shouldBe ("", "abc def", "")
    }

    it("should assign a 'abc def ghi' to the build version and description") {
      FailedBuild.parseBuildTitle("abc def ghi") shouldBe ("abc", "def ghi", "")
    }

    it("should assign a 'http://joblink' to the build link") {
      FailedBuild.parseBuildTitle("http://joblink") shouldBe ("", "", "http://joblink")
    }

    it("should assign a 'abc http://joblink' to the build version and link") {
      FailedBuild.parseBuildTitle("abc http://joblink") shouldBe ("abc", "", "http://joblink")
    }

    it("should assign a ' abc http://joblink' to the build description and link") {
      FailedBuild.parseBuildTitle(" abc http://joblink") shouldBe ("", "abc", "http://joblink")
    }

    it("should assign a 'abc def ghi http://joblink' to all attributes") {
      FailedBuild.parseBuildTitle("abc def ghi http://joblink") shouldBe ("abc", "def ghi", "http://joblink")
    }
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
