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

    it("should find build attributes") {
      FailedBuild.parseBuildTitle("abc") shouldBe ("abc", "", "")
      FailedBuild.parseBuildTitle("abc ") shouldBe ("abc", "", "")
      FailedBuild.parseBuildTitle(" abc ") shouldBe ("", "abc", "")
      FailedBuild.parseBuildTitle(" abc def ") shouldBe ("", "abc def", "")
      FailedBuild.parseBuildTitle("abc def ghi") shouldBe ("abc", "def ghi", "")
      FailedBuild.parseBuildTitle("http://joblink") shouldBe ("", "", "http://joblink")
      FailedBuild.parseBuildTitle("abc http://joblink") shouldBe ("abc", "", "http://joblink")
      FailedBuild.parseBuildTitle(" abc http://joblink") shouldBe ("", "abc", "http://joblink")
      FailedBuild.parseBuildTitle("abc def ghi http://joblink") shouldBe ("abc", "def ghi", "http://joblink")
      FailedBuild.parseBuildTitle("abc def ghi://joblink") shouldBe ("abc", "def ghi://joblink", "")
      FailedBuild.parseBuildTitle("abc def ghi xhttp://joblink") shouldBe ("abc", "def ghi xhttp://joblink", "")
    }
  }

  describe("Parsing job content") {
    it("should return empty values for an empty string") {
      FailedBuild.parseJobAndJiraInfo("") shouldBe ("", "", "", "")
      FailedBuild.parseJobAndJiraInfo("", "") shouldBe ("", "", "", "")
      FailedBuild.parseJobAndJiraInfo("", "", "ignored") shouldBe ("", "", "", "")
    }

    it("should find build steps and URLS in the first line") {
      // No URL link
      FailedBuild.parseJobAndJiraInfo("info") shouldBe ("info", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info", "") shouldBe ("info", "", "", "")
      FailedBuild.parseJobAndJiraInfo(" info ") shouldBe ("info", "", "", "")
      FailedBuild.parseJobAndJiraInfo(" info ", "") shouldBe ("info", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info abc") shouldBe ("info abc", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info abc", "") shouldBe ("info abc", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info htp://link") shouldBe ("info htp://link", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info htp://link", "") shouldBe ("info htp://link", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info xhttps://link") shouldBe ("info xhttps://link", "", "", "")
      FailedBuild.parseJobAndJiraInfo("info xhttps://link", "") shouldBe ("info xhttps://link", "", "", "")

      // With URL links
      FailedBuild.parseJobAndJiraInfo("http://link") shouldBe ("", "http://link", "", "")
      FailedBuild.parseJobAndJiraInfo("http://link", "") shouldBe ("", "http://link", "", "")
      FailedBuild.parseJobAndJiraInfo("info http://link") shouldBe ("info", "http://link", "", "")
      FailedBuild.parseJobAndJiraInfo("info http://link", "") shouldBe ("info", "http://link", "", "")
      FailedBuild.parseJobAndJiraInfo("info abc http://link") shouldBe ("info abc", "http://link", "", "")
      FailedBuild.parseJobAndJiraInfo("info abc http://link", "") shouldBe ("info abc", "http://link", "", "")
      FailedBuild.parseJobAndJiraInfo("info https://link") shouldBe ("info", "https://link", "", "")
      FailedBuild.parseJobAndJiraInfo("info https://link", "") shouldBe ("info", "https://link", "", "")
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
        |
        |job2 information http://job2link.com
        |JIRA-123 that is what happened
        |
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
            |* 1.99 job2 information http://job2link.com
            |""".stripMargin
      }
    }
  }
}
