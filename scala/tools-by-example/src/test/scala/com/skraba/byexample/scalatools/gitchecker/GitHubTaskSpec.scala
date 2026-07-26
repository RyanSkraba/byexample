package com.skraba.byexample.scalatools.gitchecker

import com.skraba.byexample.scalatools.filerenamer.WithTmpSrcDst
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}

import java.nio.file.Path

/** Unit tests for [[GitHubTask]]. */
class GitHubTaskSpec extends MultiTaskMainSpec(GitCheckerGo, Some(GitHubTask)) with WithFileTests with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the MonthlyTask with the source and destination directories. */
  def github(src: Path, dst: Path)(args: Any*): String =
    withGoStdoutSrcDst(src, dst)(TaskCmd +: args: _*)

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()

    itShouldThrowOnMissingOptValue("openPr", "org/repo", "--mock")
    itShouldThrowOnMissingOptValue("contrib", "username", "--mock")
    itShouldThrowOnMissingOptValue("failedRun", "org/repo", "--mock")
  }

  describe(s"Running $MainName $TaskCmd openPr") {

    // An example response to avoid going to the server
    val mock = """{"total_count": 100}"""
    val (src, dst) = createSrcDst("openPr")

    it("should get the open PRs for avro") {
      github(src, dst)("openPr", "apache/avro", "--plain", "--mock" -> mock) shouldBe "100"
    }

    // TODO: Test, --verbose, --plain, --mock, --html

  }

  describe(s"Running $MainName $TaskCmd contrib") {

    // An example response to avoid going to the server.
    val mock =
      """{
        |  "data" : {
        |    "user" : {
        |      "contributionsCollection" : {
        |        "contributionCalendar" : {
        |          "totalContributions" : 565,
        |          "weeks" : [
        |            { "contributionDays" : [
        |              {"contributionCount": 1, "date": "2025-07-27"},
        |              {"contributionCount": 0, "date": "2025-07-28"},
        |              {"contributionCount": 1, "date": "2025-07-29"},
        |              {"contributionCount": 0, "date": "2025-07-30"},
        |              {"contributionCount": 1, "date": "2025-07-31"},
        |              {"contributionCount": 0, "date": "2025-08-01"},
        |              {"contributionCount": 1, "date": "2025-08-02"} ] },
        |            { "contributionDays" : [
        |              {"contributionCount": 1, "date": "2025-07-27"},
        |              {"contributionCount": 2, "date": "2025-07-28"},
        |              {"contributionCount": 10, "date": "2025-07-29"},
        |              {"contributionCount": 0, "date": "2025-07-30"},
        |              {"contributionCount": 0, "date": "2025-07-31"},
        |              {"contributionCount": 0, "date": "2025-08-01"},
        |              {"contributionCount": 1, "date": "2025-08-02"} ] }
        |          ]
        |        }
        |      }
        |    }
        |  }
        |}
        |""".stripMargin
    val (src, dst) = createSrcDst("contrib")

    it("should get the contribution counts") {
      github(src, dst)("contrib", "username", "--plain", "--mock" -> mock) shouldBe ""
    }
  }
}
