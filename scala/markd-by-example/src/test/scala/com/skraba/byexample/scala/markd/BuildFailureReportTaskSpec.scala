package com.skraba.byexample.scala.markd
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.reflect.io.File

/** Unit tests for [[BuildFailureReportTask]] */
class BuildFailureReportTaskSpec extends MultiTaskMainSpec(MarkdGo, Some(BuildFailureReportTask)) with TmpDir {

  describe(s"${Main.Name} $TaskCmd command line") {
    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)
    itShouldThrowOnIncompleteArgs(Seq("--days", "1"))
    itShouldThrowOnIncompleteArgs(Seq("--all"))

    itShouldThrowOnMissingFlagValue(Seq("--days"))
    itShouldThrowOnMissingFlagValue(Seq("--until"))
    itShouldThrowOnMissingFlagValue(Seq("--after"))
    itShouldThrowOnMissingFlagValue(Seq("--add-fails"))
    itShouldThrowOnMissingFlagValue(Seq("--main-version"))
  }

  describe("Parsing build titles") {

    def parseBuildTitle(in: String): (String, String, String, String) = {
      val failure = FailedStep("").addBuildInfo(in)
      (failure.buildVersion, failure.buildDesc, failure.buildDate, failure.buildLink)
    }

    it("should return empty values for an empty string") {
      parseBuildTitle("") shouldBe ("", "", "", "")
    }

    it("should find build attributes without dates") {
      parseBuildTitle("abc") shouldBe ("abc", "", "", "")
      parseBuildTitle("abc ") shouldBe ("abc", "", "", "")
      parseBuildTitle(" abc ") shouldBe ("", "abc", "", "")
      parseBuildTitle(" abc def ") shouldBe ("", "abc def", "", "")
      parseBuildTitle("abc def ghi") shouldBe ("abc", "def ghi", "", "")
      parseBuildTitle("https://link") shouldBe ("", "", "", "https://link")
      parseBuildTitle("abc https://link") shouldBe ("abc", "", "", "https://link")
      parseBuildTitle(" abc https://link") shouldBe ("", "abc", "", "https://link")
      parseBuildTitle("abc def ghi https://link") shouldBe ("abc", "def ghi", "", "https://link")
      parseBuildTitle("abc def ghi://link") shouldBe ("abc", "def ghi://link", "", "")
      parseBuildTitle("abc def ghi xhttp://link") shouldBe ("abc", "def ghi xhttp://link", "", "")
    }

    it("should find build attributes with dates") {
      parseBuildTitle("abc (xyz)") shouldBe ("abc", "", "xyz", "")
      parseBuildTitle("abc (xyz) ") shouldBe ("abc", "", "xyz", "")
      parseBuildTitle(" abc (xyz)") shouldBe ("", "abc", "xyz", "")
      parseBuildTitle(" abc def (xyz)") shouldBe ("", "abc def", "xyz", "")
      parseBuildTitle("abc def ghi (xyz)") shouldBe ("abc", "def ghi", "xyz", "")
      parseBuildTitle("(xyz) https://link") shouldBe ("", "", "xyz", "https://link")
      parseBuildTitle("abc (xyz) https://link") shouldBe ("abc", "", "xyz", "https://link")
      parseBuildTitle(" abc (xyz) https://link") shouldBe ("", "abc", "xyz", "https://link")
      parseBuildTitle("abc def ghi (xyz) https://link") shouldBe ("abc", "def ghi", "xyz", "https://link")
      parseBuildTitle("abc def (xyz) ghi://link") shouldBe ("abc", "def (xyz) ghi://link", "", "")
      parseBuildTitle("abc def ghi (xyz) xhttp://link") shouldBe ("abc", "def ghi (xyz) xhttp://link", "", "")
    }
  }

  describe("Parsing step content") {
    def parseStepAndIssueContent(in: String): (String, String, String, String, String) = {
      val failure = FailedStep("").addStepAndIssueInfo(in, "@%s")
      (failure.stepDesc, failure.stepLink, failure.issueTag, failure.issueDesc, failure.issueLink)
    }

    it("should return empty values for an empty string") {
      parseStepAndIssueContent("") shouldBe ("", "", "", "", "")
      parseStepAndIssueContent("\n") shouldBe ("", "", "", "", "")
      parseStepAndIssueContent("\n\nignored") shouldBe ("", "", "", "", "")
    }

    it("should find build steps and URLS in the first line") {
      // No URL link
      parseStepAndIssueContent("info") shouldBe ("info", "", "", "", "")
      parseStepAndIssueContent("info\n") shouldBe ("info", "", "", "", "")
      parseStepAndIssueContent(" info ") shouldBe ("info", "", "", "", "")
      parseStepAndIssueContent(" info \n") shouldBe ("info", "", "", "", "")
      parseStepAndIssueContent("info abc") shouldBe ("info abc", "", "", "", "")
      parseStepAndIssueContent("info abc\n") shouldBe ("info abc", "", "", "", "")
      parseStepAndIssueContent("info htp://link") shouldBe ("info htp://link", "", "", "", "")
      parseStepAndIssueContent("info htp://link\n") shouldBe ("info htp://link", "", "", "", "")
      parseStepAndIssueContent("info xhttps://link") shouldBe ("info xhttps://link", "", "", "", "")
      parseStepAndIssueContent("info xhttps://link\n") shouldBe ("info xhttps://link", "", "", "", "")

      // With URL links
      parseStepAndIssueContent("http://link") shouldBe ("", "http://link", "", "", "")
      parseStepAndIssueContent("http://link\n") shouldBe ("", "http://link", "", "", "")
      parseStepAndIssueContent("info http://link") shouldBe ("info", "http://link", "", "", "")
      parseStepAndIssueContent("info http://link\n") shouldBe ("info", "http://link", "", "", "")
      parseStepAndIssueContent("info abc http://link") shouldBe ("info abc", "http://link", "", "", "")
      parseStepAndIssueContent("info abc http://link\n") shouldBe ("info abc", "http://link", "", "", "")
      parseStepAndIssueContent("info https://link") shouldBe ("info", "https://link", "", "", "")
      parseStepAndIssueContent("info https://link\n") shouldBe ("info", "https://link", "", "", "")
    }

    it("should find issue or defect information in the second line") {
      parseStepAndIssueContent("\ninfo") shouldBe ("", "", "info", "", "@info")
      parseStepAndIssueContent("\ninfo\nignored") shouldBe ("", "", "info", "", "@info")
      parseStepAndIssueContent("\n info ") shouldBe ("", "", "", "info", "")
      parseStepAndIssueContent("\ninfo abc") shouldBe ("", "", "info", "abc", "@info")
      parseStepAndIssueContent("\ninfo abc def") shouldBe ("", "", "info", "abc def", "@info")
      parseStepAndIssueContent("\ninfo abc def\nignored") shouldBe ("", "", "info", "abc def", "@info")
      parseStepAndIssueContent("\n info abc def\nignored") shouldBe ("", "", "", "info abc def", "")
    }
  }

  describe("When parsing a very simple  file") {
    val Simple = (Tmp / "simple").createDirectory()
    File(Simple / "failures.md").writeAll("""# Simple Build Failures
        |## 2024-01-01
        |### 1.0 Build failed https://buildlink
        |Step name https://buildlink/log#100
        |ISSUE-123 Everything is broken
        |""".stripMargin)

    it("should report on the bugs found") {
      withGoMatching(TaskCmd, Simple / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |ISSUE-123 https://issues.apache.org/jira/browse/ISSUE-123
            |------------------------------------------------------------------------------
            |
            |* 1.0 Step name https://buildlink/log#100
            |""".stripMargin
      }
    }

    it("should report on the issues found") {
      withGoMatching(TaskCmd, "--markdown-msg", Simple / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Failure
            |==============================================================================
            |
            |:red_circle:  Build *[1.0 Build failed](https://buildlink)* failed
            |------------------------------------------------------------------------------
            |
            |* [Step name](https://buildlink/log#100) *[ISSUE-123](https://issues.apache.org/jira/browse/ISSUE-123)* Everything is broken
            |""".stripMargin
      }
    }

    it("should make an html report") {
      withGoMatching(TaskCmd, "--html", Simple / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """<!DOCTYPE html>
            |<html><head><style>
            |  button {white-space: pre-wrap;text-align:left;}
            |  a:active, a:hover, a:link, a:visited {text-decoration: none;}
            |</style></head>
            |<body>
            |<h1>Build failure notifications</h1>
            |<p><button clipboard=":red_circle:  Build *[1.0 Build failed](https://buildlink)* failed">Build failed</button> <a href="https://buildlink">🔗</a> <button clipboard="* [Step name](https://buildlink/log#100) *[ISSUE-123](https://issues.apache.org/jira/browse/ISSUE-123)* Everything is broken">ISSUE-123</button> <a href="https://buildlink/log#100">🔗</a><a href="https://issues.apache.org/jira/browse/ISSUE-123">🐞</a></p>
            |<h1>By issue</h1>
            |<p><button clipboard="* 1.0 Step name https://buildlink/log#100" dest="https://issues.apache.org/jira/browse/ISSUE-123">ISSUE-123</button> <a href="https://issues.apache.org/jira/browse/ISSUE-123">🐞</a> <button>* 1.0 Step name https://buildlink/log#100</button></p>
            |<script>
            |// Add event listener to handle clicks on all buttons
            |document.querySelectorAll('button').forEach(button => {
            |  button.addEventListener('click', function() {
            |    navigator.clipboard.writeText(button.getAttribute('clipboard') || button.textContent);
            |    if (button.getAttribute('dest')) window.open(button.getAttribute('dest'), '_blank');
            |  });
            |});
            |</script>
            |</body></html>
            |""".stripMargin
      }
    }
  }

  describe("When parsing a synthetic, simple file") {
    // This creates a boring synthetic file with two builds investigated every day, each with the same two bugs
    val Content = "# Synthetic Build Failures\n" +
      (for (date <- 20 to 11 by -1)
        yield s"""## 2024-05-$date
             |### 1.0.1 #A$date Nightly build https://link/buildA$date
             |A1 https://link/buildA$date/A1
             |BUG-1 Describe bug 1
             |
             |A2 https://link/buildA$date/A2
             |BUG-2 Describe bug 2
             |### 1.1.2 #B$date Nightly build https://link/buildB$date
             |B1 https://link/buildB$date/B1
             |BUG-1 Describe bug 1
             |
             |B2 https://link/buildB$date/B2
             |BUG-2 Describe bug 2
             |""".stripMargin).mkString("\n")
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "failures.md").writeAll(Content)

    it("should report on the last day of investigations") {
      withGoMatching(TaskCmd, Basic / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |BUG-1 https://issues.apache.org/jira/browse/BUG-1
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A1 https://link/buildA20/A1
            |* 1.1.2 B1 https://link/buildB20/B1
            |
            |BUG-2 https://issues.apache.org/jira/browse/BUG-2
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A2 https://link/buildA20/A2
            |* 1.1.2 B2 https://link/buildB20/B2
            |""".stripMargin
      }
    }

    it("should report on the last two days of investigations") {
      withGoMatching(TaskCmd, "--days", "2", Basic / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |BUG-1 https://issues.apache.org/jira/browse/BUG-1
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A1 https://link/buildA20/A1
            |* 1.1.2 B1 https://link/buildB20/B1
            |* 1.0.1 A1 https://link/buildA19/A1
            |* 1.1.2 B1 https://link/buildB19/B1
            |
            |BUG-2 https://issues.apache.org/jira/browse/BUG-2
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A2 https://link/buildA20/A2
            |* 1.1.2 B2 https://link/buildB20/B2
            |* 1.0.1 A2 https://link/buildA19/A2
            |* 1.1.2 B2 https://link/buildB19/B2
            |""".stripMargin
      }
    }

    it("should report on the investigation before 2024-05-15") {
      withGoMatching(TaskCmd, "--until", "2024-05-15", Basic / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |BUG-1 https://issues.apache.org/jira/browse/BUG-1
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A1 https://link/buildA15/A1
            |* 1.1.2 B1 https://link/buildB15/B1
            |
            |BUG-2 https://issues.apache.org/jira/browse/BUG-2
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A2 https://link/buildA15/A2
            |* 1.1.2 B2 https://link/buildB15/B2
            |""".stripMargin
      }
    }

    it("should report on the two days investigation before 2024-05-15") {
      withGoMatching(TaskCmd, "--days", "2", "--until", "2024-05-15", Basic / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |BUG-1 https://issues.apache.org/jira/browse/BUG-1
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A1 https://link/buildA15/A1
            |* 1.1.2 B1 https://link/buildB15/B1
            |* 1.0.1 A1 https://link/buildA14/A1
            |* 1.1.2 B1 https://link/buildB14/B1
            |
            |BUG-2 https://issues.apache.org/jira/browse/BUG-2
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A2 https://link/buildA15/A2
            |* 1.1.2 B2 https://link/buildB15/B2
            |* 1.0.1 A2 https://link/buildA14/A2
            |* 1.1.2 B2 https://link/buildB14/B2
            |""".stripMargin
      }
    }

    it("should report on all days investigation before 2024-05-18") {
      withGoMatching(TaskCmd, "--all", "--after", "2024-05-18", Basic / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |BUG-1 https://issues.apache.org/jira/browse/BUG-1
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A1 https://link/buildA20/A1
            |* 1.1.2 B1 https://link/buildB20/B1
            |* 1.0.1 A1 https://link/buildA19/A1
            |* 1.1.2 B1 https://link/buildB19/B1
            |* 1.0.1 A1 https://link/buildA18/A1
            |* 1.1.2 B1 https://link/buildB18/B1
            |
            |BUG-2 https://issues.apache.org/jira/browse/BUG-2
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A2 https://link/buildA20/A2
            |* 1.1.2 B2 https://link/buildB20/B2
            |* 1.0.1 A2 https://link/buildA19/A2
            |* 1.1.2 B2 https://link/buildB19/B2
            |* 1.0.1 A2 https://link/buildA18/A2
            |* 1.1.2 B2 https://link/buildB18/B2
            |""".stripMargin
      }
    }

    it("should report on all days of investigations") {
      withGoMatching(TaskCmd, "--all", Basic / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |BUG-1 https://issues.apache.org/jira/browse/BUG-1
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A1 https://link/buildA20/A1
            |* 1.1.2 B1 https://link/buildB20/B1
            |* 1.0.1 A1 https://link/buildA19/A1
            |* 1.1.2 B1 https://link/buildB19/B1
            |* 1.0.1 A1 https://link/buildA18/A1
            |* 1.1.2 B1 https://link/buildB18/B1
            |* 1.0.1 A1 https://link/buildA17/A1
            |* 1.1.2 B1 https://link/buildB17/B1
            |* 1.0.1 A1 https://link/buildA16/A1
            |* 1.1.2 B1 https://link/buildB16/B1
            |* 1.0.1 A1 https://link/buildA15/A1
            |* 1.1.2 B1 https://link/buildB15/B1
            |* 1.0.1 A1 https://link/buildA14/A1
            |* 1.1.2 B1 https://link/buildB14/B1
            |* 1.0.1 A1 https://link/buildA13/A1
            |* 1.1.2 B1 https://link/buildB13/B1
            |* 1.0.1 A1 https://link/buildA12/A1
            |* 1.1.2 B1 https://link/buildB12/B1
            |* 1.0.1 A1 https://link/buildA11/A1
            |* 1.1.2 B1 https://link/buildB11/B1
            |
            |BUG-2 https://issues.apache.org/jira/browse/BUG-2
            |------------------------------------------------------------------------------
            |
            |* 1.0.1 A2 https://link/buildA20/A2
            |* 1.1.2 B2 https://link/buildB20/B2
            |* 1.0.1 A2 https://link/buildA19/A2
            |* 1.1.2 B2 https://link/buildB19/B2
            |* 1.0.1 A2 https://link/buildA18/A2
            |* 1.1.2 B2 https://link/buildB18/B2
            |* 1.0.1 A2 https://link/buildA17/A2
            |* 1.1.2 B2 https://link/buildB17/B2
            |* 1.0.1 A2 https://link/buildA16/A2
            |* 1.1.2 B2 https://link/buildB16/B2
            |* 1.0.1 A2 https://link/buildA15/A2
            |* 1.1.2 B2 https://link/buildB15/B2
            |* 1.0.1 A2 https://link/buildA14/A2
            |* 1.1.2 B2 https://link/buildB14/B2
            |* 1.0.1 A2 https://link/buildA13/A2
            |* 1.1.2 B2 https://link/buildB13/B2
            |* 1.0.1 A2 https://link/buildA12/A2
            |* 1.1.2 B2 https://link/buildB12/B2
            |* 1.0.1 A2 https://link/buildA11/A2
            |* 1.1.2 B2 https://link/buildB11/B2
            |""".stripMargin
      }
    }
  }

  describe("When automatically adding failed builds") {

    val Scenario = (Tmp / "addfails").createDirectory()

    // The header part for an original file section without any investigations
    val OriginalFailureSectionHeader1 =
      """Add Fails Build Failures
        |==============================================================================
        |
        |* A note here
        |""".stripMargin

    // Some initial investigations that can be put in the file
    val OriginalFailureSectionHeaders2 =
      """2024-01-02
        |------------------------------------------------------------------------------
        |
        |### 1.3 Nightly build https://build3
        |
        |Fail3
        |BUG-1
        |
        |### 1.2 Nightly build https://build2
        |
        |Fail2
        |BUG-1
        |
        |2024-01-01
        |------------------------------------------------------------------------------
        |
        |### 1.1 Nightly build https://build1
        |
        |Fail1
        |BUG-1
        |
        |### 1.0 Nightly build https://build0
        |
        |Fail0
        |BUG-1
        |""".stripMargin

    // Create an actual file for returning and the URL that points to it
    File(Scenario / "run_fails.json").writeAll("""{"workflow_runs":[
       |{"id":0,"name":"a5","run_number":5,"head_branch":"main","html_url":"https://build5","created_at":"2024-01-05"},
       |{"id":1,"name":"a4","run_number":4,"head_branch":"master","html_url":"https://build4","created_at":"2024-01-04"},
       |{"id":2,"name":"a3","run_number":3,"head_branch":"release-1.3","html_url":"https://build3","created_at":"2024-01-03"},
       |{"id":3,"name":"a2","run_number":2,"head_branch":"release-1.2","html_url":"https://build2","created_at":"2024-01-02"},
       |{"id":4,"name":"a1","run_number":1,"head_branch":"release-1.1","html_url":"https://build1","created_at":"2024-01-01"}]}
       |""".stripMargin)
    val RunFailsTemplate = s"file://$Scenario/%s"

    it("should overwrite the file to include uninvestigated build failures") {
      // This file will be overwritten
      File(Scenario / "failures.md").writeAll(OriginalFailureSectionHeader1 + OriginalFailureSectionHeaders2)

      // This is the mock return result from the API call, constructed by overriding the system property to
      // create a file URL instead of a REST API.

      sys.props("run.fails.template") = RunFailsTemplate
      withGoMatching(TaskCmd, "--add-fails", "run_fails.json", Scenario / "failures.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |Unknown
            |------------------------------------------------------------------------------
            |
            |* main TODO
            |* master TODO
            |""".stripMargin
      }
      sys.props.remove("run.fails.template")

      val today = DateTimeFormatter.ofPattern("yyyy-MM-dd").format(LocalDate.now())
      (Scenario / "failures.md").toFile.slurp() shouldBe
        s"""$OriginalFailureSectionHeader1
          |$today
          |------------------------------------------------------------------------------
          |
          |### main a5 #5 (2024-01-05) https://build5
          |
          |TODO
          |
          |### master a4 #4 (2024-01-04) https://build4
          |
          |TODO
          |
          |$OriginalFailureSectionHeaders2""".stripMargin

    }

    it("should rewrite branches to fetch build versions") {
      // This file will be overwritten
      File(Scenario / "failures.md").writeAll(OriginalFailureSectionHeader1)

      // This is the mock return result from the API call, constructed by overriding the system property to
      // create a file URL instead of a REST API.

      val yyy = (Scenario / "failures.md").toFile.slurp()

      sys.props("run.fails.template") = RunFailsTemplate
      withGoMatching(TaskCmd, "--add-fails", "run_fails.json", "--main-version", "1.99", Scenario / "failures.md") {
        case (stdout, stderr) =>
          stderr shouldBe empty
          stdout shouldBe
            """By Issue
            |==============================================================================
            |
            |Unknown
            |------------------------------------------------------------------------------
            |
            |* 1.99 TODO
            |* 1.99 TODO
            |* 1.3 TODO
            |* 1.2 TODO
            |* 1.1 TODO
            |""".stripMargin
      }
      sys.props.remove("run.fails.template")

      val today = DateTimeFormatter.ofPattern("yyyy-MM-dd").format(LocalDate.now())
      val xxx = (Scenario / "failures.md").toFile.slurp()
      (Scenario / "failures.md").toFile.slurp() shouldBe
        s"""$OriginalFailureSectionHeader1
           |$today
           |------------------------------------------------------------------------------
           |
           |### 1.99 a5 #5 (2024-01-05) https://build5
           |
           |TODO
           |
           |### 1.99 a4 #4 (2024-01-04) https://build4
           |
           |TODO
           |
           |### 1.3 a3 #3 (2024-01-03) https://build3
           |
           |TODO
           |
           |### 1.2 a2 #2 (2024-01-02) https://build2
           |
           |TODO
           |
           |### 1.1 a1 #1 (2024-01-01) https://build1
           |
           |TODO
           |""".stripMargin

    }
  }

  describe("When parsing a sample file") {

    val Sample = (Tmp / "sample").createDirectory()
    File(Sample / "failures.md").writeAll("""# Flink Build Failures
        |## 2024-05-03
        |
        |### 1.20 Nightly (beta) #270 https://github.com/apache/flink/actions/runs/8917610620
        |
        |Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491172511#step:10:8154
        |FLINK-35041 testSharedStateReRegistration
        |
        |Java 21 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491154789#step:10:8873
        |FLINK-35041 testSharedStateReRegistration
        |
        |### 1.18 Nightly (beta) #268 https://github.com/apache/flink/actions/runs/8904361381
        |
        |AdaptiveScheduler / Test (module: table) https://github.com/apache/flink/actions/runs/8904361381/job/24453748069#step:10:14980
        |FLINK-34227 Job doesn't disconnect from ResourceManager
        |
        |### 1.20 Nightly (beta) #267 https://github.com/apache/flink/actions/runs/8904361061
        |
        |Java 21 / Test (module: misc) https://github.com/apache/flink/commit/80af4d502318348ba15a8f75a2a622ce9dbdc968/checks/24453751708/logs
        |FLINK-35095 ExecutionEnvironmentImplTest.testFromSource
        |
        |### 1.19 Nightly (beta) #266 https://github.com/apache/flink/actions/runs/8904361031
        |
        |AdaptiveScheduler / Compile https://github.com/apache/flink/commit/ac4aa35c6e2e2da87760ffbf45d85888b1976c2f/checks/24453516397/logs
        |FLINK-35002 timeout to ArtifactService
        |
        |### 1.20 Flink CI (beta) #411 https://github.com/apache/flink/actions/runs/8901164251
        |
        |Default (Java 8) / Test (module: tests) https://github.com/apache/flink/actions/runs/8901164251/job/24444807095#step:10:7971
        |FLINK-28440 Could not restore keyed state backend
        |
        |## 2024-05-02
        |
        |### 1.20 #20240501.1 https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=results
        |
        |e2e_2_cron_adaptive_scheduler https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=fb37c667-81b7-5c22-dd91-846535e99a97&t=011e961e-597c-5c96-04fe-7941c8b83f23&l=3076
        |FLINK-35284 File Sink end-to-end Did not finish after 900 seconds
        |
        |test_cron_hadoop313 core https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=d89de3df-4600-5585-dadc-9bbc9a5e661c&t=be5a4b15-4b23-56b1-7582-795f58a645a2&l=9001
        |FLINK-35041 testSharedStateReRegistration
        |
        |test_cron_adaptive_scheduler tests https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=8fd9202e-fd17-5b26-353c-ac1ff76c8f28&t=bc77b88f-20e6-5fb3-ac3b-0b6efcca48c5&l=1068
        |FLINK-34273 git fetch fails
        |
        |## 2024-04-30
        |
        |### 1.20 Nightly (beta) #263 https://github.com/apache/flink/actions/runs/8888221960
        |
        |Java 11 / Test (module: core)  https://github.com/apache/flink/actions/runs/8888221960/job/24404966761#step:10:7787
        |FLINK-35041 testSharedStateReRegistration
        |
        |Java 21 / Test (module: misc) https://github.com/apache/flink/actions/runs/8888221960/job/24404965886#step:10:22919
        |FLINK-18476 Failed to start python process
        |
        |AdaptiveScheduler / Test (module: core) https://github.com/apache/flink/actions/runs/8888221960/job/24404939797#step:10:8361
        |FLINK-35041 testSharedStateReRegistration
        |
        |### 1.20 Flink CI (beta) #403 https://github.com/apache/flink/actions/runs/8887882381
        |
        |Default (Java 8) / Test (module: tests) https://github.com/apache/flink/actions/runs/8887882381/job/24404087819#step:10:8262
        |FLINK-28440 Could not restore keyed state backend
        |
        |### 1.20 Flink CI (beta) #402 https://github.com/apache/flink/actions/runs/8874021289
        |
        |Default (Java 8) / Test (module: core) https://github.com/apache/flink/actions/runs/8874021289/job/24361049250#step:10:8308
        |FLINK-35041 testSharedStateReRegistration
        |
        |### 1.20 Nightly (beta) #262 https://github.com/apache/flink/actions/runs/8872328953
        |
        |Java 8 / Compile https://github.com/apache/flink/commit/e412402ca4dfc438e28fb990dc53ea7809430aee/checks/24356511040/logs
        |FLINK-35002 timeout to ArtifactService
        |
        |Java 17 / Test (module: core) https://github.com/apache/flink/actions/runs/8872328953/job/24356752585#step:10:8911
        |FLINK-35041 testSharedStateReRegistration
        |
        |### 1.18 Nightly (beta) #260 https://github.com/apache/flink/actions/runs/8872328847
        |
        |Java 11 / Test (module: misc) https://github.com/apache/flink/actions/runs/8872328847/job/24356773170#step:10:21780
        |FLINK-34645 testFinishBundleTriggeredByCount
        |
        |### 1.20 Nightly (beta) #258 https://github.com/apache/flink/actions/runs/8864296312
        |
        |Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8864296312/job/24339779126#step:10:9083
        |FLINK-35041 testSharedStateReRegistration
        |""".stripMargin)

    it("should report on the last day of investigations") {
      withGoMatching(TaskCmd, Sample / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |FLINK-28440 https://issues.apache.org/jira/browse/FLINK-28440
            |------------------------------------------------------------------------------
            |
            |* 1.20 Default (Java 8) / Test (module: tests) https://github.com/apache/flink/actions/runs/8901164251/job/24444807095#step:10:7971
            |
            |FLINK-34227 https://issues.apache.org/jira/browse/FLINK-34227
            |------------------------------------------------------------------------------
            |
            |* 1.18 AdaptiveScheduler / Test (module: table) https://github.com/apache/flink/actions/runs/8904361381/job/24453748069#step:10:14980
            |
            |FLINK-35002 https://issues.apache.org/jira/browse/FLINK-35002
            |------------------------------------------------------------------------------
            |
            |* 1.19 AdaptiveScheduler / Compile https://github.com/apache/flink/commit/ac4aa35c6e2e2da87760ffbf45d85888b1976c2f/checks/24453516397/logs
            |
            |FLINK-35041 https://issues.apache.org/jira/browse/FLINK-35041
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491172511#step:10:8154
            |* 1.20 Java 21 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491154789#step:10:8873
            |
            |FLINK-35095 https://issues.apache.org/jira/browse/FLINK-35095
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 21 / Test (module: misc) https://github.com/apache/flink/commit/80af4d502318348ba15a8f75a2a622ce9dbdc968/checks/24453751708/logs
            |""".stripMargin
      }
    }

    it("should report on the last two days of investigations") {
      withGoMatching(TaskCmd, "--days", "2", Sample / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |FLINK-28440 https://issues.apache.org/jira/browse/FLINK-28440
            |------------------------------------------------------------------------------
            |
            |* 1.20 Default (Java 8) / Test (module: tests) https://github.com/apache/flink/actions/runs/8901164251/job/24444807095#step:10:7971
            |
            |FLINK-34227 https://issues.apache.org/jira/browse/FLINK-34227
            |------------------------------------------------------------------------------
            |
            |* 1.18 AdaptiveScheduler / Test (module: table) https://github.com/apache/flink/actions/runs/8904361381/job/24453748069#step:10:14980
            |
            |FLINK-34273 https://issues.apache.org/jira/browse/FLINK-34273
            |------------------------------------------------------------------------------
            |
            |* 1.20 test_cron_adaptive_scheduler tests https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=8fd9202e-fd17-5b26-353c-ac1ff76c8f28&t=bc77b88f-20e6-5fb3-ac3b-0b6efcca48c5&l=1068
            |
            |FLINK-35002 https://issues.apache.org/jira/browse/FLINK-35002
            |------------------------------------------------------------------------------
            |
            |* 1.19 AdaptiveScheduler / Compile https://github.com/apache/flink/commit/ac4aa35c6e2e2da87760ffbf45d85888b1976c2f/checks/24453516397/logs
            |
            |FLINK-35041 https://issues.apache.org/jira/browse/FLINK-35041
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491172511#step:10:8154
            |* 1.20 Java 21 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491154789#step:10:8873
            |* 1.20 test_cron_hadoop313 core https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=d89de3df-4600-5585-dadc-9bbc9a5e661c&t=be5a4b15-4b23-56b1-7582-795f58a645a2&l=9001
            |
            |FLINK-35095 https://issues.apache.org/jira/browse/FLINK-35095
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 21 / Test (module: misc) https://github.com/apache/flink/commit/80af4d502318348ba15a8f75a2a622ce9dbdc968/checks/24453751708/logs
            |
            |FLINK-35284 https://issues.apache.org/jira/browse/FLINK-35284
            |------------------------------------------------------------------------------
            |
            |* 1.20 e2e_2_cron_adaptive_scheduler https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=fb37c667-81b7-5c22-dd91-846535e99a97&t=011e961e-597c-5c96-04fe-7941c8b83f23&l=3076
            |""".stripMargin
      }
    }

    it("should report on all days investigations") {
      withGoMatching(TaskCmd, "--all", Sample / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Issue
            |==============================================================================
            |
            |FLINK-18476 https://issues.apache.org/jira/browse/FLINK-18476
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 21 / Test (module: misc) https://github.com/apache/flink/actions/runs/8888221960/job/24404965886#step:10:22919
            |
            |FLINK-28440 https://issues.apache.org/jira/browse/FLINK-28440
            |------------------------------------------------------------------------------
            |
            |* 1.20 Default (Java 8) / Test (module: tests) https://github.com/apache/flink/actions/runs/8901164251/job/24444807095#step:10:7971
            |* 1.20 Default (Java 8) / Test (module: tests) https://github.com/apache/flink/actions/runs/8887882381/job/24404087819#step:10:8262
            |
            |FLINK-34227 https://issues.apache.org/jira/browse/FLINK-34227
            |------------------------------------------------------------------------------
            |
            |* 1.18 AdaptiveScheduler / Test (module: table) https://github.com/apache/flink/actions/runs/8904361381/job/24453748069#step:10:14980
            |
            |FLINK-34273 https://issues.apache.org/jira/browse/FLINK-34273
            |------------------------------------------------------------------------------
            |
            |* 1.20 test_cron_adaptive_scheduler tests https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=8fd9202e-fd17-5b26-353c-ac1ff76c8f28&t=bc77b88f-20e6-5fb3-ac3b-0b6efcca48c5&l=1068
            |
            |FLINK-34645 https://issues.apache.org/jira/browse/FLINK-34645
            |------------------------------------------------------------------------------
            |
            |* 1.18 Java 11 / Test (module: misc) https://github.com/apache/flink/actions/runs/8872328847/job/24356773170#step:10:21780
            |
            |FLINK-35002 https://issues.apache.org/jira/browse/FLINK-35002
            |------------------------------------------------------------------------------
            |
            |* 1.19 AdaptiveScheduler / Compile https://github.com/apache/flink/commit/ac4aa35c6e2e2da87760ffbf45d85888b1976c2f/checks/24453516397/logs
            |* 1.20 Java 8 / Compile https://github.com/apache/flink/commit/e412402ca4dfc438e28fb990dc53ea7809430aee/checks/24356511040/logs
            |
            |FLINK-35041 https://issues.apache.org/jira/browse/FLINK-35041
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491172511#step:10:8154
            |* 1.20 Java 21 / Test (module: core) https://github.com/apache/flink/actions/runs/8917610620/job/24491154789#step:10:8873
            |* 1.20 test_cron_hadoop313 core https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=d89de3df-4600-5585-dadc-9bbc9a5e661c&t=be5a4b15-4b23-56b1-7582-795f58a645a2&l=9001
            |* 1.20 Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8888221960/job/24404966761#step:10:7787
            |* 1.20 AdaptiveScheduler / Test (module: core) https://github.com/apache/flink/actions/runs/8888221960/job/24404939797#step:10:8361
            |* 1.20 Default (Java 8) / Test (module: core) https://github.com/apache/flink/actions/runs/8874021289/job/24361049250#step:10:8308
            |* 1.20 Java 17 / Test (module: core) https://github.com/apache/flink/actions/runs/8872328953/job/24356752585#step:10:8911
            |* 1.20 Java 11 / Test (module: core) https://github.com/apache/flink/actions/runs/8864296312/job/24339779126#step:10:9083
            |
            |FLINK-35095 https://issues.apache.org/jira/browse/FLINK-35095
            |------------------------------------------------------------------------------
            |
            |* 1.20 Java 21 / Test (module: misc) https://github.com/apache/flink/commit/80af4d502318348ba15a8f75a2a622ce9dbdc968/checks/24453751708/logs
            |
            |FLINK-35284 https://issues.apache.org/jira/browse/FLINK-35284
            |------------------------------------------------------------------------------
            |
            |* 1.20 e2e_2_cron_adaptive_scheduler https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=fb37c667-81b7-5c22-dd91-846535e99a97&t=011e961e-597c-5c96-04fe-7941c8b83f23&l=3076
            |""".stripMargin
      }
    }

    it("should rewrite all days investigations as markdown notifications") {
      withGoMatching(TaskCmd, "--all", "--markdown-msg", Sample / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Failure
            |==============================================================================
            |
            |:red_circle:  Build *[1.20 Nightly (beta) #270](https://github.com/apache/flink/actions/runs/8917610620)* failed
            |------------------------------------------------------------------------------
            |
            |* [Java 11 / Test (module: core)](https://github.com/apache/flink/actions/runs/8917610620/job/24491172511#step:10:8154) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |* [Java 21 / Test (module: core)](https://github.com/apache/flink/actions/runs/8917610620/job/24491154789#step:10:8873) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |
            |:red_circle:  Build *[1.18 Nightly (beta) #268](https://github.com/apache/flink/actions/runs/8904361381)* failed
            |------------------------------------------------------------------------------
            |
            |* [AdaptiveScheduler / Test (module: table)](https://github.com/apache/flink/actions/runs/8904361381/job/24453748069#step:10:14980) *[FLINK-34227](https://issues.apache.org/jira/browse/FLINK-34227)* Job doesn't disconnect from ResourceManager
            |
            |:red_circle:  Build *[1.20 Nightly (beta) #267](https://github.com/apache/flink/actions/runs/8904361061)* failed
            |------------------------------------------------------------------------------
            |
            |* [Java 21 / Test (module: misc)](https://github.com/apache/flink/commit/80af4d502318348ba15a8f75a2a622ce9dbdc968/checks/24453751708/logs) *[FLINK-35095](https://issues.apache.org/jira/browse/FLINK-35095)* ExecutionEnvironmentImplTest.testFromSource
            |
            |:red_circle:  Build *[1.19 Nightly (beta) #266](https://github.com/apache/flink/actions/runs/8904361031)* failed
            |------------------------------------------------------------------------------
            |
            |* [AdaptiveScheduler / Compile](https://github.com/apache/flink/commit/ac4aa35c6e2e2da87760ffbf45d85888b1976c2f/checks/24453516397/logs) *[FLINK-35002](https://issues.apache.org/jira/browse/FLINK-35002)* timeout to ArtifactService
            |
            |:red_circle:  Build *[1.20 Flink CI (beta) #411](https://github.com/apache/flink/actions/runs/8901164251)* failed
            |------------------------------------------------------------------------------
            |
            |* [Default (Java 8) / Test (module: tests)](https://github.com/apache/flink/actions/runs/8901164251/job/24444807095#step:10:7971) *[FLINK-28440](https://issues.apache.org/jira/browse/FLINK-28440)* Could not restore keyed state backend
            |
            |:red_circle:  Build *[1.20 #20240501.1](https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=results)* failed
            |------------------------------------------------------------------------------
            |
            |* [e2e_2_cron_adaptive_scheduler](https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=fb37c667-81b7-5c22-dd91-846535e99a97&t=011e961e-597c-5c96-04fe-7941c8b83f23&l=3076) *[FLINK-35284](https://issues.apache.org/jira/browse/FLINK-35284)* File Sink end-to-end Did not finish after 900 seconds
            |* [test_cron_hadoop313 core](https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=d89de3df-4600-5585-dadc-9bbc9a5e661c&t=be5a4b15-4b23-56b1-7582-795f58a645a2&l=9001) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |* [test_cron_adaptive_scheduler tests](https://dev.azure.com/apache-flink/apache-flink/_build/results?buildId=59303&view=logs&j=8fd9202e-fd17-5b26-353c-ac1ff76c8f28&t=bc77b88f-20e6-5fb3-ac3b-0b6efcca48c5&l=1068) *[FLINK-34273](https://issues.apache.org/jira/browse/FLINK-34273)* git fetch fails
            |
            |:red_circle:  Build *[1.20 Nightly (beta) #263](https://github.com/apache/flink/actions/runs/8888221960)* failed
            |------------------------------------------------------------------------------
            |
            |* [Java 11 / Test (module: core)](https://github.com/apache/flink/actions/runs/8888221960/job/24404966761#step:10:7787) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |* [Java 21 / Test (module: misc)](https://github.com/apache/flink/actions/runs/8888221960/job/24404965886#step:10:22919) *[FLINK-18476](https://issues.apache.org/jira/browse/FLINK-18476)* Failed to start python process
            |* [AdaptiveScheduler / Test (module: core)](https://github.com/apache/flink/actions/runs/8888221960/job/24404939797#step:10:8361) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |
            |:red_circle:  Build *[1.20 Flink CI (beta) #403](https://github.com/apache/flink/actions/runs/8887882381)* failed
            |------------------------------------------------------------------------------
            |
            |* [Default (Java 8) / Test (module: tests)](https://github.com/apache/flink/actions/runs/8887882381/job/24404087819#step:10:8262) *[FLINK-28440](https://issues.apache.org/jira/browse/FLINK-28440)* Could not restore keyed state backend
            |
            |:red_circle:  Build *[1.20 Flink CI (beta) #402](https://github.com/apache/flink/actions/runs/8874021289)* failed
            |------------------------------------------------------------------------------
            |
            |* [Default (Java 8) / Test (module: core)](https://github.com/apache/flink/actions/runs/8874021289/job/24361049250#step:10:8308) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |
            |:red_circle:  Build *[1.20 Nightly (beta) #262](https://github.com/apache/flink/actions/runs/8872328953)* failed
            |------------------------------------------------------------------------------
            |
            |* [Java 8 / Compile](https://github.com/apache/flink/commit/e412402ca4dfc438e28fb990dc53ea7809430aee/checks/24356511040/logs) *[FLINK-35002](https://issues.apache.org/jira/browse/FLINK-35002)* timeout to ArtifactService
            |* [Java 17 / Test (module: core)](https://github.com/apache/flink/actions/runs/8872328953/job/24356752585#step:10:8911) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |
            |:red_circle:  Build *[1.18 Nightly (beta) #260](https://github.com/apache/flink/actions/runs/8872328847)* failed
            |------------------------------------------------------------------------------
            |
            |* [Java 11 / Test (module: misc)](https://github.com/apache/flink/actions/runs/8872328847/job/24356773170#step:10:21780) *[FLINK-34645](https://issues.apache.org/jira/browse/FLINK-34645)* testFinishBundleTriggeredByCount
            |
            |:red_circle:  Build *[1.20 Nightly (beta) #258](https://github.com/apache/flink/actions/runs/8864296312)* failed
            |------------------------------------------------------------------------------
            |
            |* [Java 11 / Test (module: core)](https://github.com/apache/flink/actions/runs/8864296312/job/24339779126#step:10:9083) *[FLINK-35041](https://issues.apache.org/jira/browse/FLINK-35041)* testSharedStateReRegistration
            |""".stripMargin
      }
    }
  }
}
