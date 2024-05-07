package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.BuildFailureReportTask.FailedStep
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
      FailedStep.parseBuildTitle("") shouldBe ("", "", "")
    }

    it("should find build attributes") {
      FailedStep.parseBuildTitle("abc") shouldBe ("abc", "", "")
      FailedStep.parseBuildTitle("abc ") shouldBe ("abc", "", "")
      FailedStep.parseBuildTitle(" abc ") shouldBe ("", "abc", "")
      FailedStep.parseBuildTitle(" abc def ") shouldBe ("", "abc def", "")
      FailedStep.parseBuildTitle("abc def ghi") shouldBe ("abc", "def ghi", "")
      FailedStep.parseBuildTitle("http://link") shouldBe ("", "", "http://link")
      FailedStep.parseBuildTitle("abc http://link") shouldBe ("abc", "", "http://link")
      FailedStep.parseBuildTitle(" abc http://link") shouldBe ("", "abc", "http://link")
      FailedStep.parseBuildTitle("abc def ghi http://link") shouldBe ("abc", "def ghi", "http://link")
      FailedStep.parseBuildTitle("abc def ghi://link") shouldBe ("abc", "def ghi://link", "")
      FailedStep.parseBuildTitle("abc def ghi xhttp://link") shouldBe ("abc", "def ghi xhttp://link", "")
    }
  }

  describe("Parsing job content") {
    it("should return empty values for an empty string") {
      FailedStep.parseJobAndJiraInfo("") shouldBe ("", "", "", "")
      FailedStep.parseJobAndJiraInfo("", "") shouldBe ("", "", "", "")
      FailedStep.parseJobAndJiraInfo("", "", "ignored") shouldBe ("", "", "", "")
    }

    it("should find build steps and URLS in the first line") {
      // No URL link
      FailedStep.parseJobAndJiraInfo("info") shouldBe ("info", "", "", "")
      FailedStep.parseJobAndJiraInfo("info", "") shouldBe ("info", "", "", "")
      FailedStep.parseJobAndJiraInfo(" info ") shouldBe ("info", "", "", "")
      FailedStep.parseJobAndJiraInfo(" info ", "") shouldBe ("info", "", "", "")
      FailedStep.parseJobAndJiraInfo("info abc") shouldBe ("info abc", "", "", "")
      FailedStep.parseJobAndJiraInfo("info abc", "") shouldBe ("info abc", "", "", "")
      FailedStep.parseJobAndJiraInfo("info htp://link") shouldBe ("info htp://link", "", "", "")
      FailedStep.parseJobAndJiraInfo("info htp://link", "") shouldBe ("info htp://link", "", "", "")
      FailedStep.parseJobAndJiraInfo("info xhttps://link") shouldBe ("info xhttps://link", "", "", "")
      FailedStep.parseJobAndJiraInfo("info xhttps://link", "") shouldBe ("info xhttps://link", "", "", "")

      // With URL links
      FailedStep.parseJobAndJiraInfo("http://link") shouldBe ("", "http://link", "", "")
      FailedStep.parseJobAndJiraInfo("http://link", "") shouldBe ("", "http://link", "", "")
      FailedStep.parseJobAndJiraInfo("info http://link") shouldBe ("info", "http://link", "", "")
      FailedStep.parseJobAndJiraInfo("info http://link", "") shouldBe ("info", "http://link", "", "")
      FailedStep.parseJobAndJiraInfo("info abc http://link") shouldBe ("info abc", "http://link", "", "")
      FailedStep.parseJobAndJiraInfo("info abc http://link", "") shouldBe ("info abc", "http://link", "", "")
      FailedStep.parseJobAndJiraInfo("info https://link") shouldBe ("info", "https://link", "", "")
      FailedStep.parseJobAndJiraInfo("info https://link", "") shouldBe ("info", "https://link", "", "")
    }

    it("should find defect/jira information in the second line") {
      FailedStep.parseJobAndJiraInfo("", "info") shouldBe ("", "", "info", "")
      FailedStep.parseJobAndJiraInfo("", "info", "ignored") shouldBe ("", "", "info", "")
      FailedStep.parseJobAndJiraInfo("", " info ") shouldBe ("", "", "", "info")
      FailedStep.parseJobAndJiraInfo("", "info abc") shouldBe ("", "", "info", "abc")
      FailedStep.parseJobAndJiraInfo("", "info abc def") shouldBe ("", "", "info", "abc def")
      FailedStep.parseJobAndJiraInfo("", "info abc def", "ignored") shouldBe ("", "", "info", "abc def")
      FailedStep.parseJobAndJiraInfo("", " info abc def", "ignored") shouldBe ("", "", "", "info abc def")
    }
  }

  describe("When parsing a sample file") {
    val Basic = (Tmp / "basic").createDirectory()
    File(Basic / "failures.md").writeAll("""# Flink Build Failures
        |
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
      withGoMatching(TaskCmd, Basic / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Jira
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

    it("should report on all days investigations") {
      withGoMatching(TaskCmd, "--all", Basic / "failures.md") { case (stdout, _) =>
        stdout shouldBe
          """By Jira
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
  }
}
