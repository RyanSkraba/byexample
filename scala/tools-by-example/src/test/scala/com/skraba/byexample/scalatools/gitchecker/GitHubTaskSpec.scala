package com.skraba.byexample.scalatools.gitchecker

import com.skraba.byexample.scalatools.filerenamer.WithTmpSrcDst
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
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

  describe(s"Running $MainName $TaskCmd openPrs") {
    val (src, dst) = createSrcDst("openPrs")
    it("should get the open PRs for avro") {
      val mock = """{"total_count": 100}"""
      github(src, dst)("openPr", "apache/avro", "--plain", "--mock", mock) shouldBe "100"
    }
  }
}
