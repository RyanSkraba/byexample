package com.skraba.byexample.scalatools.gitchecker

import com.skraba.byexample.scalatools.filerenamer.WithTmpSrcDst
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}
import com.tinfoiled.markd.Markd

import java.nio.file.Path

/** Unit tests for [[CherryPickerReportTask]]. */
class CherryPickerReportTaskSpec
    extends MultiTaskMainSpec(GitCheckerGo, Some(CherryPickerReportTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the MonthlyTask with the source and destination directories. */
  def rewriteDate(src: Path, dst: Path)(args: Any*): String =
    withGoStdoutSrcDst(src, dst, "\\[main .*?]" -> "<MAIN>")(TaskCmd +: args: _*)

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnMissingOptValue("--src")
    itShouldThrowOnMissingOptValue("--dst")
    itShouldThrowOnMissingOptValue("--lTag")
    itShouldThrowOnMissingOptValue("--rTag")

    itShouldBeAnExistingDir()("--src", "<>")
  }

  /** Make a commit and extract the replacements for the hash. */
  def gitCommitAndExtractReplacements(repo: Path, message: String, tag: String): Seq[(String, String)] = {
    git(repo, "commit", "-m", message)
    val hash = git(repo, "rev-parse", "HEAD")
    Seq(hash -> s"<$tag-LONG>", hash.take(10) -> s"<$tag>")
  }

  describe(s"Running $MainName $TaskCmd") {

    val (src, dst) = createSrcDst("repo", "a", "b", "c")

    val replacements = {
      git(src, "init", ".")
      git(src, "config", "user.name", "user")
      git(src, "config", "user.email", "user@example.com")

      // The initial commit is used as a reference for "next" commands
      git(src, "add", "a")
      val commitA = gitCommitAndExtractReplacements(src, "Initial commit.", "A")

      git(src, "switch", "-C", "branch")
      git(src, "add", "b")
      val commitB = gitCommitAndExtractReplacements(src, "Branch commit.", "B")

      git(src, "switch", "main")
      git(src, "add", "c")
      val commitC = gitCommitAndExtractReplacements(src, "Main commit.", "C")
      commitA ++ commitB ++ commitC
    }

    it("create a new cherry-pick report") {
      (src / ".git").toFile should exist

      val stdout = withGoStdoutSrcDst(src, dst)(TaskCmd, "--plain", "--src" -> src, "--dst" -> dst / "status.md")
      stdout shouldBe
        """Arguments:
          |      repo:: <SRC>
          |      rTag:: main
          |      lTag:: branch
          | statusDoc:: Some(<DST>/status.md)
          |
          |Git command:
          |--no-pager log --left-right --graph --cherry-pick --pretty=format:%H%x00%an%x00%ae%x00%at%x00%cn%x00%ce%x00%ct%x00%d%x00%s%x00%B%x00 branch...main
          |
          |""".stripMargin

      val report = replace((dst / "status.md").slurp(), replacements: _*)
      Markd.parse(report).build().toString shouldBe
        """CherryPickerReport
          |==============================================================================
          |
          || Project Info |        |
          ||--------------|--------|
          || Left         | branch |
          || Left N       | 1      |
          || Left Dups    | 0      |
          || Right        | main   |
          || Right N      | 1      |
          || Right Dups   | 0      |
          |
          |Left
          |==============================================================================
          |
          || Commit | Subject        | Notes |
          ||--------|----------------|-------|
          || [<B>]  | Branch commit. |       |
          |
          |Right
          |==============================================================================
          |
          || Commit | Subject      | Notes |
          ||--------|--------------|-------|
          || [<C>]  | Main commit. |       |
          |
          |References
          |==============================================================================
          |
          |[<B>]: https://github.com/apache/avro/commit/<B-LONG> "Branch commit."
          |[<C>]: https://github.com/apache/avro/commit/<C-LONG> "Main commit."
          |""".stripMargin
    }
  }
}
