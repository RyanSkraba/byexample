package com.skraba.byexample.scalatools.gitchecker

import com.skraba.byexample.scalatools.filerenamer.WithTmpSrcDst
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}

import java.nio.file.Path
import scala.sys.process.Process

/** Unit tests for [[RewriteDateTask]]. */
class RewriteDateTaskSpec
    extends MultiTaskMainSpec(GitCheckerGo, Some(RewriteDateTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the MonthlyTask with the source and destination directories. */
  def rewriteDate(src: Path, dst: Path)(args: Any*): String =
    withGoStdoutSrcDst(src, dst, "\\[(main|master) .*?]" -> "<MAIN>", "GPG_FAKED_DATE=\"\\d+\"" -> "<GPGDATE>")(
      TaskCmd +: args: _*
    )

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnMissingOptValue("--src")
    itShouldThrowOnMissingOptValue("--fuzz")

    itShouldBeAnExistingDir()("--src", "<>")
  }

  describe(s"Running $MainName $TaskCmd") {

    val (src, dst) = createSrcDst("repo", "a", "b")
    Git(src, "init", ".")
    Git(src, "config", "user.name", "user")
    Git(src, "config", "user.email", "user@example.com")

    // The initial commit is used as a reference for "next" commands
    Git(src, "add", "a")
    Git(src, "commit", "-m", "Initial commit.")
    val stdout = rewriteDate(src, dst)("--src" -> src, "--plain", "2026-02-14T12:34:56")
    Git(src, "add", "b")
    Git(src, "commit", "-m", "Last commit.")

    it("should have rewritten the date of the initial commit") {
      (src / ".git").toFile should exist

      stdout shouldBe
        """Succeeded parsing ISO
          |
          |      fuzz: 0.1 / 0.0 / 0s
          | base date: 2026-02-14T12:34:56
          |  adjusted: 2026-02-14T12:34:56 (0s)
          |    fuzzed: 2026-02-14T12:34:56 (0s)
          |<GPGDATE> GIT_COMMITTER_DATE="2026-02-14T12:34:56" \
          |    git -c "gpg.program=/tmp/gpgWithRewrite.sh" commit --amend --no-edit --date 2026-02-14T12:34:56
          |<MAIN> Initial commit.
          | Date: Sat Feb 14 12:34:56 2026 +0100
          | 1 file changed, 1 insertion(+)
          | create mode 100644 a
          |""".stripMargin
    }

    it("should rewrite the last commit to the next exact day") {
      rewriteDate(src, dst)("--src" -> src, "--fuzz" -> 0, "--plain", "next1day") shouldBe
        """      fuzz: 0.0 / 0.0 / 0s
          | base date: 2026-02-14T12:34:56
          |  adjusted: 2026-02-15T12:34:56 (86400s)
          |    fuzzed: 2026-02-15T12:34:56 (86400s)
          |<GPGDATE> GIT_COMMITTER_DATE="2026-02-15T12:34:56" \
          |    git -c "gpg.program=/tmp/gpgWithRewrite.sh" commit --amend --no-edit --date 2026-02-15T12:34:56
          |<MAIN> Last commit.
          | Date: Sun Feb 15 12:34:56 2026 +0100
          | 1 file changed, 1 insertion(+)
          | create mode 100644 b
          |""".stripMargin
    }
  }
}
