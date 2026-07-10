package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}

import java.nio.file.Path

/** Unit tests for [[MonthifyTask]]. */
class MonthifyTaskSpec
    extends MultiTaskMainSpec(FileRenamerGo, Some(MonthifyTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the MonthlyTask with the source and destination directories. */
  def monthify(src: Path, dst: Path)(args: Any*): String = withGoStdoutSrcDst(src, dst)(TaskCmd +: args: _*)

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnMissingOptValue("--src")
    itShouldThrowOnMissingOptValue("--dst")
    itShouldThrowOnMissingOptValue("--prefix")

    itShouldBeAnExistingDir()("--src", "<>")
    itShouldBeAnExistingDir()("--src", ExistingFile.getParent, "--dst", "<>")
  }

  describe(s"Running $MainName $TaskCmd") {
    val (src, dst) = createSrcDst(
      "monthify",
      "other.txt",
      "IMG_20240101_123456.jpg",
      "20240102_123456.jpg",
      "20240103_123456Stuff.jpg",
      "20240104_123456.txt",
      "20240201_123456.txt"
    )

    it("should move files") {
      // Running the first time tests a dry run
      {
        val stdout = monthify(src, dst)("--dryRun", "--plain", "--src", src, "--dst", dst, "--prefix", "back")
        stdout shouldBe """Destination: <DST>
                          |  back202401: Moving files (4)....
                          |  back202402: Moving files (1).
                          |""".stripMargin

        dst.files shouldBe empty
        dst.dirs shouldBe empty
        src.files should have size 6
      }

      // Running the first time should move all of the files
      {
        val stdout = monthify(src, dst)("--src", src, "--dst", dst, "--prefix", "back")
        stdout shouldBe s"""${Ansi.bold("Destination: ")}<DST>
                           |  ${Ansi.ok("back202401", bold = true)}: Moving files (4)....
                           |  ${Ansi.ok("back202402", bold = true)}: Moving files (1).
                           |""".stripMargin

        dst.files shouldBe empty
        (dst / "back202401").files should have size 4
        (dst / "back202402").files should have size 1
        src.files should have size 1
      }
    }
  }
}
