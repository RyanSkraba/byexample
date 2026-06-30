package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}

import java.nio.file.Path

/** Unit tests for [[PayslipTask]]. */
class PayslipTaskSpec
    extends MultiTaskMainSpec(FileRenamerGo, Some(PayslipTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the PayslipTask with the source and destination directories. */
  def payslip(src: Path, dst: Path)(args: Any*): String = withGoStdoutSrcDst(src, dst)(TaskCmd +: args: _*)

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnMissingOptValue("--src")
    itShouldThrowOnMissingOptValue("--dst")

    itShouldBeAnExistingDir()("--src", "<>")
    itShouldBeAnExistingDir()("--src", ExistingFile.getParent, "--dst", "<>")
  }

  describe(s"Running $MainName $TaskCmd") {
    val (src, dst) = createSrcDst(
      "payslip",
      "Bulletins 01_2021.pdf",
      "Bulletins 02_2021.pdf",
      "03-2021_bulletin_de_paie.pdf",
      "04-2021_bulletin_de_paie.pdf",
      "202105Payslip.pdf",
      "other.txt"
    )

    it("should move files") {
      // Running the first time tests a dry run
      {
        val stdout = payslip(src, dst)("--dryRun", "--plain", "--src", src, "--dst", dst)
        stdout shouldBe
          """Destination: <DST>
          |# Not a match <SRC>/other.txt
          |mv "<SRC>/03-2021_bulletin_de_paie.pdf" "<DST>/202103Payslip.pdf"
          |mv "<SRC>/04-2021_bulletin_de_paie.pdf" "<DST>/202104Payslip.pdf"
          |mv "<SRC>/202105Payslip.pdf" "<DST>/052021Payslip.pdf"
          |mv "<SRC>/Bulletins 01_2021.pdf" "<DST>/202101Payslip.pdf"
          |mv "<SRC>/Bulletins 02_2021.pdf" "<DST>/202102Payslip.pdf"
          |""".stripMargin

        dst.files shouldBe empty
        dst.dirs shouldBe empty
        src.files should have size 6
        src.dirs shouldBe empty
      }

      // Running the first time should move all of the files
      {
        val stdout = payslip(src, dst)("--plain", "--src", src, "--dst", dst)
        stdout shouldBe
          """Destination: <DST>
            |# Not a match <SRC>/other.txt
            |""".stripMargin

        dst.files should have size 5
        dst.dirs shouldBe empty
        src.files should have size 1
        src.dirs shouldBe empty
      }
    }
  }
}
