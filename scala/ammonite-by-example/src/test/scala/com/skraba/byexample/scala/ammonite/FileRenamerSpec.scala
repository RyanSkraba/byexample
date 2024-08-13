package com.skraba.byexample.scala.ammonite

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.{Directory, File}

/** Test the file_renamer.sc script. */
class FileRenamerSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File = AmmoniteScriptSpecBase.find("/file_renamer.sc")

  def help(args: String*): String = withTaskSuccess()("help")(args: _*)
  def cameraphone(args: String*): String = withTaskSuccess()("cameraphone")(args: _*)
  def screenshot(args: String*): String = withTaskSuccess()("screenshot")(args: _*)
  def payslip(args: String*): String = withTaskSuccess()("payslip")(args: _*)

  describe(s"Running the $ScriptName help") {
    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(s"$BOLD${GREEN}file_renamer.sc$RESET - File operations for general clean-up")
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith("file_renamer.sc - File operations for general clean-up")
    }
  }

  describe(s"Using $ScriptName cameraphone and screenshot") {

    val backedup = "backedup" + DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())

    it("should move files from the camera source to the directory with all defaults") {
      // Set up a scenario
      val (src, dst) = createSrcDst("basic")

      // Running the first time should move all of the files
      {
        val stdout = cameraphone("--noVerbose", "--deviceRootDir", src.toString, "--dst", dst.toString)
        stdout shouldBe empty

        (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
        (src / "DCIM" / "Camera" / backedup).toDirectory.files should have size 3

        dst.toDirectory.files shouldBe empty
        val dstDirs = dst.toDirectory.dirs.toSeq
        dstDirs should have size 1
        dstDirs.head.files should have size 3
        (dstDirs.head / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
      }

      // Add another image to the camera
      createSrcDst("basic", srcSubDirFiles = Seq("image4.jpg"))

      // Running the second time should create a new default destination
      {
        val stdout = cameraphone("--noVerbose", "--deviceRootDir", src.toString, "--dst", dst.toString)
        stdout shouldBe empty

        (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
        (src / "DCIM" / "Camera" / backedup).toDirectory.files should have size 4

        dst.toDirectory.files shouldBe empty
        val dstDirs = dst.toDirectory.dirs.toSeq
        dstDirs should have size 2

        val newDst = dstDirs.find(_.name.endsWith("-2 Cameraphone")).get
        newDst.files should have size 1
        (newDst / "image4.jpg").toFile.slurp() shouldBe "image4.jpg"
      }
    }

    it("should move files from the screenshot source to the directory with all defaults") {
      // Set up a scenario
      val (src, dst) = {
        createSrcDst("shots", "Pictures/Screenshots")
        createSrcDst("shots", "DCIM/Screenshots", Seq("image4.jpg"))
      }

      // Running the first time should move all of the files
      {
        val stdout = screenshot("--noVerbose", "--deviceRootDir", src.toString, "--dst", dst.toString)
        stdout shouldBe empty

        (src / "Pictures" / "Screenshots").toDirectory.files shouldBe empty
        (src / "Pictures" / "Screenshots" / backedup).toDirectory.files should have size 3
        (src / "DCIM" / "Screenshots").toDirectory.files shouldBe empty
        (src / "DCIM" / "Screenshots" / backedup).toDirectory.files should have size 1

        dst.toDirectory.files shouldBe empty
        val dstDirs = dst.toDirectory.dirs.toSeq
        dstDirs should have size 1
        dstDirs.head.files should have size 4
        (dstDirs.head / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
      }
    }

    it("should copy files from the source to a specific destination") {
      // Set up a scenario
      val (src, dst) = createSrcDst("specificDst")

      val stdout =
        cameraphone("--noVerbose", "--deviceRootDir", src.toString, "--dst", dst.toString, "--dstSub", "Copied")
      stdout shouldBe empty

      (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
      (src / "DCIM" / "Camera" / backedup).toDirectory.files should have size 3

      dst.toDirectory.files shouldBe empty
      (dst / "Copied").toDirectory.files should have size 3
      (dst / "Copied" / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
    }

    it("should backup files at the source to a specific directory") {
      // Set up a scenario
      val (src, dst) = createSrcDst("specificSrc")

      val stdout = cameraphone(
        "--noVerbose",
        "--deviceRootDir",
        src.toString,
        "--deviceBackedupSubDir",
        "BackedUp",
        "--dst",
        dst.toString,
        "--dstSub",
        "Copied"
      )
      stdout shouldBe empty

      (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
      (src / "DCIM" / "Camera" / "BackedUp").toDirectory.files should have size 3

      dst.toDirectory.files shouldBe empty
      (dst / "Copied").toDirectory.files should have size 3
      (dst / "Copied" / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
    }
  }

  describe(s"Using $ScriptName payslip") {

    /** Creates a simplified scenario with the following items:
      *   - `/tmp/tag/src/` as a source directory
      *     - `Bulletins 01_2021.pdf` (fake PDFs, contains their own name)
      *     - `Bulletins 02_2021.pdf`
      *     - `03-2021_bulletin_de_paie.pdf`
      *     - `04-2021_bulletin_de_paie.pdf`
      *     - `202105Payslip.pdf`
      *     - `other.txt`
      *   - `/tmp/tag/dst/` An empty directory to use as output.
      */
    val (src, dst) = createSrcDst(
      "basic",
      srcSubDir = "",
      srcSubDirFiles = Seq(
        "Bulletins 01_2021.pdf",
        "Bulletins 02_2021.pdf",
        "03-2021_bulletin_de_paie.pdf",
        "04-2021_bulletin_de_paie.pdf",
        "202105Payslip.pdf",
        "other.txt"
      )
    )

    it("should suggest moving payslip files") {
      val stdout = payslip("--plain", "--srcPath", src.toString, "--dstPath", dst.toString)
      stdout shouldBe
        """mv "<TMP>/basic/src/03-2021_bulletin_de_paie.pdf" "<TMP>/basic/dst/202103Payslip.pdf"
          |mv "<TMP>/basic/src/04-2021_bulletin_de_paie.pdf" "<TMP>/basic/dst/202104Payslip.pdf"
          |mv "<TMP>/basic/src/202105Payslip.pdf" "<TMP>/basic/dst/052021Payslip.pdf"
          |mv "<TMP>/basic/src/Bulletins 01_2021.pdf" "<TMP>/basic/dst/202101Payslip.pdf"
          |mv "<TMP>/basic/src/Bulletins 02_2021.pdf" "<TMP>/basic/dst/202102Payslip.pdf"
          |""".stripMargin

      dst.files shouldBe empty
    }
  }
}
