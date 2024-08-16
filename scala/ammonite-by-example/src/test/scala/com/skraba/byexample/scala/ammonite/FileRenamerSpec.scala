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

  val yyyyMm = DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())
  val yyyyMmDd = DateTimeFormatter.ofPattern("yyyyMMdd").format(LocalDate.now())

  def help(args: String*): String = withTaskSuccess()("help")(args: _*)
  def cameraphone(args: String*): String =
    withTaskSuccess(yyyyMmDd -> "<YYYYMMDD>", yyyyMm -> "<YYYYMM>")("cameraphone")(args: _*)
  def screenshot(args: String*): String =
    withTaskSuccess(yyyyMmDd -> "<YYYYMMDD>", yyyyMm -> "<YYYYMM>")("screenshot")(args: _*)
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

    it("should move files from the camera source to the directory with all defaults") {
      // Set up a scenario
      val (src, dst) = createSrcDst("basic")

      // Running the first time tests a dry run
      {
        val stdout = cameraphone("--dryRun", "--deviceRootDir", src.toString, "--dst", dst.toString)
        stdout shouldBe s"""<TMP>/basic/src
            |There are 3 files in <SRC>/DCIM/Camera.
            |  ${Ansi.bold("jpg")}: 3
            |cp <TMP>/basic/src/DCIM/Camera/image1.jpg <TMP>/basic/dst/<YYYYMMDD> Cameraphone/image1.jpg
            |mv <TMP>/basic/src/DCIM/Camera/image1.jpg <TMP>/basic/src/DCIM/Camera/backedup<YYYYMM>/image1.jpg
            |cp <TMP>/basic/src/DCIM/Camera/image2.jpg <TMP>/basic/dst/<YYYYMMDD> Cameraphone/image2.jpg
            |mv <TMP>/basic/src/DCIM/Camera/image2.jpg <TMP>/basic/src/DCIM/Camera/backedup<YYYYMM>/image2.jpg
            |cp <TMP>/basic/src/DCIM/Camera/image3.jpg <TMP>/basic/dst/<YYYYMMDD> Cameraphone/image3.jpg
            |mv <TMP>/basic/src/DCIM/Camera/image3.jpg <TMP>/basic/src/DCIM/Camera/backedup<YYYYMM>/image3.jpg
            |""".stripMargin

        (src / "DCIM" / "Camera").toDirectory.files should have size 3
        (src / "DCIM" / "Camera" / s"backedup$yyyyMm").toDirectory.files shouldBe empty

        dst.toDirectory.files shouldBe empty
        dst.toDirectory.dirs shouldBe empty
      }

      // Running the first time should move all of the files
      {
        val stdout = cameraphone("--noVerbose", "--deviceRootDir", src.toString, "--dst", dst.toString)
        stdout shouldBe empty

        (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
        (src / "DCIM" / "Camera" / s"backedup$yyyyMm").toDirectory.files should have size 3

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
        (src / "DCIM" / "Camera" / s"backedup$yyyyMm").toDirectory.files should have size 4

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
        val stdout = screenshot("--plain", "--deviceRootDir", src.toString, "--dst", dst.toString)
        stdout shouldBe """<TMP>/shots/src
                          |There are 3 files in <SRC>/Pictures/Screenshots.
                          |There are 1 files in <SRC>/DCIM/Screenshots.
                          |  jpg: 4
                          |<TMP>/shots/dst/<YYYYMMDD> Screenshots/image1.jpg...
                          |<TMP>/shots/dst/<YYYYMMDD> Screenshots/image2.jpg...
                          |<TMP>/shots/dst/<YYYYMMDD> Screenshots/image3.jpg...
                          |<TMP>/shots/dst/<YYYYMMDD> Screenshots/image4.jpg...
                          |""".stripMargin

        (src / "Pictures" / "Screenshots").toDirectory.files shouldBe empty
        (src / "Pictures" / "Screenshots" / s"backedup$yyyyMm").toDirectory.files should have size 3
        (src / "DCIM" / "Screenshots").toDirectory.files shouldBe empty
        (src / "DCIM" / "Screenshots" / s"backedup$yyyyMm").toDirectory.files should have size 1

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
      (src / "DCIM" / "Camera" / s"backedup$yyyyMm").toDirectory.files should have size 3

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
