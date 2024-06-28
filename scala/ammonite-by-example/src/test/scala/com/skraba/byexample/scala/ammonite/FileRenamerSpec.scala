package com.skraba.byexample.scala.ammonite

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.{Directory, File}

/** Test the file_renamer.sc script. */
class FileRenamerSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File =
    AmmoniteScriptSpecBase.find("/file_renamer.sc")

  describe("Running the file_renamer.sc help") {

    /** Helper to run git_checker.sc help successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def help(args: String*): String = {
      val arguments: Seq[String] = Seq("help") ++ args
      withScript(arguments: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout
      }
    }

    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(s"$BOLD${GREEN}file_renamer.sc$RESET - File operations for general clean-up")
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith("file_renamer.sc - File operations for general clean-up")
    }
  }

  describe("Using file_renamer.sc cameraphone") {

    val backedup = "backedup" + DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())

    /** Creates a simplified scenario with the following items:
      *
      *   - `/tmp/tag/src/DCIM/Camera` for a camera directory
      *     - `image1.jpg` (fake images, containing their name as text)
      *     - `image2.jpg`
      *     - `image3.jpg`
      *   - `/tmp/tag/dst/` An empty directory to use as output.
      *
      * @param tag
      *   a string to use to uniquely identify the scenario
      */
    def scenario(
        tag: String,
        cameraFiles: Seq[String] = Seq("image1.jpg", "image2.jpg", "image3.jpg")
    ): (Directory, Directory) = {
      val src = (Tmp / tag / "src").createDirectory(failIfExists = false)
      val dst = (Tmp / tag / "dst").createDirectory(failIfExists = false)
      val cameraSrc =
        (src / "DCIM" / "Camera").createDirectory(failIfExists = false)
      cameraFiles.foreach(f => (cameraSrc / f).createFile().writeAll(f))
      (src, dst)
    }

    /** Helper to run git_checker.sc help successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def cameraphone(args: String*): String = {
      val arguments: Seq[String] = Seq("cameraphone") ++ args
      withScript(arguments: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout
      }
    }

    it("should move files from the source to the directory with all defaults") {
      // Set up a scenario
      val (src, dst) = scenario("basic")

      // Running the first time should move all of the files
      {
        val stdout = cameraphone("--noVerbose", "--src", src.toString, "--dst", dst.toString)
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
      scenario("basic", Seq("image4.jpg"))

      // Running the second time should create a new default destination
      {
        val stdout = cameraphone("--noVerbose", "--src", src.toString, "--dst", dst.toString)
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

    it("should copy files from the source to a specific destination") {
      // Set up a scenario
      val (src, dst) = scenario("specificDst")

      val stdout = cameraphone("--noVerbose", "--src", src.toString, "--dst", dst.toString, "--dstSub", "Copied")
      stdout shouldBe empty

      (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
      (src / "DCIM" / "Camera" / backedup).toDirectory.files should have size 3

      dst.toDirectory.files shouldBe empty
      (dst / "Copied").toDirectory.files should have size 3
      (dst / "Copied" / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
    }

    it("should backup files at the source to a specific directory") {
      // Set up a scenario
      val (src, dst) = scenario("specificSrc")

      val stdout = cameraphone(
        "--noVerbose",
        "--src",
        src.toString,
        "--srcSub",
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

  describe("Using file_renamer.sc payslip") {

    /** Creates a simplified scenario with the following items:
      *
      *   - `/tmp/tag/src/` as a source directory
      *     - `Bulletins 01_2021.pdf` (fake PDFs, contains their own name)
      *     - `Bulletins 02_2021.pdf`
      *     - `03-2021_bulletin_de_paie.pdf`
      *     - `04-2021_bulletin_de_paie.pdf`
      *     - `202105Payslip.pdf`
      *       - `other.txt`
      *   - `/tmp/tag/dst/` An empty directory to use as output.
      *
      * @param tag
      *   a string to use to uniquely identify the scenario
      */
    def scenario(
        tag: String,
        files: Seq[String] = Seq(
          "Bulletins 01_2021.pdf",
          "Bulletins 02_2021.pdf",
          "03-2021_bulletin_de_paie.pdf",
          "04-2021_bulletin_de_paie.pdf",
          "202105Payslip.pdf",
          "other.txt"
        )
    ): (Directory, Directory) = {
      val src = (Tmp / tag / "src").createDirectory(failIfExists = false)
      val dst = (Tmp / tag / "dst").createDirectory(failIfExists = false)
      files.foreach(f => (src / f).createFile().writeAll(f))
      (src, dst)
    }

    /** Helper to run git_checker.sc help successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def payslip(args: String*): String = {
      val arguments: Seq[String] = Seq("payslip") ++ args
      withScript(arguments: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout.replace(Tmp.toString(), "<TMP>")
      }
    }

    it("should suggest moving payslip files") {
      val (src, dst) = scenario("basic")
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
