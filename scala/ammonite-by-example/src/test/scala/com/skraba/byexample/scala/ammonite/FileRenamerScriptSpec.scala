package com.skraba.byexample.scala.ammonite

import com.skraba.byexample.scala.ammonite.FileRenamer.MissingPhoneDirException

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.Directory

/** Test the file_renamer.sc script.
  *
  * Some of the file renaming functions depend on the current date. If this spec is run too close to midnight, it is
  * possible that failures will occur.
  */
class FileRenamerScriptSpec extends AmmoniteScriptSpecBase("/file_renamer.sc") {

  /** The current year and month for testing. */
  val yyyyMm: String = DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())

  /** The current year, month, and day for testing. */
  val yyyyMmDd: String = DateTimeFormatter.ofPattern("yyyyMMdd").format(LocalDate.now())

  def cameraphone(src: Directory, dst: Directory)(args: Any*): String =
    withTaskSuccessSrcDst(src, dst, yyyyMmDd -> "<YYYYMMDD>", yyyyMm -> "<YYYYMM>")("cameraphone")(args: _*)
  def screenshot(src: Directory, dst: Directory)(args: Any*): String =
    withTaskSuccessSrcDst(src, dst, yyyyMmDd -> "<YYYYMMDD>", yyyyMm -> "<YYYYMM>")("screenshot")(args: _*)
  def monthify(src: Directory, dst: Directory)(args: Any*): String =
    withTaskSuccessSrcDst(src, dst)("monthify")(args: _*)
  def payslip(src: Directory, dst: Directory)(args: Any*): String = withTaskSuccessSrcDst(src, dst)("payslip")(args: _*)

  describe(s"Running $ScriptName help") {
    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(s"$BOLD${GREEN}file_renamer.sc$RESET - File operations for general clean-up")
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith("file_renamer.sc - File operations for general clean-up")
    }
  }

  describe(s"Running $ScriptName cameraphone and screenshot") {

    it("should move files from the camera source to the directory with all defaults") {
      // Set up a scenario
      val (src, dst) = createSrcDst("basic")

      // Running the first time tests a dry run
      {
        val stdout = cameraphone(src, dst)("--dryRun", "--deviceRootDir", src, "--dst", dst)
        stdout shouldBe s"""<SRC>
            |There are 3 files in <SRC>/DCIM/Camera.
            |  ${Ansi.bold("jpg")}: 3
            |cp <SRC>/DCIM/Camera/image1.jpg <DST>/<YYYYMMDD> Cameraphone/image1.jpg
            |mv <SRC>/DCIM/Camera/image1.jpg <SRC>/DCIM/Camera/backedup<YYYYMM>/image1.jpg
            |cp <SRC>/DCIM/Camera/image2.jpg <DST>/<YYYYMMDD> Cameraphone/image2.jpg
            |mv <SRC>/DCIM/Camera/image2.jpg <SRC>/DCIM/Camera/backedup<YYYYMM>/image2.jpg
            |cp <SRC>/DCIM/Camera/image3.jpg <DST>/<YYYYMMDD> Cameraphone/image3.jpg
            |mv <SRC>/DCIM/Camera/image3.jpg <SRC>/DCIM/Camera/backedup<YYYYMM>/image3.jpg
            |""".stripMargin

        (src / "DCIM" / "Camera").toDirectory.files should have size 3
        (src / "DCIM" / "Camera" / s"backedup$yyyyMm").toDirectory.files shouldBe empty

        dst.toDirectory.files shouldBe empty
        dst.toDirectory.dirs shouldBe empty
      }

      // Running the first time should move all of the files
      {
        val stdout = cameraphone(src, dst)("--noVerbose", "--deviceRootDir", src, "--dst", dst)
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
        val stdout = cameraphone(src, dst)("--noVerbose", "--deviceRootDir", src, "--dst", dst)
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
        val stdout = screenshot(src, dst)("--plain", "--deviceRootDir", src, "--dst", dst)
        stdout shouldBe """<SRC>
                          |There are 3 files in <SRC>/Pictures/Screenshots.
                          |There are 1 files in <SRC>/DCIM/Screenshots.
                          |  jpg: 4
                          |<DST>/<YYYYMMDD> Screenshots/image1.jpg...
                          |<DST>/<YYYYMMDD> Screenshots/image2.jpg...
                          |<DST>/<YYYYMMDD> Screenshots/image3.jpg...
                          |<DST>/<YYYYMMDD> Screenshots/image4.jpg...
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

      val stdout = cameraphone(src, dst)("--noVerbose", "--deviceRootDir", src, "--dst", dst, "--dstSub", "Copied")
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

      val stdout = cameraphone(src, dst)(
        "--noVerbose",
        "--deviceRootDir",
        src,
        "--deviceBackedupSubDir",
        "BackedUp",
        "--dst",
        dst,
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

    describe("When two phones are connected") {
      it("should copy files from more than one phone when both connected") {
        // Set up a scenario
        val (src, dst) = createSrcDst("multiphone", "gvfs/phone1/Drive/DCIM/Camera", Seq("a1.jpg", "a2.jpg"))
        createSrcDst("multiphone", "gvfs/phone2/Disk/DCIM/Camera", Seq("b1.jpg"))

        val stdout = cameraphone(src, dst)("--plain", "--dryRun", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout shouldBe
          """<SRC>/gvfs/phone1/Drive
          |There are 2 files in <SRC>/DCIM/Camera.
          |  jpg: 2
          |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <DST>/<YYYYMMDD> Cameraphone/a1.jpg
          |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/backedup<YYYYMM>/a1.jpg
          |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <DST>/<YYYYMMDD> Cameraphone/a2.jpg
          |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/backedup<YYYYMM>/a2.jpg
          |<SRC>/gvfs/phone2/Disk
          |There are 1 files in <SRC>/DCIM/Camera.
          |  jpg: 1
          |cp <SRC>/gvfs/phone2/Disk/DCIM/Camera/b1.jpg <DST>/<YYYYMMDD> Cameraphone/b1.jpg
          |mv <SRC>/gvfs/phone2/Disk/DCIM/Camera/b1.jpg <SRC>/gvfs/phone2/Disk/DCIM/Camera/backedup<YYYYMM>/b1.jpg
          |""".stripMargin

        val stdout2 = cameraphone(src, dst)("--plain", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout2 shouldBe
          """<SRC>/gvfs/phone1/Drive
          |There are 2 files in <SRC>/DCIM/Camera.
          |  jpg: 2
          |<DST>/<YYYYMMDD> Cameraphone/a1.jpg...
          |<DST>/<YYYYMMDD> Cameraphone/a2.jpg...
          |<SRC>/gvfs/phone2/Disk
          |There are 1 files in <SRC>/DCIM/Camera.
          |  jpg: 1
          |<DST>/<YYYYMMDD>-2 Cameraphone/b1.jpg...
          |""".stripMargin

        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").toDirectory.files shouldBe empty
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").toDirectory.dirs should have size 1
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").toDirectory.dirs.toSeq.head.files should have size 2
        (src / "gvfs" / "phone2" / "Disk" / "DCIM" / "Camera").toDirectory.files shouldBe empty
        (src / "gvfs" / "phone2" / "Disk" / "DCIM" / "Camera").toDirectory.dirs should have size 1
        (src / "gvfs" / "phone2" / "Disk" / "DCIM" / "Camera").toDirectory.dirs.toSeq.head.files should have size 1
      }

      it("should copy files from one phone if the other is disconnected") {
        // Set up a scenario
        val (src, dst) = createSrcDst("multiphone_one", "gvfs/phone1/Drive/DCIM/Camera", Seq("a1.jpg", "a2.jpg"))
        (Tmp / "multiphone_one" / "src/gvfs/phone2").createDirectory(failIfExists = true)

        val stdout = cameraphone(src, dst)("--plain", "--dryRun", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout shouldBe
          """<SRC>/gvfs/phone1/Drive
          |There are 2 files in <SRC>/DCIM/Camera.
          |  jpg: 2
          |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <DST>/<YYYYMMDD> Cameraphone/a1.jpg
          |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/backedup<YYYYMM>/a1.jpg
          |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <DST>/<YYYYMMDD> Cameraphone/a2.jpg
          |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/backedup<YYYYMM>/a2.jpg
          |""".stripMargin

        val stdout2 = cameraphone(src, dst)("--plain", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout2 shouldBe
          """<SRC>/gvfs/phone1/Drive
          |There are 2 files in <SRC>/DCIM/Camera.
          |  jpg: 2
          |<DST>/<YYYYMMDD> Cameraphone/a1.jpg...
          |<DST>/<YYYYMMDD> Cameraphone/a2.jpg...
          |""".stripMargin

        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").toDirectory.files shouldBe empty
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").toDirectory.dirs should have size 1
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").toDirectory.dirs.toSeq.head.files should have size 2
        (src / "gvfs" / "phone2").toDirectory.files shouldBe empty
        (src / "gvfs" / "phone2").toDirectory.dirs shouldBe empty
      }

      it("should fail files if both phones are disconnected") {
        // Set up a scenario
        val src = (Tmp / "multiphone_none" / "src").toDirectory
        val dst = (Tmp / "multiphone_none" / "dst").toDirectory
        (src / "gvfs/phone1").createDirectory(failIfExists = true)
        (src / "gvfs/phone2").createDirectory(failIfExists = true)
        withScript("cameraphone", "--plain", "--dryRun", "--phoneMountDir", src / "gvfs", "--dst", dst) {
          case (result, stdout, stderr) =>
            result shouldBe false
            stdout shouldBe empty
            stderr should include("MissingPhoneDirException: Unable to find pics storage.")
        }
      }
    }
  }

  describe(s"Running $ScriptName monthify") {

    val (src, dst) = createSrcDst(
      "monthify",
      srcSubDir = "",
      srcSubDirFiles = Seq(
        "other.txt",
        "IMG_20240101_123456.jpg",
        "20240102_123456.jpg",
        "20240103_123456Stuff.jpg",
        "20240104_123456.txt",
        "20240201_123456.txt"
      )
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
        (dst / "back202401").toDirectory.files should have size 4
        (dst / "back202402").toDirectory.files should have size 1
        src.files should have size 1
      }
    }

  }

  describe(s"Running $ScriptName payslip") {

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
      "payslip",
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
      val stdout = payslip(src, dst)("--plain", "--srcPath", src, "--dstPath", dst)
      stdout shouldBe
        """mv "<SRC>/03-2021_bulletin_de_paie.pdf" "<DST>/202103Payslip.pdf"
          |mv "<SRC>/04-2021_bulletin_de_paie.pdf" "<DST>/202104Payslip.pdf"
          |mv "<SRC>/202105Payslip.pdf" "<DST>/052021Payslip.pdf"
          |mv "<SRC>/Bulletins 01_2021.pdf" "<DST>/202101Payslip.pdf"
          |mv "<SRC>/Bulletins 02_2021.pdf" "<DST>/202102Payslip.pdf"
          |""".stripMargin

      dst.files shouldBe empty
    }
  }
}
