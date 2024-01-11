package com.skraba.byexample.scala.ammonite

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
      ansiHelp should startWith(
        s"$BOLD${GREEN}file_renamer.sc$RESET - File operations for general clean-up"
      )
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "file_renamer.sc - File operations for general clean-up"
      )
    }
  }

  describe("Using file_renamer.sc cameraphone") {

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
    def scenario(tag: String): (Directory, Directory) = {
      val src = (Tmp / tag / "src").createDirectory(failIfExists = true)
      val dst = (Tmp / tag / "dst").createDirectory(failIfExists = true)

      val cameraSrc =
        (src / "DCIM" / "Camera").createDirectory(failIfExists = true)

      (cameraSrc / "image1.jpg").createFile().writeAll("image1.jpg")
      (cameraSrc / "image2.jpg").createFile().writeAll("image2.jpg")
      (cameraSrc / "image3.jpg").createFile().writeAll("image3.jpg")

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

      val stdout =
        cameraphone("--noVerbose", "--src", src.toString, "--dst", dst.toString)
      stdout shouldBe empty

      (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
      (src / "DCIM" / "Camera" / "backedup").toDirectory.files should have size 3

      dst.toDirectory.files shouldBe empty
      val dstDirs = dst.toDirectory.dirs.toSeq
      dstDirs should have size 1
      dstDirs.head.files should have size 3
      (dstDirs.head / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
    }

    it("should move files from the source to the directory with a dst tag") {
      // Set up a scenario
      val (src, dst) = scenario("basic2")

      val stdout = cameraphone(
        "--noVerbose",
        "--src",
        src.toString,
        "--dst",
        dst.toString,
        "--dstSub",
        "Copied"
      )
      stdout shouldBe empty

      (src / "DCIM" / "Camera").toDirectory.files shouldBe empty
      (src / "DCIM" / "Camera" / "backedup").toDirectory.files should have size 3

      dst.toDirectory.files shouldBe empty
      val dstDirs = dst.toDirectory.dirs.toSeq
      dstDirs should have size 1
      dstDirs.head.name shouldBe "Copied"

      dstDirs.head.files should have size 3
      (dstDirs.head / "image1.jpg").toFile.slurp() shouldBe "image1.jpg"
    }
  }
}
