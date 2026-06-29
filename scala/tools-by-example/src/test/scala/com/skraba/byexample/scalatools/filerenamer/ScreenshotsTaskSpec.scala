package com.skraba.byexample.scalatools.filerenamer

import com.skraba.byexample.scalatools.filerenamer.CameraphoneTask.{DefaultBackupDir, Today}
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}

import java.nio.file.Path

/** Unit tests for [[ScreenshotsTask]]. */
class ScreenshotsTaskSpec
    extends MultiTaskMainSpec(FileRenamerGo, Some(ScreenshotsTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the cameraphone task with the source and destination directories. */
  def screenshots(src: Path, dst: Path)(args: Any*): String =
    withGoStdoutSrcDst(src, dst, DefaultBackupDir -> "<DEFAULTBACKUP>", Today -> "<TODAY>")(TaskCmd +: args: _*)

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnMissingOptValue("--deviceRootDir")
    itShouldThrowOnMissingOptValue("--deviceBackedupSubDir")
    itShouldThrowOnMissingOptValue("--deviceRelPath")
    itShouldThrowOnMissingOptValue("--deviceRelPath", "abc", "--deviceRelPath")
    itShouldThrowOnMissingOptValue("--extension", "gif", "--extension")
    itShouldThrowOnMissingOptValue("--dst")
    itShouldThrowOnMissingOptValue("--dstSub")
    itShouldThrowOnMissingOptValue("--dstSuffix")
    itShouldThrowOnMissingOptValue("--phoneMountDir")
    itShouldThrowOnMissingOptValue("--phoneTag")

    itShouldBeAnExistingDir()("--deviceRootDir", "<>")
    itShouldBeAnExistingDir()("--phoneMountDir", "<>")
  }

  val BasicFiles: Seq[String] =
    Seq(
      "Pictures/Screenshots/image1.jpg",
      "Pictures/Screenshots/image2.jpg",
      "Pictures/Screenshots/movie.mp4",
      "Pictures/Screenshots/list.txt",
      "DCIM/Screenshots/image4.jpg"
    )

  describe(s"Running $MainName $TaskCmd") {

    it("should move files from the camera source to the directory with all defaults") {

      // Set up a scenario
      val (src, dst) = createSrcDst("basic", BasicFiles: _*)

      // Test a dry run
      {
        val stdout = screenshots(src, dst)("--dryRun", "--plain", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe
          """<SRC>
            |There are 4 files in <SRC>/Pictures/Screenshots.
            |There are 1 files in <SRC>/DCIM/Screenshots.
            |  jpg: 3
            |  mp4: 1
            |  txt: 1
            |cp <SRC>/DCIM/Screenshots/image4.jpg <DST>/<TODAY> Screenshots/image4.jpg
            |mv <SRC>/DCIM/Screenshots/image4.jpg <SRC>/DCIM/Screenshots/<DEFAULTBACKUP>/image4.jpg
            |cp <SRC>/Pictures/Screenshots/image1.jpg <DST>/<TODAY> Screenshots/image1.jpg
            |mv <SRC>/Pictures/Screenshots/image1.jpg <SRC>/Pictures/Screenshots/<DEFAULTBACKUP>/image1.jpg
            |cp <SRC>/Pictures/Screenshots/image2.jpg <DST>/<TODAY> Screenshots/image2.jpg
            |mv <SRC>/Pictures/Screenshots/image2.jpg <SRC>/Pictures/Screenshots/<DEFAULTBACKUP>/image2.jpg
            |cp <SRC>/Pictures/Screenshots/movie.mp4 <DST>/<TODAY> Screenshots/movie.mp4
            |mv <SRC>/Pictures/Screenshots/movie.mp4 <SRC>/Pictures/Screenshots/<DEFAULTBACKUP>/movie.mp4
            |""".stripMargin

        (src / "DCIM" / "Screenshots").list shouldBe Seq(src / "DCIM" / "Screenshots" / "image4.jpg")
        (src / "Pictures" / "Screenshots").files should have size 4
        (src / "Pictures" / "Screenshots").dirs shouldBe empty
        dst.list shouldBe empty
      }

      // Running the first time should move all of the files
      {
        val stdout = screenshots(src, dst)("--plain", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe
          """<SRC>
            |There are 4 files in <SRC>/Pictures/Screenshots.
            |There are 1 files in <SRC>/DCIM/Screenshots.
            |  jpg: 3
            |  mp4: 1
            |  txt: 1
            |<DST>/<TODAY> Screenshots/image4.jpg...
            |<DST>/<TODAY> Screenshots/image1.jpg...
            |<DST>/<TODAY> Screenshots/image2.jpg...
            |<DST>/<TODAY> Screenshots/movie.mp4...
            |""".stripMargin

        (src / "DCIM" / "Screenshots").files shouldBe empty
        (src / "DCIM" / "Screenshots").dirs shouldBe Seq(
          src / "DCIM" / "Screenshots" / CameraphoneTask.DefaultBackupDir
        )
        (src / "DCIM" / "Screenshots" / CameraphoneTask.DefaultBackupDir / "image4.jpg").toFile should exist

        (src / "Pictures" / "Screenshots").files shouldBe Seq(src / "Pictures" / "Screenshots" / "list.txt")
        (src / "Pictures" / "Screenshots").dirs should have size 1
        (src / "Pictures" / "Screenshots" / CameraphoneTask.DefaultBackupDir / "image1.jpg").toFile should exist

        dst.files shouldBe empty
        dst.dirs should have size 1
        (dst / s"$Today Screenshots").toFile should exist
        (dst / s"$Today Screenshots").files should have size 4
        (dst / s"$Today Screenshots").dirs shouldBe empty
        (dst / s"$Today Screenshots" / "image1.jpg").toFile should exist
        (dst / s"$Today Screenshots" / "image4.jpg").toFile should exist

      }

    }
  }
}
