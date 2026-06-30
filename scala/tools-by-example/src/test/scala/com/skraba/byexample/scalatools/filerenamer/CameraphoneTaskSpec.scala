package com.skraba.byexample.scalatools.filerenamer

import com.skraba.byexample.scalatools.filerenamer.CameraphoneTask.{DefaultBackupDir, MissingPhoneDirException, Today}
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests}

import java.nio.file.Path

/** Unit tests for [[CameraphoneTask]]. */
class CameraphoneTaskSpec
    extends MultiTaskMainSpec(FileRenamerGo, Some(CameraphoneTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the CameraphoneTask with the source and destination directories. */
  def cameraphone(src: Path, dst: Path)(args: Any*): String =
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
    Seq("DCIM/Camera/image1.jpg", "DCIM/Camera/image2.jpg", "DCIM/Camera/movie.mp4", "DCIM/Camera/list.txt")

  describe(s"Running $MainName $TaskCmd") {

    it("should move files from the camera source to the directory with all defaults") {
      // Set up a scenario
      val (src, dst) = createSrcDst("basic", BasicFiles: _*)

      // Test a dry run
      {
        val stdout = cameraphone(src, dst)("--dryRun", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe s"""<SRC>
                           |There are 4 files in <SRC>/DCIM/Camera.
                           |  ${Ansi.bold("jpg")}: 2
                           |  ${Ansi.bold("mp4")}: 1
                           |  ${Ansi.bold("txt")}: 1
                           |cp <SRC>/DCIM/Camera/image1.jpg <DST>/<TODAY> Cameraphone/image1.jpg
                           |mv <SRC>/DCIM/Camera/image1.jpg <SRC>/DCIM/Camera/<DEFAULTBACKUP>/image1.jpg
                           |cp <SRC>/DCIM/Camera/image2.jpg <DST>/<TODAY> Cameraphone/image2.jpg
                           |mv <SRC>/DCIM/Camera/image2.jpg <SRC>/DCIM/Camera/<DEFAULTBACKUP>/image2.jpg
                           |cp <SRC>/DCIM/Camera/movie.mp4 <DST>/<TODAY> Cameraphone/movie.mp4
                           |mv <SRC>/DCIM/Camera/movie.mp4 <SRC>/DCIM/Camera/<DEFAULTBACKUP>/movie.mp4
                           |""".stripMargin

        (src / "DCIM" / "Camera").files should have size 4
        (src / "DCIM" / "Camera").dirs shouldBe empty // No backup created
        (src / "DCIM" / "Camera" / DefaultBackupDir).toFile shouldNot exist
        dst.list shouldBe empty
      }

      // Test a dry run with more options
      {
        val stdout = cameraphone(src, dst)(
          "--dryRun",
          "--deviceRootDir" -> src,
          "--dst" -> dst,
          "--deviceBackedupSubDir" -> "Backup",
          "--extension" -> "mp4",
          "--extension" -> "txt",
          "--dstSuffix" -> "Copied"
        )
        stdout shouldBe s"""<SRC>
                           |There are 4 files in <SRC>/DCIM/Camera.
                           |  ${Ansi.bold("jpg")}: 2
                           |  ${Ansi.bold("mp4")}: 1
                           |  ${Ansi.bold("txt")}: 1
                           |cp <SRC>/DCIM/Camera/list.txt <DST>/<TODAY>Copied/list.txt
                           |mv <SRC>/DCIM/Camera/list.txt <SRC>/DCIM/Camera/Backup/list.txt
                           |cp <SRC>/DCIM/Camera/movie.mp4 <DST>/<TODAY>Copied/movie.mp4
                           |mv <SRC>/DCIM/Camera/movie.mp4 <SRC>/DCIM/Camera/Backup/movie.mp4
                           |""".stripMargin

        (src / "DCIM" / "Camera").files should have size 4
        (src / "DCIM" / "Camera").dirs shouldBe empty // No backup created
        (src / "DCIM" / "Camera" / DefaultBackupDir).toFile shouldNot exist
        dst.list shouldBe empty
      }

      // Another dry run without the date in the destination subdirectory
      {
        val stdout = cameraphone(src, dst)(
          "--dryRun",
          "--deviceRootDir" -> src,
          "--dst" -> dst,
          "--deviceBackedupSubDir" -> "Backup",
          "--extension" -> "mp4",
          "--dstSub" -> "Copied"
        )
        stdout shouldBe s"""<SRC>
                           |There are 4 files in <SRC>/DCIM/Camera.
                           |  ${Ansi.bold("jpg")}: 2
                           |  ${Ansi.bold("mp4")}: 1
                           |  ${Ansi.bold("txt")}: 1
                           |cp <SRC>/DCIM/Camera/movie.mp4 <DST>/Copied/movie.mp4
                           |mv <SRC>/DCIM/Camera/movie.mp4 <SRC>/DCIM/Camera/Backup/movie.mp4
                           |""".stripMargin

        (src / "DCIM" / "Camera").files should have size 4
        (src / "DCIM" / "Camera").dirs shouldBe empty // No backup created
        (src / "DCIM" / "Camera" / DefaultBackupDir).toFile shouldNot exist
        dst.list shouldBe empty
      }

      // Running the first time should move all of the files
      {
        val stdout = cameraphone(src, dst)("--noVerbose", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe empty

        (src / "DCIM" / "Camera").files shouldBe Seq(src / "DCIM" / "Camera" / "list.txt")
        (src / "DCIM" / "Camera").dirs.filter(_.startsWith("backedup")) shouldBe empty
        (src / "DCIM" / "Camera" / DefaultBackupDir).files should have size 3

        dst.files shouldBe empty
        val dstDirs = dst.dirs
        dstDirs shouldBe Seq(dst / s"$Today Cameraphone")
        dstDirs.head.files should have size 3
        (dstDirs.head / "image1.jpg").slurp() shouldBe "image1.jpg"
      }

      // Add another image to the camera
      createSrcDst("basic", "DCIM/Camera/image4.jpg")

      // Running the second time should create a new default destination
      {
        val stdout = cameraphone(src, dst)("--noVerbose", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe empty

        (src / "DCIM" / "Camera").files shouldBe Seq(src / "DCIM" / "Camera" / "list.txt")
        (src / "DCIM" / "Camera" / DefaultBackupDir).files should have size 4

        dst.files shouldBe empty
        val dstDirs = dst.dirs
        dstDirs should contain(dst / s"$Today Cameraphone")
        dstDirs should contain(dst / s"$Today-2 Cameraphone")

        (dst / s"$Today-2 Cameraphone" / "image4.jpg").toFile should exist
        (dst / s"$Today-2 Cameraphone" / "image4.jpg").slurp() shouldBe "image4.jpg"
      }
    }

    describe("When two phones are connected") {
      it("should copy files from more than one phone when both connected") {
        // Set up a scenario
        val (src, dst) = createSrcDst(
          "multiphone",
          "gvfs/phone1/Drive/DCIM/Camera/a1.jpg",
          "gvfs/phone1/Drive/DCIM/Camera/a2.jpg",
          "gvfs/phone2/Disk/DCIM/Camera/b1.jpg"
        )

        val stdout = cameraphone(src, dst)("--plain", "--dryRun", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout shouldBe
          """<SRC>/gvfs/phone1/Drive
            |There are 2 files in <SRC>/DCIM/Camera.
            |  jpg: 2
            |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <DST>/<TODAY> Cameraphone/a1.jpg
            |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/<DEFAULTBACKUP>/a1.jpg
            |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <DST>/<TODAY> Cameraphone/a2.jpg
            |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/<DEFAULTBACKUP>/a2.jpg
            |<SRC>/gvfs/phone2/Disk
            |There are 1 files in <SRC>/DCIM/Camera.
            |  jpg: 1
            |cp <SRC>/gvfs/phone2/Disk/DCIM/Camera/b1.jpg <DST>/<TODAY> Cameraphone/b1.jpg
            |mv <SRC>/gvfs/phone2/Disk/DCIM/Camera/b1.jpg <SRC>/gvfs/phone2/Disk/DCIM/Camera/<DEFAULTBACKUP>/b1.jpg
            |""".stripMargin

        val stdout2 = cameraphone(src, dst)("--plain", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout2 shouldBe
          """<SRC>/gvfs/phone1/Drive
            |There are 2 files in <SRC>/DCIM/Camera.
            |  jpg: 2
            |<DST>/<TODAY> Cameraphone/a1.jpg...
            |<DST>/<TODAY> Cameraphone/a2.jpg...
            |<SRC>/gvfs/phone2/Disk
            |There are 1 files in <SRC>/DCIM/Camera.
            |  jpg: 1
            |<DST>/<TODAY>-2 Cameraphone/b1.jpg...
            |""".stripMargin

        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").files shouldBe empty
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").dirs should have size 1
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").dirs.head.files should have size 2
        (src / "gvfs" / "phone2" / "Disk" / "DCIM" / "Camera").files shouldBe empty
        (src / "gvfs" / "phone2" / "Disk" / "DCIM" / "Camera").dirs should have size 1
        (src / "gvfs" / "phone2" / "Disk" / "DCIM" / "Camera").dirs.toSeq.head.files should have size 1
      }

      it("should copy files from one phone if the other is disconnected") {
        // Set up a scenario
        val (src, dst) =
          createSrcDst("multiphone_one", "gvfs/phone1/Drive/DCIM/Camera/a1.jpg", "gvfs/phone1/Drive/DCIM/Camera/a2.jpg")
        (Tmp / "multiphone_one" / "src/gvfs/phone2").createDirectory(failIfExists = true)

        val stdout = cameraphone(src, dst)("--plain", "--dryRun", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout shouldBe
          """<SRC>/gvfs/phone1/Drive
            |There are 2 files in <SRC>/DCIM/Camera.
            |  jpg: 2
            |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <DST>/<TODAY> Cameraphone/a1.jpg
            |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a1.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/<DEFAULTBACKUP>/a1.jpg
            |cp <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <DST>/<TODAY> Cameraphone/a2.jpg
            |mv <SRC>/gvfs/phone1/Drive/DCIM/Camera/a2.jpg <SRC>/gvfs/phone1/Drive/DCIM/Camera/<DEFAULTBACKUP>/a2.jpg
            |""".stripMargin

        val stdout2 = cameraphone(src, dst)("--plain", "--phoneMountDir", src / "gvfs", "--dst", dst)
        stdout2 shouldBe
          """<SRC>/gvfs/phone1/Drive
            |There are 2 files in <SRC>/DCIM/Camera.
            |  jpg: 2
            |<DST>/<TODAY> Cameraphone/a1.jpg...
            |<DST>/<TODAY> Cameraphone/a2.jpg...
            |""".stripMargin

        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").files shouldBe empty
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").dirs should have size 1
        (src / "gvfs" / "phone1" / "Drive" / "DCIM" / "Camera").dirs.toSeq.head.files should have size 2
        (src / "gvfs" / "phone2").files shouldBe empty
        (src / "gvfs" / "phone2").dirs shouldBe empty
      }
    }
  }

  describe(s"Running $MainName $TaskCmd with missing phones") {

    // Set up a scenario with two directories and no files.
    val (src, dst) = createSrcDst("multiphone_none", "gvfs/phone1/", "gvfs/phone2/")

    it("should fail files when no phone is found") {
      interceptGo[MissingPhoneDirException](
        TaskCmd,
        "--plain",
        "--dryRun",
        "--phoneMountDir" -> src / "gvfs",
        "--dst" -> dst
      ).getMessage shouldBe s"Unable to find phone storage (${src / "gvfs"} phone1)."
    }

    for (tag <- Seq("phone1", "phone2", "phone3"))
      it(s"should fail files when no phone is found: $tag") {
        interceptGo[MissingPhoneDirException](
          TaskCmd,
          "--plain",
          "--dryRun",
          "--phoneMountDir" -> src / "gvfs",
          "--phoneTag" -> tag,
          "--dst" -> dst
        ).getMessage shouldBe s"Unable to find phone storage (${src / "gvfs"} $tag)."
      }

    it(s"should fail files when the --phoneMountDir exists but with no contents") {
      interceptGo[MissingPhoneDirException](
        TaskCmd,
        "--plain",
        "--dryRun",
        "--phoneMountDir" -> src / "gvfs" / "phone1",
        "--dst" -> dst
      ).getMessage shouldBe s"Unable to find phone storage (${src / "gvfs" / "phone1"})."
    }
  }
}
