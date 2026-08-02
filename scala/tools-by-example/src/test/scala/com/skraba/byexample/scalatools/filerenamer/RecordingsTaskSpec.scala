package com.skraba.byexample.scalatools.filerenamer

import com.skraba.byexample.scalatools.filerenamer.DeviceBackup.{DefaultBackupDir, Today}
import com.tinfoiled.docopt4s.AnsiConsole
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, WithFileTests, WithTmpSrcDst}

import java.nio.file.Path

/** Unit tests for [[RecordingsTask]]. */
class RecordingsTaskSpec
    extends MultiTaskMainSpec(FileRenamerGo, Some(RecordingsTask))
    with WithFileTests
    with WithTmpSrcDst {

  /** Used for colour codes. */
  val Ansi = AnsiConsole()

  /** Run the cameraphone task with the source and destination directories. */
  def recorder(src: Path, dst: Path)(args: Any*): String =
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
      "Recordings/recorder1.m4a",
      "Recordings/image2.jpg"
    )

  describe(s"Running $MainName $TaskCmd") {

    it("should move files from the device source to the directory with all defaults") {

      // Set up a scenario
      val (src, dst) = createSrcDst("basic", BasicFiles: _*)

      // Test a dry run
      {
        val stdout = recorder(src, dst)("--dryRun", "--plain", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe
          """<SRC>
            |Source directory not found <SRC>/EasyVoiceRecorder
            |There are 2 files in <SRC>/Recordings.
            |  jpg: 1
            |  m4a: 1
            |cp <SRC>/Recordings/recorder1.m4a <DST>/<TODAY> Recordings/recorder1.m4a
            |mv <SRC>/Recordings/recorder1.m4a <SRC>/Recordings/<DEFAULTBACKUP>/recorder1.m4a
            |""".stripMargin

        (src / "Recordings").list should have size 2
        (src / "Recordings" / "recorder1.m4a").toFile should exist
        (src / "Recordings" / "image2.jpg").toFile should exist
        dst.list shouldBe empty
      }

      // Running the first time should move all of the files
      {
        val stdout = recorder(src, dst)("--plain", "--deviceRootDir" -> src, "--dst" -> dst)
        stdout shouldBe
          """<SRC>
            |Source directory not found <SRC>/EasyVoiceRecorder
            |There are 2 files in <SRC>/Recordings.
            |  jpg: 1
            |  m4a: 1
            |<DST>/<TODAY> Recordings/recorder1.m4a...
            |""".stripMargin

        (src / "Recordings").files should have size 1
        (src / "Recordings" / "image2.jpg").toFile should exist
        (src / "Recordings").dirs should have size 1
        (src / "Recordings" / DefaultBackupDir / "recorder1.m4a").toFile should exist

        dst.list should have size 1
        (dst / s"$Today Recordings").list should have size 1
        (dst / s"$Today Recordings" / "recorder1.m4a").toFile should exist
      }
    }
  }
}
