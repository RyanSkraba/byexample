package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{MultiTaskMain, Task}

/** A driver for running a file operations for general clean-up. */
object FileRenamerGo extends MultiTaskMain {
  override lazy val Name: String = "FileRenamerGo"
  override lazy val Version: String = "0.0.1-SNAPSHOT"
  override lazy val Tasks: Seq[Task] = Seq(CameraphoneTask, ScreenshotsTask, MonthifyTask, GroupTask, PayslipTask)
  override lazy val Doc: String = "File operations for general clean-up.\n\n" + SimpleDoc
}
