package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Backs up screenshots from the connected phone */
object ScreenshotsTask extends Task {
  override val Description: String = "Backs up screenshots from the connected phone"
  override val Cmd: String = "screenshot"
  override val Doc: String = DeviceBackup.Doc
    .replace("<DESCRIPTION>", ScreenshotsTask.Description)
    .replace("<CMD>", ScreenshotsTask.Cmd)
    .replace("<SUFFIX>", "Screenshots")
    .replace("<DFLTDIR>", "(Default: Pictures/Screenshots, DCIM/Screenshots)")

  override def go(opt: Docopt): Unit =
    DeviceBackup.backup(opt, " Screenshots", Seq("Pictures/Screenshots", "DCIM/Screenshots"))
}
