package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Backs up screenshots from the connected phone */
object ScreenshotsTask extends Task {
  override val Description: String = "Backs up screenshots from the connected phone"
  override val Cmd: String = "screenshot"
  override val Doc: String =
    Description + "\n\n" + DeviceBackup.doc(Cmd, "Screenshots", "Pictures/Screenshots, DCIM/Screenshots", "mp4,jpg")

  override def go(opt: Docopt): Unit =
    DeviceBackup.backup(opt, " Screenshots", Seq("Pictures/Screenshots", "DCIM/Screenshots"))
}
