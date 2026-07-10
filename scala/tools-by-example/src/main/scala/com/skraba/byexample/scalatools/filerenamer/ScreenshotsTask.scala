package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Backs up screenshots from the connected phone */
object ScreenshotsTask extends Task {
  override val Description: String = "Backs up screenshots from the connected phone"
  override val Cmd: String = "screenshot"
  override val Doc: String = CameraphoneTask.Doc
    .replace(CameraphoneTask.Description, ScreenshotsTask.Description)
    .replace(CameraphoneTask.Cmd, ScreenshotsTask.Cmd)
    .replace("Cameraphone", "Screenshots")
    .replace("(Default: DCIM/Camera)", "(Default: Pictures/Screenshots, DCIM/Screenshots)")

  override def go(opt: Docopt): Unit =
    CameraphoneTask.go(opt, " Screenshots", Seq("Pictures/Screenshots", "DCIM/Screenshots"))
}
