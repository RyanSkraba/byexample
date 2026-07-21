package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Backs up pictures from the connected device. */
object CameraphoneTask extends Task {
  override val Description: String = "Backs up pictures from the connected phone"
  override val Cmd: String = "cameraphone"
  override val Doc: String = DeviceBackup.Doc
    .replace("<DESCRIPTION>", CameraphoneTask.Description)
    .replace("<CMD>", CameraphoneTask.Cmd)
    .replace("<SUFFIX>", "Cameraphone")
    .replace("<DFLTDIR>", "DCIM/Camera")

  override def go(opt: Docopt): Unit = {
    DeviceBackup.backup(opt, " Cameraphone", Seq("DCIM/Camera"))
  }
}
