package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Backs up pictures from the connected device. */
object CameraphoneTask extends Task {
  override val Description: String = "Backs up pictures from the connected phone"
  override val Cmd: String = "cameraphone"
  override val Doc: String = Description + "\n\n" + DeviceBackup.doc(Cmd, "Cameraphone", "DCIM/Camera", "mp4,jpg")

  override def go(opt: Docopt): Unit = {
    DeviceBackup.backup(opt, " Cameraphone", Seq("DCIM/Camera"))
  }
}
