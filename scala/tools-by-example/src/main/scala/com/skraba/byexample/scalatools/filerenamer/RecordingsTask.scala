package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Backs up pictures from the connected device. */
object RecordingsTask extends Task {
  override val Description: String = "Backs up voice recordings from the connected device"
  override val Cmd: String = "recordings"
  override val Doc: String = Description + "\n\n" + DeviceBackup.doc(Cmd, "Recordings", "EasyVoiceRecorder", "mpa")

  override def go(opt: Docopt): Unit = {
    DeviceBackup.backup(opt, " Recordings", Seq("EasyVoiceRecorder", "Recordings"), Seq("m4a"))
  }
}
