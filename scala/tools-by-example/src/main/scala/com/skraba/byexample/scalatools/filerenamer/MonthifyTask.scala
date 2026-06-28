package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.{Docopt, Task}

/** Move image files to directories by month */
object MonthifyTask extends Task {
  override val Description: String = "Move image files to directories by month"
  override val Cmd: String = "monthify"
  override val Doc: String =
    s"""${Description}
       |
       |Usage:
       |  ${FileRenamerGo.Name} $Cmd [options]
       |
       |Options:
       |  -h --help                     Show this screen
       |  --version                     Show version
       |  --dryRun                      True if no files should actually be copied or
       |                                moved
       |  --noVerbose                   True if the action should be silent (verbose
       |                                by default)
       |  --plain                       Do not show colour output
       |  --yes                         Quiet mode, assume "yes" to all Y/N prompts
       |
       |""".stripMargin

  override def go(opt: Docopt): Unit = ???
}
