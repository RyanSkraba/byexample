package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, Task}

import java.nio.file.Files
import scala.util.matching.Regex

/** Move image files to directories by month */
object MonthifyTask extends Task {
  override val Description: String = "Move image files to directories by month"
  override val Cmd: String = "monthify"
  override val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${FileRenamerGo.Name} $Cmd [options]
       |
       |Options:
       |  -h --help        Show this screen
       |  --version        Show version
       |  --src=SRC        The source directory to sort files from
       |  --dst=SRC        The destination directory to create YYYYMM subdirectories
       |                   and move files to
       |  --prefix=PREFIX  A prefix for the created subdirectories.
       |  --dryRun         True if no files should actually be copied or moved
       |  --noVerbose      True if the action should be silent (verbose by default)
       |  --plain          Do not show colour output
       |  --yes            Quiet mode, assume "yes" to all Y/N prompts
       |
       |""".stripMargin.trim

  override def go(opt: Docopt): Unit = {

    val srcDir = opt.dir.get("--src")
    val dstDir = opt.dir.getOr("--dst", srcDir)
    val prefix = opt.string.getOr("--prefix", "")
    val dryRun = opt.flag("--dryRun")

    val out = AnsiConsole(verbose = !opt.flag("--noVerbose"), plain = opt.flag("--plain"), yes = opt.flag("--yes"))

    // Use the given src for the device, or try to detect it
    out.vPrintln(out.bold("Destination: ") + dstDir.toString)

    // The regex used to extract dates from the filenames
    val DateExtract: Regex = raw"^(\D+_)?(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2}).*".r

    val files: Map[String, Seq[String]] =
      srcDir.list
        .filter(_.toFile.isFile)
        .map(_.name)
        .map(f => f -> f)
        .collect { case (f, DateExtract(_, yy, mm, _*)) => (yy + mm, f) }
        .groupMap(_._1)(_._2)

    for (yyyymm <- files.keySet.toSeq.sorted) {

      val fToMove = files(yyyymm)
      val yymmDstDir = dstDir / (prefix + yyyymm)
      out.vPrint(s"  ${out.ok(yymmDstDir.name, bold = true)}: Moving files (${fToMove.size})")

      if (!dryRun) yymmDstDir.createDirectory(failIfExists = false)
      fToMove.foreach { fmv =>
        out.vPrint(".")
        if (!dryRun) Files.move(srcDir / fmv, yymmDstDir / fmv)
      }
      out.vPrintln()
    }
  }

}
