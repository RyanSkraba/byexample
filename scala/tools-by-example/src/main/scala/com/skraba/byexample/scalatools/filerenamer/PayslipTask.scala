package com.skraba.byexample.scalatools.filerenamer

import com.skraba.byexample.scalatools.filerenamer.MonthifyTask.{Cmd, Description}
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, Task}

import java.nio.file.{Files, Path}
import scala.util.matching.Regex

/** Rename payslip files */
object PayslipTask extends Task {
  override val Description: String = "Rename payslip files"
  override val Cmd: String = "payslip"
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
       |  --dryRun         True if no files should actually be copied or moved
       |  --noVerbose      True if the action should be silent (verbose by default)
       |  --plain          Do not show colour output
       |  --yes            Quiet mode, assume "yes" to all Y/N prompts
       |
       |""".stripMargin.trim

  override def go(opt: Docopt): Unit = {

    val srcDir = opt.dir.get("--src")
    val dstDir = opt.dir.getOr("--dst", srcDir)
    val dryRun = opt.flag("--dryRun")

    val out = AnsiConsole(verbose = !opt.flag("--noVerbose"), plain = opt.flag("--plain"), yes = opt.flag("--yes"))

    // Use the given src for the device, or try to detect it
    out.vPrintln(out.bold("Destination: ") + dstDir.toString)

    // The two file names that are commonly sent as payslip files
    val fileRe1: Regex = raw"Bulletins (\d\d)_(\d\d\d\d).pdf".r
    val fileRe2: Regex = raw"(\d\d)-(\d\d\d\d)_bulletin_de_paie.pdf".r

    // The intended file name, used for simply moving files
    val fileRe3: Regex = raw"(\d\d\d\d)(\d\d)Payslip.pdf".r

    // All  the files in the directory
    val files = srcDir.list
    val renaming: Seq[(Path, Option[Path])] = files
      .map { file =>
        file.name match {
          case f @ fileRe1(mm, yyyy) => srcDir / f -> Some(dstDir / s"$yyyy${mm}Payslip.pdf")
          case f @ fileRe2(mm, yyyy) => srcDir / f -> Some(dstDir / s"$yyyy${mm}Payslip.pdf")
          case f @ fileRe3(mm, yyyy) => srcDir / f -> Some(dstDir / s"$yyyy${mm}Payslip.pdf")
          case _                     => file -> None
        }
      }
      .sortBy(_._1)

    renaming.filter(_._2.isEmpty).foreach(f => out.vPrintln(out.error(s"# Not a match ${f._1}")))
    renaming.foreach {
      case (srcFile, Some(dstFile)) if srcFile == dstFile => out.vPrintln(out.warn(s"# No move necessary $srcFile"))
      case (srcFile, Some(dstFile)) =>
        if (dryRun)
          println(out.ok(s"""mv "$srcFile" "$dstFile""""))
        else
          Files.move(srcFile, dstFile)
      case _ =>
    }
  }
}
