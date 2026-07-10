package com.skraba.byexample.scalatools.gitchecker

import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, FsPath, PathValidator, Task}
import com.tinfoiled.markd.Markd

import java.nio.file.{Files, Path}
import scala.util.{Failure, Left, Random, Right, Success, Try}

object CherryPickerReportTask extends Task {
  override val Description: String = "Create a cherry-picking report between two branches"
  override val Cmd: String = "cherrypick"
  override val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${GitCheckerGo.Name} $Cmd [CMD] [options]
       |
       |Options:
       |  -h --help    Show this screen
       |  --version    Show version
       |  CMD          The command to execute for modifying the last commit date
       |  --src=SRC    The directory where the git repo is located (By default, the
       |               current working directory)
       |  --dst=DST    The output file to write the report.
       |  --lTag=LTAG  The "left"   branch of the "left"
       |  --rTag=RTAG
       |  --noVerbose  True if the action should be silent (verbose by default)
       |  --plain      Do not show colour output
       |  --yes        Quiet mode, assume "yes" to all Y/N prompts
       |
       |""".stripMargin.trim

  override def go(opt: Docopt): Unit = {
    val src: Path = opt.dir.getOr("--src", FsPath.Pwd)
    val statusDoc: Option[Path] = opt.file.getOption("--dst", PathValidator().optionallyExists())
    val rTag: String = opt.string.getOr("--rTag", "main")
    val lTag: String = opt.string.getOr("--lTag", "branch")
    val out = AnsiConsole(verbose = !opt.flag("--noVerbose"), plain = opt.flag("--plain"), yes = opt.flag("--yes"))

    out.vPrint(
      s"""${out.bold("Arguments:")}
         |${out.kv("      repo:", src)}
         |${out.kv("      rTag:", rTag)}
         |${out.kv("      lTag:", lTag)}
         |${out.kv(" statusDoc:", statusDoc)}
         |
         |${out.bold("Git command:")}
         |${(CherryPickerReport.Cmd :+ s"$lTag...$rTag").mkString(" ")}
         |
         |""".stripMargin
    )

    // The current state of the git branches
    val current: CherryPickerReport = CherryPickerReport.fromGit(src, lTag, rTag)

    // If the status document exists, merge it with the current state
    val updated = statusDoc
      .flatMap(_.safeSlurp())
      .map(Markd.parse(_))
      .map(CherryPickerReport.fromDoc(_).update(current))
      .getOrElse(current)

    val cleaned = Markd.parse(updated.toDoc.build().toString)
    val txt = cleaned.build().toString
    if (statusDoc.isEmpty) println(txt)

    statusDoc.foreach(_.writeAll(txt))
  }
}
