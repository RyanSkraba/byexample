#!/usr/bin/env amm

/** A user script for renaming files that are close together in time. This is
  * useful for photos that were taken in close proximity.
  *
  * This assumes that https://ammonite.io/ is installed.
  *
  * Ammonite includes:
  *   - requests (https://github.com/lihaoyi/requests-scala)
  *   - upickle (https://github.com/lihaoyi/upickle)
  */

import mainargs.{arg, main}

import scala.concurrent.duration.DurationInt
import scala.io.AnsiColor._
import scala.util.matching.Regex

import $file.local_import_util
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ColourCfg

// ==========================================================================
// Top level variables available to the script

val DefaultTimeGap = 30.seconds

// ==========================================================================
// help

@arg(doc = "Print help to the console.")
@main
def help(cfg: ColourCfg): Unit = {

  // The help header includes all of the subcommands
  val cli = "getting_things_done.sc"
  println(
    cfg.helpHeader(
      "file_renamer.sc",
      "File operations for general clean-up",
      "group" -> "Rename files grouped by time",
      "payslip" -> "Rename payslip files",
      "pics" -> "Copy pics from the phone"
    )
  )

  // Usage examples
  println(cfg.helpUse(cli, "group", "[DIR]"))
  println(cfg.helpUse(cli, "payslip", "[DIR]"))
  println(cfg.helpUse(cli, "pics", "[DIR]"))
  println()
}

/** @return
  *   Find the directory that corresponds to a connected USB phone, or null for
  *   None
  */
private[this] def findPhoneStorage(): Option[os.Path] = {
  Some(os.root / "run" / "user")
    .find(os.exists)
    .flatMap(os.list(_).headOption)
    .map(_ / "gvfs")
    .flatMap(os.list(_).headOption)
    .flatMap(os.list(_).headOption)
}

@arg(doc = "Group the files by time and rename them with the same root name ")
@main
def pics(
    dir: Option[os.Path] = None
): Unit = {
  val src: os.Path = dir.orElse(findPhoneStorage()).get
  println(src)
}

@arg(doc = "Group the files by time and rename them with the same root name ")
@main
def group(
    dir: Option[os.Path] = None,
    gap: Long = DefaultTimeGap.toMillis
): Unit = {
  // The source path to analyse
  val src: os.Path = dir.getOrElse(os.pwd)
  if (!(os.exists(src))) {
    println(s"$RED${BOLD}ERROR:$RESET $src does not exist.")
    System.exit(1)
  }

  // All of the files and their creation dates.
  val files: List[os.Path] = os.list(src).toList.sortBy(os.mtime(_))

  // A fold function that groups elements that are in the same "window" (i.e. separated by less
  // than the gap).
  val foldGroupByTime: (List[List[os.Path]], os.Path) => List[List[os.Path]] =
    (acc, file) =>
      acc match {
        // If there is already a head group within the gap, then add this one before in the same group.
        case headGroup :: rest
            if (os.mtime(file) - os.mtime(headGroup.head)) < gap =>
          (file :: headGroup) :: rest
        // If there isn't a list, or the head element is outside the gap then add it as a new group
        case rest => List(file) :: rest
      }

  // And the grouped files.
  val groupedByTime: List[List[os.Path]] =
    files
      .foldLeft[List[List[os.Path]]](Nil)(foldGroupByTime)
      .reverse
      .map(_.reverse)

  // This just asks for a name and prints out the mv command.
  val commands: Seq[String] = groupedByTime.flatMap { files =>
    // Print to the screen
    println(s"$RED${BOLD}========================================$RESET")
    println(s"$RED${BOLD}Group ${os.mtime(files.head)}$RESET")
    files.foreach { f => println(s"  ${BOLD}${os.mtime(f)}:$RESET ${f.last}") }

    // Prompt for a new name
    val prompt1: String = scala.io.StdIn.readLine(
      s"  $BLUE${BOLD}Rename group:$RESET"
    )

    // Print the commands to the screen for each group with an indent
    var i = 0;
    for (f <- files) yield {
      val cmd = s"mv ${f.last} ${prompt1}_$i.${f.ext}"
      println(s"  ${BOLD}$cmd$RESET")
      i = i + 1
      cmd
    }
  }

  // Print the command summaries
  println(s"$GREEN${BOLD}========================================$RESET")
  for (cmd <- commands)
    println(s"${BOLD}$cmd$RESET")
}

@arg(doc = "Renames payslip files to a standard format")
@main
def payslip(
    srcPath: Option[os.Path] = None,
    dstPath: Option[os.Path] = None
): Unit = {

  // Error if the directory doesn't exist.
  val src: os.Path = srcPath.getOrElse(os.pwd)
  if (!(os.exists(src))) {
    println(s"$RED${BOLD}ERROR:$RESET $src does not exist.")
    System.exit(1)
  }

  val dst: os.Path = dstPath.getOrElse(src)
  if (!(os.exists(dst))) {
    println(s"$RED${BOLD}ERROR:$RESET $dst does not exist.")
    System.exit(1)
  }

  // The two file names that are commonly sent as payslip files
  val fileRe1: Regex = raw"Bulletins (\d\d)_(\d\d\d\d).pdf".r
  val fileRe2: Regex = raw"(\d\d)-(\d\d\d\d)_bulletin_de_paie.pdf".r

  // All of the files in the directory
  val files: List[os.Path] = os.list(src).toList
  files
    .map { file =>
      file.last match {
        case fileRe1(mm, yyyy) =>
          s"""mv "$file" "$dst/$yyyy${mm}Payslip.pdf""""
        case fileRe2(mm, yyyy) =>
          s"""mv "$file" "$dst/$yyyy${mm}Payslip.pdf""""
        case _ => s"# unmatched $file"
      }
    }
    .sorted
    .foreach(println(_))

  // TODO: Verify that no files are deleted or removed by this operation
}
