#!/usr/bin/env amm

/** A user script concentrating on file operations */
import mainargs.{Flag, arg, main}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import os.Path
import scala.concurrent.duration.DurationInt
import scala.io.AnsiColor._
import scala.util.matching.Regex

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ConsoleCfg

// ==========================================================================
// Top level variables available to the script

val DefaultTimeGap = 30.seconds

// ==========================================================================
// help

@arg(doc = "Print help to the console.")
@main
def help(cfg: ConsoleCfg): Unit = {

  // The help header includes all of the subcommands
  val cli = "getting_things_done.sc"
  println(
    cfg.helpHeader(
      "file_renamer.sc",
      "File operations for general clean-up",
      "cameraphone" -> "Backs up pictures from the connected phone",
      "group" -> "Rename files grouped by time",
      "payslip" -> "Rename payslip files"
    )
  )

  // Usage examples
  println(cfg.helpUse(cli, "group", "[DIR]"))
  println(cfg.helpUse(cli, "payslip", "[DIR]"))
  println(cfg.helpUse(cli, "cameraphone", "[DIR]"))
  println()
}

/** @param phoneTag
  *   A substring that must be in the path of the phone (i.e. SAMSUNG or the phone model).
  * @return
  *   Find the directory that corresponds to a connected USB phone, or null for None
  */
@arg(doc = "Finds the phone directory, if any connected")
@main
def phoneDir(
    @arg(doc = "A substring to search for when finding where the phone might be mounted")
    phoneTag: Option[String] = None
): Path = {
  Some(os.root / "run" / "user")
    .find(os.exists)
    .flatMap(os.list(_).headOption)
    .map(_ / "gvfs") // /run/user/1000/gvfs
    .map(os.list(_).filter(_.toString.contains(phoneTag.getOrElse(""))))
    .flatMap(_.headOption)
    .flatMap(os.list(_).headOption)
    .getOrElse(throw new RuntimeException("Unable to find pics storage."))
}

@arg(doc = "Backs up pictures and media from the connected phone")
@main
def cameraphone(
    @arg(doc = "The root path of the device containing pictures, or None to detect")
    src: Option[os.Path] = None,
    @arg(doc = "The subdirectory to move source directories once they are copied")
    srcSub: Option[String] = None,
    @arg(doc = "Create directories here to copy or move out media. (Default: ~/Pictures).")
    dst: Option[os.Path] = None,
    @arg(doc = "If specified, the name of the created subdirectory in dst (Default: Autocreated with a date prefix)")
    dstSub: Option[String] = None,
    @arg(doc = "A substring to search for when finding where the phone might be mounted")
    phoneTag: Option[String] = None,
    @arg(doc = "True if no files should actually be copied or moved")
    dryRun: Flag,
    @arg(doc = "True if the action should be silent (verbose by default)")
    noVerbose: Flag = Flag(false),
    cfgGroup: ConsoleCfg
): Unit = {
  val cfg = cfgGroup.withVerbose(!noVerbose.value)

  // Use the given src for the device, or try to detect it
  val srcDir = src.getOrElse(phoneDir(phoneTag))
  cfg.vPrintln(srcDir)

  // This is one directory that might contain media in the device
  val mediaDir = srcDir / "DCIM" / "Camera"
  if (!os.exists(mediaDir)) {
    println(cfg.error("Source directory not found", mediaDir))
    return
  }

  val files = os.list(mediaDir).filter(os.isFile)
  cfg.vPrintln(s"There are ${files.size} files.")
  val byExtension = files.groupBy(_.ext)
  for (ext <- byExtension)
    cfg.vPrintln(s"  ${cfg.bold(ext._1)}: ${ext._2.size}")

  val filesToCopy = Seq("jpg", "mp4").flatMap(byExtension.get).flatten
  if (filesToCopy.isEmpty) {
    println(cfg.ok("No files to copy", bold = true))
    return
  }

  val dst2 = dst.getOrElse(os.home / "Pictures")
  if (!os.exists(dst2)) {
    println(cfg.error("Destination directory not found", dst2))
    return
  }

  val tag: String = dstSub.getOrElse {
    // The tag for today
    val today = DateTimeFormatter.ofPattern("yyyyMMdd").format(LocalDate.now())

    // Find alternatives if the directory already exists
    LazyList
      .from(2)
      .map("-" + _)
      .prepended("")
      .map(today + _ + " Cameraphone")
      .filterNot(sub => os.exists(dst2 / sub))
      .head
  }

  val dstDir = dst2 / tag
  if (!dryRun.value) os.makeDir(dstDir)

  val srcSubDir: String = srcSub.getOrElse(s"backedup${DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())}")
  val backupDir: Path = mediaDir / srcSubDir

  if (!dryRun.value) os.makeDir.all(backupDir)

  for (file <- filesToCopy) {
    if (dryRun.value) {
      println(s"cp $file ${dstDir / file.last}")
      println(s"mv $file ${backupDir / file.last}")
    } else {
      cfg.vPrint(s"${dstDir / file.last}.")
      os.copy(file, dstDir / file.last)
      cfg.vPrint(".")
      os.move(file, backupDir / file.last)
      cfg.vPrintln(".")
    }
  }
}

@arg(doc = "Given a directory with dated photo files, moves them to directories sorted by YYYYMM")
@main
def monthify(
    @arg(doc = "The directory to analyse and move files from")
    src: os.Path,
    @arg(doc = "The destination directory (before adding a YYYYMM suffix)")
    dst: Option[os.Path] = None,
    @arg(doc = "True if no files should actually be copied or moved")
    dryRun: Flag,
    @arg(doc = "True if the action should be silent (verbose by default)")
    noVerbose: Flag = Flag(false),
    cfgGroup: ConsoleCfg
): Unit = {
  val cfg = cfgGroup.withVerbose(!noVerbose.value)

  // Use the given src for the device, or try to detect it
  val dst2 = dst.getOrElse(src)
  cfg.vPrintln(s"dst: $dst2")

  // The regex used too extract dates from the filenames
  val DateExtract: Regex =
    raw"^(\D+_)?(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2}).*".r

  val files: Map[String, Seq[String]] =
    os.list(src)
      .filter(os.isFile)
      .map(_.last)
      .map(f => f -> f)
      .collect { case (f, DateExtract(_, yy, mm, _*)) =>
        (yy + mm, f)
      }
      .groupMap(_._1)(_._2)

  files.keySet.toSeq.sorted.foreach { yymm =>
    val fToMove = files(yymm)
    val dstDir = dst2 / os.up / (dst2.last + yymm)
    cfg.vPrintln(s"YYYYMM: $yymm (${fToMove.size} files) to ${dstDir.last}")

    if (!dryRun.value) os.makeDir.all(dstDir)
    fToMove.foreach { fmv =>
      cfg.vPrint(".")
      if (!dryRun.value) os.move(src / fmv, dstDir / fmv)
    }
    cfg.vPrintln(".")
  }
}

@arg(doc = "Group the files by time and rename them with the same root name")
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
        case headGroup :: rest if (os.mtime(file) - os.mtime(headGroup.head)) < gap => (file :: headGroup) :: rest
        // If there isn't a list, or the head element is outside the gap then add it as a new group
        case rest => List(file) :: rest
      }

  // And the grouped files.
  val groupedByTime: List[List[os.Path]] =
    files.foldLeft[List[List[os.Path]]](Nil)(foldGroupByTime).reverse.map(_.reverse)

  // This just asks for a name and prints out the mv command.
  val commands: Seq[String] = groupedByTime.flatMap { files =>
    // Print to the screen
    println(s"$RED${BOLD}========================================$RESET")
    println(s"$RED${BOLD}Group ${os.mtime(files.head)}$RESET")
    files.foreach { f => println(s"  ${BOLD}${os.mtime(f)}:$RESET ${f.last}") }

    // Prompt for a new name
    val prompt1: String = scala.io.StdIn.readLine(s"  $BLUE${BOLD}Rename group:$RESET")

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
  for (cmd <- commands) println(s"${BOLD}$cmd$RESET")
}

@arg(doc = "Renames payslip files to a standard format")
@main
def payslip(srcPath: Option[os.Path] = None, dstPath: Option[os.Path] = None, cfg: ConsoleCfg): Unit = {

  // Error if the directory doesn't exist.
  val src: os.Path = srcPath.getOrElse(os.pwd)
  if (!(os.exists(src))) {
    cfg.error("ERROR", s"$src does not exist.")
    System.exit(1)
  }

  val dst: os.Path = dstPath.getOrElse(src)
  if (!(os.exists(dst))) {
    cfg.error("ERROR", s"$dst does not exist.")
    System.exit(1)
  }

  // The two file names that are commonly sent as payslip files
  val fileRe1: Regex = raw"Bulletins (\d\d)_(\d\d\d\d).pdf".r
  val fileRe2: Regex = raw"(\d\d)-(\d\d\d\d)_bulletin_de_paie.pdf".r

  // The intended file name, used for simply moving files
  val fileRe3: Regex = raw"(\d\d\d\d)(\d\d)Payslip.pdf".r

  // All of the files in the directory
  val files: List[os.Path] = os.list(src).toList
  val renaming: List[(Path, Option[Path])] = files
    .map { file =>
      file.last match {
        case f @ fileRe1(mm, yyyy) => src / f -> Some(dst / s"$yyyy${mm}Payslip.pdf")
        case f @ fileRe2(mm, yyyy) => src / f -> Some(dst / s"$yyyy${mm}Payslip.pdf")
        case f @ fileRe3(mm, yyyy) => src / f -> Some(dst / s"$yyyy${mm}Payslip.pdf")
        case _                     => file -> None
      }
    }
    .sortBy(_._1)

  renaming.filter(_._2.isEmpty).foreach(f => cfg.vPrintln(s"# Not a match ${f._1}"))
  renaming.foreach {
    case (srcFile, Some(dstFile)) if src == dst => cfg.vPrintln(s"# No move necessary $srcFile")
    case (srcFile, Some(dstFile))               => println(s"""mv "$srcFile" "$dstFile"""")
    case _                                      =>
  }
}
