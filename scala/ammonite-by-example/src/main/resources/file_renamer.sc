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
local_import_util.load("scala-by-example")
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
def help(out: ConsoleCfg): Unit = {

  // The help header includes all of the subcommands
  val cli = "getting_things_done.sc"
  println(
    out.helpHeader(
      cli = "file_renamer.sc",
      description = "File operations for general clean-up",
      subcommands = "cameraphone" -> "Backs up pictures from the connected phone",
      "group" -> "Rename files grouped by time",
      "monthify" -> "Move image files to directories by month",
      "payslip" -> "Rename payslip files",
      "screenshot" -> "Backs up screenshots from the connected phone"
    )
  )

  // Usage examples
  println(out.helpUse(cli, "group", "[DIR]"))
  println(out.helpUse(cli, "payslip", "[DIR]"))
  println(out.helpUse(cli, "cameraphone", "[DIR]"))
  println(out.helpUse(cli, "monthify", "[DIR]"))
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
    @arg(doc = "The root path of the device containing pictures (Default: autodetected in /run/media)")
    deviceRootDir: Option[os.Path] = None,
    @arg(doc = "The subdirectory to move source directories once they are copied (Default: backedupYYYYMMDD)")
    deviceBackedupSubDir: Option[String] = None,
    @arg(doc = "From the device root, the relative paths to find media in (Default: DCIM/Camera)")
    deviceRelPath: Seq[String],
    @arg(doc = "The extensions to copy and move (Default: mp4 and jpg).")
    extension: Seq[String],
    @arg(doc = "Create directories here to copy or move out media. (Default: ~/Pictures).")
    dst: Option[os.Path] = None,
    @arg(doc = "If specified, the name of the created subdirectory in dst (Default: Autocreated with a date prefix)")
    dstSub: Option[String] = None,
    @arg(doc = "When autocreating the destination subdirectory, a suffix after the date (Default: Cameraphone)")
    dstSuffix: String = " Cameraphone",
    @arg(doc = "A substring to search for when finding where the phone might be mounted")
    phoneTag: Option[String] = None,
    @arg(doc = "True if no files should actually be copied or moved")
    dryRun: Flag,
    @arg(doc = "True if the action should be silent (verbose by default)")
    noVerbose: Flag = Flag(false),
    cfgGroup: ConsoleCfg
): Unit = {

  // A config with verbose on by default
  val cfg = cfgGroup.withVerbose(!noVerbose.value)

  // These defaults need to be a bit more refined
  val deviceRelPathsWithDefault = if (deviceRelPath.nonEmpty) deviceRelPath else Seq("DCIM/Camera")
  val extensionsWithDefault = if (extension.nonEmpty) extension else Seq("mp4", "jpg")
  val deviceBackedupSubDirWithDefault: String =
    deviceBackedupSubDir.getOrElse(s"backedup${DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())}")

  // Find all of the files that exist in the device subdirectories
  val files = {
    // Use the given src for the device, or try to detect it
    val rootDir = deviceRootDir.getOrElse(phoneDir(phoneTag))
    cfg.vPrintln(rootDir)

    for (mediaDir <- deviceRelPathsWithDefault.map(os.RelPath.apply(_)).map(rootDir / _)) yield {
      if (!os.exists(mediaDir)) {
        cfg.vPrintln(cfg.warn("Source directory not found", mediaDir))
        Seq.empty
      } else {
        val files = os.list(mediaDir).filter(os.isFile)
        cfg.vPrintln(s"There are ${files.size} files in <SRC>/${mediaDir.relativeTo(rootDir)}.")
        files
      }
    }
  }

  // Only consider the valid extensions
  val filesToCopy = {
    val byExtension = files.flatten.groupBy(_.ext)
    for (ext <- byExtension) {
      cfg.vPrintln(s"  ${cfg.bold(ext._1)}: ${ext._2.size}")
    }
    extensionsWithDefault.flatMap(byExtension.get).flatten
  }

  if (filesToCopy.isEmpty) {
    cfg.vPrintln(cfg.ok("No files to copy", bold = true))
    return
  }

  // The actual destination directory or the default
  val dstRoot = dst.getOrElse(os.home / "Pictures")
  if (!os.exists(dstRoot)) {
    println(cfg.error("Destination directory not found", dstRoot))
    return
  }

  // The actual destination subdirectory or the auto-generated default
  val actualDstSub: String = dstSub.getOrElse {
    // The tag for today
    val today = DateTimeFormatter.ofPattern("yyyyMMdd").format(LocalDate.now())

    // Find alternatives if the directory already exists
    LazyList
      .from(2)
      .map("-" + _)
      .prepended("")
      .map(today + _ + dstSuffix)
      .filterNot(sub => os.exists(dstRoot / sub))
      .head
  }

  val dstDir = dstRoot / actualDstSub
  if (!dryRun.value) os.makeDir(dstDir)

  for (file <- filesToCopy) {
    if (dryRun.value) {
      println(s"cp $file ${dstDir / file.last}")
      println(s"mv $file ${file / os.up / deviceBackedupSubDirWithDefault / file.last}")
    } else {
      cfg.vPrint(s"${dstDir / file.last}.")
      os.copy(file, dstDir / file.last)
      cfg.vPrint(".")

      val deviceBackedupDir = file / os.up / deviceBackedupSubDirWithDefault
      os.makeDir.all(deviceBackedupDir)
      os.move(file, deviceBackedupDir / file.last)
      cfg.vPrintln(".")
    }
  }
}

@arg(doc = "Backs up screenshots from the connected phone")
@main
def screenshot(
    @arg(doc = "The root path of the device containing pictures (Default: autodetected in /run/media)")
    deviceRootDir: Option[os.Path] = None,
    @arg(doc = "The subdirectory to move source directories once they are copied (Default: backedupYYYYMMDD)")
    deviceBackedupSubDir: Option[String] = None,
    @arg(doc = "From the device root, relative paths to find media (Default: Pictures/Screenshots, DCIM/Screenshots)")
    deviceRelPath: Seq[String],
    @arg(doc = "The extensions to copy and move (Default: mp4 and jpg).")
    extension: Seq[String],
    @arg(doc = "Create directories here to copy or move out media. (Default: ~/Pictures).")
    dst: Option[os.Path] = None,
    @arg(doc = "If specified, the name of the created subdirectory in dst (Default: Autocreated with a date prefix)")
    dstSub: Option[String] = None,
    @arg(doc = "When autocreating the destination subdirectory, a suffix after the date (Default: Cameraphone)")
    dstSuffix: String = " Screenshots",
    @arg(doc = "A substring to search for when finding where the phone might be mounted")
    phoneTag: Option[String] = None,
    @arg(doc = "True if no files should actually be copied or moved")
    dryRun: Flag,
    @arg(doc = "True if the action should be silent (verbose by default)")
    noVerbose: Flag = Flag(false),
    cfgGroup: ConsoleCfg
): Unit = {
// This is identical to the cameraphone task but with different defauls
  cameraphone(
    deviceRootDir = deviceRootDir,
    deviceBackedupSubDir = deviceBackedupSubDir,
    deviceRelPath = if (deviceRelPath.nonEmpty) deviceRelPath else Seq("Pictures/Screenshots", "DCIM/Screenshots"),
    extension = extension,
    dst = dst,
    dstSub = dstSub,
    dstSuffix = dstSuffix,
    phoneTag = phoneTag,
    dryRun = dryRun,
    noVerbose = noVerbose,
    cfgGroup = cfgGroup
  )
}

@arg(doc = "Given a directory with dated photo files, moves them to directories sorted by YYYYMM")
@main
def monthify(
    @arg(doc = "The directory to analyse and move files from")
    src: os.Path,
    @arg(doc = "The destination directory to create YYYYMM subdirectories and move files to")
    dst: Option[os.Path] = None,
    @arg(doc = "The prefix of the YYYYMM directories in dst")
    prefix: String = "",
    @arg(doc = "True if no files should actually be copied or moved")
    dryRun: Flag,
    @arg(doc = "True if the action should be silent (verbose by default)")
    noVerbose: Flag = Flag(false),
    cfgGroup: ConsoleCfg
): Unit = {
  val cfg = cfgGroup.withVerbose(!noVerbose.value)

  // Use the given src for the device, or try to detect it
  val actualDst = dst.getOrElse(src)
  cfg.vPrintln(cfg.bold("Destination: ") + actualDst)

  // The regex used to extract dates from the filenames
  val DateExtract: Regex = raw"^(\D+_)?(\d{4})(\d{2})(\d{2})_(\d{2})(\d{2})(\d{2}).*".r

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
    val dstDir = actualDst / (prefix + yymm)
    cfg.vPrint(s"  ${cfg.ok(dstDir.last, bold = true)}: Moving files (${fToMove.size})")

    if (!dryRun.value) os.makeDir.all(dstDir)
    fToMove.foreach { fmv =>
      cfg.vPrint(".")
      if (!dryRun.value) os.move(src / fmv, dstDir / fmv)
    }
    cfg.vPrintln()
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
def payslip(srcPath: Option[os.Path] = None, dstPath: Option[os.Path] = None, out: ConsoleCfg): Unit = {

  // Error if the directory doesn't exist.
  val src: os.Path = srcPath.getOrElse(os.pwd)
  if (!(os.exists(src))) {
    println(out.error("ERROR", s"$src does not exist."))
    System.exit(1)
  }

  val dst: os.Path = dstPath.getOrElse(src)
  if (!(os.exists(dst))) {
    println(out.error("ERROR", s"$dst does not exist."))
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

  renaming.filter(_._2.isEmpty).foreach(f => out.vPrintln(out.error(s"# Not a match ${f._1}")))
  renaming.foreach {
    case (srcFile, Some(dstFile)) if srcFile == dstFile => out.vPrintln(out.warn(s"# No move necessary $srcFile"))
    case (srcFile, Some(dstFile))                       => println(out.ok(s"""mv "$srcFile" "$dstFile""""))
    case _                                              =>
  }
}
