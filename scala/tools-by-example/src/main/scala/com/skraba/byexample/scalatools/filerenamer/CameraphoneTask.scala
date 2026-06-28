package com.skraba.byexample.scalatools.filerenamer

import com.tinfoiled.docopt4s.FsPath._
import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, PathValidator, Task}

import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate
import java.time.format.DateTimeFormatter

/** Backs up pictures from the connected phone. */
object CameraphoneTask extends Task {
  override val Description: String = "Backs up pictures from the connected phone"
  override val Cmd: String = "cameraphone"
  override val Doc: String =
    s"""${Description}
       |
       |Usage:
       |  ${FileRenamerGo.Name} $Cmd [options] [--extension=EXT]...
       |      [--deviceRelPath=REL]...
       |
       |Options:
       |  -h --help                     Show this screen
       |  --version                     Show version
       |  --deviceRootDir=ROOT
       |        The full path of the device containing pictures on the local
       |        filesystem. If unset, all of the devices found in the --phoneMountDir
       |         will be processed.  This corresponds to `/` on the actual device.
       |  --phoneMountDir=PHONEMNT
       |        If --deviceRootDir is unset, this is the location where we look for
       |        phone filesystems mounted on the local filesystem.  (Default:
       |        /run/user/UID/gvfs)
       |  --phoneTag=TAG
       |        If there are multiple phones present in the --phoneMountDir, the tag
       |        is used to select a single one.
       |  --deviceBackedupSubDir=BCKUP
       |        The subdirectory to move source directories once they are copied. By
       |        default, the subdirectory is constructed by the date as
       |        `backedupYYYYMMDD`
       |  --deviceRelPath=REL
       |        From the device root, the relative paths to find media in.
       |        (Default: DCIM/Camera)
       |  --extension=EXT
       |        The extensions to copy and move. (Default: mp4 and jpg)
       |  --dst=DST
       |        Create directories here to copy or move out media.
       |        (Default: ~/Pictures)
       |  --dstSub=DSTSUB
       |        If specified, the name of the created subdirectory in dst
       |        (Default: Autocreated with a date prefix)
       |  --dstSuffix=DSTSFX
       |        When autocreating the destination subdirectory, a suffix after the
       |        date. By default, uses " Cameraphone"
       |  --dryRun
       |        True if no files should actually be copied or moved
       |  --noVerbose
       |        True if the action should be silent (verbose by default)
       |  --plain
       |        Do not show colour output
       |  --yes
       |        Quiet mode, assume "yes" to all Y/N prompts
       |
       |""".stripMargin.trim

  /** An exception for when we can't find the base directory for the mounted phone. */
  class MissingPhoneDirException(root: String) extends RuntimeException(s"Unable to find phone storage ($root).")

  /** Given the input parameters, finds the list of root directories that should be processed.
    *
    * @param deviceRootDir
    *   The full path of the device containing pictures on the local filesystem. If unset, all of the devices found in
    *   the --phoneMountDir will be processed. This corresponds to `/` on the actual device.
    * @param phoneMountDir
    *   If --deviceRootDir is unset, this is the location where we look for phone filesystems mounted on the local
    *   filesystem. (Default: /run/user/UID/gvfs)
    * @param phoneTag
    *   If there are multiple phones present in the --phoneMountDir, the tag is used to select a single one.
    */
  def findRootDirs(deviceRootDir: Option[Path], phoneMountDir: Option[Path], phoneTag: Option[String]): Seq[Path] = {
    lazy val gvfs = phoneMountDir
      .orElse(
        Some(Paths.get("/run/user"))
          .find(_.exists)
          .flatMap(_.list.headOption)
          .map(_ / "gvfs")
      ) // /run/user/1000/gvfs
      .find(Files.isDirectory(_))

    // If both deviceRootDir and phoneTag are empty, there might be more than one phone currently connected.
    (deviceRootDir, phoneTag) match {
      case (Some(rootDir), _) => Seq(rootDir)
      case (None, Some(tag)) =>
        gvfs
          .map(_.list.filter(_.toString.contains(phoneTag)))
          .flatMap(_.headOption)
          .flatMap(_.list.headOption) match {
          case Some(found) => Seq(found)
          case _           => throw new MissingPhoneDirException(Seq(gvfs, phoneTag).flatten.mkString(" "))
        }
      case (None, None) =>
        val phoneTags = gvfs.toSeq.flatMap(_.list).sortBy(_.toString)
        val storageDirs = phoneTags.flatMap(_.list)
        if (storageDirs.isEmpty)
          throw new MissingPhoneDirException(Seq(gvfs, phoneTags.headOption.map(_.name)).flatten.mkString(" "))
        storageDirs
    }
  }

  lazy val Today: String = DateTimeFormatter.ofPattern("yyyyMMdd").format(LocalDate.now())
  lazy val DefaultBackupDir = s"backedup${DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())}"

  override def go(opt: Docopt): Unit = {
    go(opt, " Cameraphone", Seq("DCIM/Camera"))
  }

  def go(opt: Docopt, dfltDstSuffix: String, dfltDeviceRelPath: Seq[String]): Unit = {
    // To find the source of the mounted phone
    val deviceRootDir = opt.dir.getOption("--deviceRootDir")
    val phoneMountDir = opt.dir.getOption("--phoneMountDir")
    val phoneTag = opt.string.getOption("--phoneTag")

    val deviceBackedupSubDir = opt.string.getOr("--deviceBackedupSubDir", DefaultBackupDir)
    val deviceRelPath = opt.strings.getOrEmpty("--deviceRelPath") match {
      case xs if xs.isEmpty => dfltDeviceRelPath
      case other            => other
    }
    val extension = opt.strings.getOrEmpty("--extension") match {
      case xs if xs.isEmpty => Seq("mp4", "jpg")
      case other            => other
    }
    val dst = opt.dir.getOption("--dst", PathValidator().optionallyExists())
    val dstSub = opt.string.getOption("--dstSub")
    val dstSuffix = opt.string.getOr("--dstSuffix", dfltDstSuffix)
    val dryRun = opt.boolean.get("--dryRun")

    val console = AnsiConsole(verbose = !opt.flag("--noVerbose"), plain = opt.flag("--plain"))

    val rootDirs = findRootDirs(deviceRootDir, phoneMountDir, phoneTag).sortBy(_.toString)
    var atLeastOneSuccess = false
    for (rootDir <- rootDirs)
      cameraphone(
        rootDir,
        deviceBackedupSubDir = deviceBackedupSubDir,
        deviceRelPath = deviceRelPath.toSeq,
        extension = extension.toSeq,
        dst = dst,
        dstSub = dstSub,
        dstSuffix = dstSuffix,
        dryRun = dryRun,
        console = console
      )
    atLeastOneSuccess = true
  }

  /** Copies files from a directory, usually a phone, into the local hard disk. On the phone, files are moved into a
    * subdirectory to indicate they've already been copied.
    *
    * @param rootDir
    *   The root path of the device containing pictures (Default: autodetected in /run/media)
    * @param deviceBackedupSubDir
    *   The subdirectory to move source directories once they are copied (Default: backedupYYYYMMDD)
    * @param deviceRelPath
    *   From the device root, the relative paths to find media in (Default: DCIM/Camera)
    * @param extension
    *   The extensions to copy and move (Default: mp4 and jpg).
    * @param dst
    *   Create directories here to copy or move out media. (Default: ~/Pictures).
    * @param dstSub
    *   If specified, the name of the created subdirectory in dst (Default: Autocreated with a date prefix)
    * @param dstSuffix
    *   When autocreating the destination subdirectory, a suffix after the date (Default: Cameraphone)
    * @param dryRun
    *   True if no files should actually be copied or moved
    * @param console
    *   A configuration for running
    */
  def cameraphone(
      deviceRootDir: Path,
      deviceBackedupSubDir: String,
      deviceRelPath: Seq[String],
      extension: Seq[String],
      dst: Option[Path],
      dstSub: Option[String],
      dstSuffix: String,
      dryRun: Boolean,
      console: AnsiConsole
  ): Unit = {

    // Find all the files that exist in the device subdirectories
    val files = {
      // Use the given src for the device, or try to detect it
      console.vPrintln(deviceRootDir)

      for (mediaDir <- deviceRelPath.map(deviceRootDir / _)) yield {
        if (!mediaDir.exists) {
          console.vPrintln(console.warn("Source directory not found", mediaDir))
          Seq.empty
        } else {
          val files = mediaDir.list.filter(Files.isRegularFile(_))
          console.vPrintln(s"There are ${files.size} files in <SRC>/${deviceRootDir.relativize(mediaDir)}.")
          files
        }
      }
    }

    // Only consider the valid extensions
    val filesToCopy = {
      val byExtension = files.flatten.groupBy(_.ext)
      for (ext <- byExtension) {
        console.vPrintln(s"  ${console.bold(ext._1)}: ${ext._2.size}")
      }
      extension.flatMap(byExtension.get).flatten
    }.sortBy(_.toString)

    if (filesToCopy.isEmpty) {
      console.vPrintln(console.ok("No files to copy", bold = true))
      return
    }

    // The actual destination directory or the default
    val dstRoot = dst.getOrElse(Home / "Pictures")
    if (!dstRoot.exists) {
      println(console.error("Destination directory not found", dstRoot))
      return
    }

    // The actual destination subdirectory or the auto-generated default
    val actualDstSub: String = dstSub.getOrElse {
      // Find alternatives if the directory already exists
      LazyList
        .from(2)
        .map("-" + _)
        .prepended("")
        .map(Today + _ + dstSuffix)
        .filterNot(sub => (dstRoot / sub).exists)
        .head
    }

    val dstDir = dstRoot / actualDstSub
    if (!dryRun) dstDir.createDirectory(failIfExists = false)

    for (file <- filesToCopy) {
      if (dryRun) {
        println(s"cp $file ${dstDir / file.getFileName}")
        println(s"mv $file ${file.getParent / deviceBackedupSubDir / file.getFileName}")
      } else {
        console.vPrint(s"${dstDir / file.getFileName}.")
        Files.copy(file, dstDir / file.getFileName)
        console.vPrint(".")

        val deviceBackedupDir = file.getParent / deviceBackedupSubDir
        deviceBackedupDir.createDirectory(failIfExists = false)
        Files.move(file, deviceBackedupDir / file.getFileName)
        console.vPrintln(".")
      }
    }
  }
}
