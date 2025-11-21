package com.skraba.byexample.scala.ammonite

import com.tinfoiled.docopt4s.AnsiConsole
import os.{Path, home}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.util.{Failure, Try}

/** Utilities and helpers for renaming files in my ammonite scripts. */
object FileRenamer {

  class MissingPhoneDirException() extends RuntimeException("Unable to find pics storage.")

  /** @param phoneTag
    *   A substring that must be in the path of the phone (i.e. SAMSUNG or the phone model).
    * @return
    *   Find the directory that corresponds to a connected USB phone, or null for None
    */
  def phoneDir(gvfs: Option[Path], phoneTag: Option[String] = None): Path = {
    gvfs
      .map(os.list(_).filter(_.toString.contains(phoneTag.getOrElse(""))))
      .flatMap(_.headOption)
      .flatMap(os.list(_).headOption)
      .getOrElse(throw new MissingPhoneDirException())
  }

  /** Copies files from a directory, usually a phone, into the local hard disk. On the phone, files are moved into a
    * subdirectory to indicate they've already been copied.
    *
    * @param deviceRootDir
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
    * @param phoneMountDir
    *   The location where the phone is mounted in the filesystem (Default: /run/user/UID/gvfs)
    * @param phoneTag
    *   A substring to search for when finding where the phone might be mounted
    * @param dryRun
    *   True if no files should actually be copied or moved
    * @param console
    *   A configuration for running
    */
  def cameraphone(
      deviceRootDir: Option[Path],
      deviceBackedupSubDir: Option[String],
      deviceRelPath: Seq[String],
      extension: Seq[String],
      dst: Option[Path],
      dstSub: Option[String],
      dstSuffix: String,
      phoneMountDir: Option[Path],
      phoneTag: Option[String],
      dryRun: Boolean,
      console: AnsiConsole
  ): Unit = {
    // These defaults need to be a bit more refined
    val deviceRelPathsWithDefault = if (deviceRelPath.nonEmpty) deviceRelPath else Seq("DCIM/Camera")
    val extensionsWithDefault = if (extension.nonEmpty) extension else Seq("mp4", "jpg")
    val deviceBackedupSubDirWithDefault: String =
      deviceBackedupSubDir.getOrElse(s"backedup${DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())}")

    val gvfs = phoneMountDir
      .orElse(
        Some(os.root / "run" / "user")
          .find(os.exists)
          .flatMap(os.list(_).headOption)
          .map(_ / "gvfs")
      ) // /run/user/1000/gvfs
      .find(os.isDir)

    // If both deviceRootDir and phoneTag are empty, there might be more than one phone currently connected.
    (deviceRootDir, phoneTag) match {
      case (None, None) =>
        val mounted: Seq[Path] = gvfs.toSeq.flatMap(os.list)
        // if there's more than one phone connected, then do each one individually.
        if (mounted.size > 1) {
          // Ignore MissingPhoneDirException if at least one did succeed
          var atLeastOneSuccess = false
          for (phone <- mounted)
            try {
              cameraphone(
                deviceRootDir,
                deviceBackedupSubDir,
                deviceRelPath,
                extension,
                dst,
                dstSub,
                dstSuffix,
                phoneMountDir,
                phoneTag = Some(phone.baseName),
                dryRun,
                console
              )
              atLeastOneSuccess = true
            } catch {
              case ex: MissingPhoneDirException if atLeastOneSuccess || phone != mounted.last => None
              case ex                                                                         => throw ex
            }
          return
        }
      case _ =>
    }

    // Find all the files that exist in the device subdirectories
    val files = {
      // Use the given src for the device, or try to detect it
      val rootDir = deviceRootDir.getOrElse(phoneDir(gvfs, phoneTag))
      console.vPrintln(rootDir)

      for (mediaDir <- deviceRelPathsWithDefault.map(os.RelPath.apply(_)).map(rootDir / _)) yield {
        if (!os.exists(mediaDir)) {
          console.vPrintln(console.warn("Source directory not found", mediaDir))
          Seq.empty
        } else {
          val files = os.list(mediaDir).filter(os.isFile)
          console.vPrintln(s"There are ${files.size} files in <SRC>/${mediaDir.relativeTo(rootDir)}.")
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
      extensionsWithDefault.flatMap(byExtension.get).flatten
    }

    if (filesToCopy.isEmpty) {
      console.vPrintln(console.ok("No files to copy", bold = true))
      return
    }

    // The actual destination directory or the default
    val dstRoot = dst.getOrElse(os.home / "Pictures")
    if (!os.exists(dstRoot)) {
      println(console.error("Destination directory not found", dstRoot))
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
    if (!dryRun) os.makeDir(dstDir)

    for (file <- filesToCopy) {
      if (dryRun) {
        println(s"cp $file ${dstDir / file.last}")
        println(s"mv $file ${file / os.up / deviceBackedupSubDirWithDefault / file.last}")
      } else {
        console.vPrint(s"${dstDir / file.last}.")
        os.copy(file, dstDir / file.last)
        console.vPrint(".")

        val deviceBackedupDir = file / os.up / deviceBackedupSubDirWithDefault
        os.makeDir.all(deviceBackedupDir)
        os.move(file, deviceBackedupDir / file.last)
        console.vPrintln(".")
      }
    }

  }

}
