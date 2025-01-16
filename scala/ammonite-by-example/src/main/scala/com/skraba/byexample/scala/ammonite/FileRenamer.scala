package com.skraba.byexample.scala.ammonite

import os.Path

import java.time.LocalDate
import java.time.format.DateTimeFormatter

/** Utilities and helpers for renaming files in my ammonite scripts. */
object FileRenamer {

  /** @param phoneTag
    *   A substring that must be in the path of the phone (i.e. SAMSUNG or the phone model).
    * @return
    *   Find the directory that corresponds to a connected USB phone, or null for None
    */
  def phoneDir(phoneTag: Option[String] = None): Path = {
    Some(os.root / "run" / "user")
      .find(os.exists)
      .flatMap(os.list(_).headOption)
      .map(_ / "gvfs") // /run/user/1000/gvfs
      .map(os.list(_).filter(_.toString.contains(phoneTag.getOrElse(""))))
      .flatMap(_.headOption)
      .flatMap(os.list(_).headOption)
      .getOrElse(throw new RuntimeException("Unable to find pics storage."))
  }

  def cameraphone(
      deviceRootDir: Option[Path],
      deviceBackedupSubDir: Option[String],
      deviceRelPath: Seq[String],
      extension: Seq[String],
      dst: Option[Path],
      dstSub: Option[String],
      dstSuffix: String,
      phoneTag: Option[String],
      dryRun: Boolean,
      cfg: ConsoleCfg
  ): Unit = {
    // These defaults need to be a bit more refined
    val deviceRelPathsWithDefault = if (deviceRelPath.nonEmpty) deviceRelPath else Seq("DCIM/Camera")
    val extensionsWithDefault = if (extension.nonEmpty) extension else Seq("mp4", "jpg")
    val deviceBackedupSubDirWithDefault: String =
      deviceBackedupSubDir.getOrElse(s"backedup${DateTimeFormatter.ofPattern("yyyyMM").format(LocalDate.now())}")

    // Find all the files that exist in the device subdirectories
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
    if (!dryRun) os.makeDir(dstDir)

    for (file <- filesToCopy) {
      if (dryRun) {
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

}
