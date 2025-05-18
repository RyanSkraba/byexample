package com.skraba.byexample.scala.markd
import com.skraba.docoptcli.DocoptCliGo
import com.skraba.docoptcli.DocoptCliGo.Task
import com.tinfoiled.markd._

import scala.reflect.io.{Directory, File, Path}

/** A driver for the various utilities that use the [[Markd]] model.
  */
object MarkdGo extends DocoptCliGo {

  override lazy val Cli: String = "MarkdGo"

  override lazy val Version: String = "0.0.1-SNAPSHOT"

  override lazy val Tasks: Seq[Task] =
    Seq(BeautifyTask, DateCountdownTask, BuildFailureReportTask, SortTableTask, QueryTask)

  override lazy val Doc: String = "A driver for the various markdown utilities.\n\n" + SimpleDoc

  /** A helper method to process a list of files supplied on the command line.
    * @param files
    *   Files and directories relative to the current directory as string arguments. Markdown files are discovered
    *   recursively in the directory.
    * @param fn
    *   A method to call on each discovered file.
    */
  def processMd(files: Iterable[String])(fn: File => Unit): Unit = files
    .map(Path(_).toAbsolute)
    .flatMap {
      case f: File if f.exists => Some(f)
      case d: Directory if d.exists =>
        d.walkFilter(p => p.isDirectory || """.*\.md$""".r.findFirstIn(p.name).isDefined).map(_.toFile)
      case p => throw new InternalDocoptException(s"The file ${p.name} doesn't exist.")
    }
    .foreach(fn)
}
