package com.skraba.byexample.scalatools.gtd

import com.tinfoiled.docopt4s.FsPath
import com.tinfoiled.docopt4s.FsPath.RichPath

import java.nio.file.{Files, Path}
import scala.io.Source
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.jdk.StreamConverters.StreamHasToScala
import scala.reflect.io.Streamable

object ThunderbirdMailbox {

  /** A helper method to find the number of unread messages in a Thunderbird mailbox (.msf) file.
    *
    * @param dir
    *   The root directory to search for mailbox files.
    * @param filename
    *   A strictly matching filename to look for, probably with an .msf extension.
    * @param refiner
    *   If present, used to disambiguate between multiple mailbox files by checking that the path contains this string
    * @return
    *   The number of unread messages detected from that mailbox file.
    */
  def getNumberOfMessagesFromMailbox(
      dir: Path,
      filename: String,
      refiner: String = ""
  ): Int = {
    // Find the requested mailbox file or fail immediately
    val f: Path = Files
      .walk(FsPath.Home / dir)
      .toScala(LazyList)
      .filter((p: Path) => p.isFile && p.name == filename && p.toString.contains(refiner)) match {
      case Seq()      => throw new RuntimeException(s"'$filename' can't be found in $dir")
      case Seq(first) => first
      case fs =>
        throw new RuntimeException(
          s"More than one '$filename' found in $dir:\n  ${fs.sortBy(_.toString).mkString("\n  ")}"
        )
    }

    // Find the tag used in the mailbox to indicate new messages.
    val tagFinderRegex = """([A-Z0-9]+)=numNewMsgs""".r
    val tag = f.lines
      .flatMap(line => tagFinderRegex.findFirstMatchIn(line).map(_.group(1)))
      .nextOption()
      .getOrElse(throw new RuntimeException(s"Can't find the 'numNewMsgs' tag in $filename"))

    // And the most recent count corresponding to that tag.
    val msgFinderRegex = s"""\\(\\^$tag=(\\w+)\\)""".r
    val numNewMsgs = f.lines.toSeq.reverse
      .flatMap(line => msgFinderRegex.findFirstMatchIn(line).map(_.group(1)))
      .headOption
      .getOrElse(
        throw new RuntimeException(
          s"Can't find number of messages for 'numNewMsgs' tag ($tag) in $filename"
        )
      )

    Integer.parseInt(numNewMsgs, 16)
  }
}
