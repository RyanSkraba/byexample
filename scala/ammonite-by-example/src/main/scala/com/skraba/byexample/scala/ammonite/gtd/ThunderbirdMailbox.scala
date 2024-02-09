package com.skraba.byexample.scala.ammonite.gtd

import scala.io.Source
import scala.reflect.io._
import scala.util.Properties

object ThunderbirdMailbox {

  /** A helper method to find the number of unread messages in a Thunderbird mailbox (.msf) file.
    *
    * @param dir
    *   The root directory to search for mailbox files.
    * @param filename
    *   A strictly matching filename to look for, probably with an .msf extension.
    * @param refiner
    *   To disambiguate between multiple mailbox files, matches the entire path.
    * @return
    *   The number of unread messages detected from that mailbox file.
    */
  def getNumberOfMessagesFromMailbox(
      dir: String,
      filename: String,
      refiner: String
  ): Int = {
    // Find the requested mailbox file or fail immediately
    val f: File = (Directory(Properties.userHome) / dir)
      .walkFilter(p => p.isDirectory || p.name == filename && p.path.contains(refiner))
      .filter(_.isFile) match {
      case fs if fs.hasNext =>
        val first = fs.next().toFile
        if (fs.hasNext)
          throw new RuntimeException(s"More than one $filename found in $dir")
        first
      case _ => throw new RuntimeException(s"$filename can't be found in $dir")
    }

    // Find the tag used in the mailbox to indicate new messages.
    val tagFinderRegex = """([A-Z0-9]+)=numNewMsgs""".r
    val tag = Streamable.closing(Source.fromFile(f.jfile)) {
      _.getLines()
        .flatMap(line => tagFinderRegex.findFirstMatchIn(line).map(_.group(1)))
        .find(_ => true)
        .getOrElse(
          throw new RuntimeException(
            s"Can't find the 'numNewMsgs' tag in $filename"
          )
        )
    }

    // And the most recent count corresponding to that tag.
    val msgFinderRegex = s"""\\(\\^$tag=(\\w+)\\)""".r
    val numNewMsgs = Streamable.closing(Source.fromFile(f.jfile)) {
      _.getLines().toSeq.reverse
        .flatMap(line => msgFinderRegex.findFirstMatchIn(line).map(_.group(1)))
        .find(_ => true)
        .getOrElse(
          throw new RuntimeException(
            s"Can't find number of messages for 'numNewMsgs' tag ($tag) in $filename"
          )
        )
    }

    Integer.parseInt(numNewMsgs, 16)
  }
}
