#!/usr/bin/env amm

/** A user script for renaming files that are close together in time.  This is useful for
  * photos that were taken in close proximity.
  *
  * This assumes that https://ammonite.io/ is installed.
  *
  * Ammonite includes:
  * - requests (https://github.com/lihaoyi/requests-scala)
  * - upickle (https://github.com/lihaoyi/upickle)
  */

import ammonite.ops._
import mainargs.{arg, main}

import scala.concurrent.duration.DurationInt
import scala.io.AnsiColor._

val DefaultTimeGap = 30.seconds

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  val cmd = s"${GREEN}file_renamer$RESET"
  println(s"""$BOLD$cmd - Let's clean up some files!
             |
             |  $CYAN     group$RESET : Rename files grouped by time.
             |
             |Usage:
             |
             | $cmd ${CYAN}group$RESET /run/media/$$USER/MyDisk/ToSort
             |""".stripMargin)
}

@arg(doc = "Perform the renaming action")
@main
def group(
    dir: Option[Path] = None,
    gap: Long = DefaultTimeGap.toMillis
): Unit = {
  // The source path to analyse
  val src: Path = dir.getOrElse(pwd)
  if (!(exists ! src)) {
    println(s"$RED${BOLD}ERROR:$RESET $src does not exist.")
    System.exit(1)
  }

  // All of the files and their creation dates.
  val files: List[Path] = (ls ! src).toList.sortBy(_.mtime)

  // A fold function that groups elements that are in the same "window" (i.e. separated by less
  // than the gap).
  val foldGroupByTime: (List[List[Path]], Path) => List[List[Path]] =
    (acc, file) =>
      acc match {
        // If there is already a head group within the gap, then add this one before in the same group.
        case headGroup :: rest
            if (file.mtime.toMillis - headGroup.head.mtime.toMillis) < gap =>
          (file :: headGroup) :: rest
        // If there isn't a list, or the head element is outside the gap then add it as a new group
        case rest => List(file) :: rest
      }

  // And the grouped files.
  val groupedByTime: List[List[Path]] =
    files.foldLeft[List[List[Path]]](Nil)(foldGroupByTime).reverse.map(_.reverse)

  // This just asks for a name and prints out the mv command.
  val commands: Seq[String] = groupedByTime.flatMap { files =>

    // Print to the screen
    println(s"$RED${BOLD}========================================$RESET")
    println(s"$RED${BOLD}Group ${files.head.mtime}$RESET")
    files.foreach { f=> println(s"  ${BOLD}${f.mtime}:$RESET ${f.last}")}

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
