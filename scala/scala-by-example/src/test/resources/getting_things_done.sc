#!/usr/bin/env amm

/** A user script for interacting with my Apache status sheet.
  *
  * This assumes that https://ammonite.io/ is installed.
  *
  * Ammonite includes:
  * - requests (https://github.com/lihaoyi/requests-scala)
  * - upickle (https://github.com/lihaoyi/upickle)
  */

import ammonite.ops.{home, _}
import mainargs.{arg, main}

import scala.io.AnsiColor._
import scala.util.matching.Regex

interp.repositories() ++= {
  // Get the local Maven repository.
  val localM2: Path = Option(sys.props("maven.repo.local"))
    .map(Path(_))
    .getOrElse(home / ".m2" / "repository")

  Seq(coursierapi.MavenRepository.of(localM2.toIO.toURI.toString))
}

@
import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`
import com.skraba.byexample.scala.markd._

/** A tag used to distinguish between documents. */
val StatusTag: String = sys.env.getOrElse("GTD_TAG", "GTD")

/** Git root directory for the status file. */
val StatusRepo: Path = sys.env
  .get(s"${StatusTag}_STATUS_REPO")
  .map(Path(_))
  .getOrElse(home / "Documents")

/** The actual status file to update. */
val StatusFile: Path = sys.env
  .get(s"${StatusTag}_STATUS_FILE")
  .map(Path(_))
  .getOrElse(StatusRepo / "todo" / "status.md")

/** The header with the weekly statuses. */
val H1Weekly: String = "Weekly Status"

/** Propose a git commit message for the status page
  * @param msg The git message to propose
  * @return A string to copy and paste to check the changes.
  */
def proposeGit(msg: String): String = {
  s"""${GREEN}Commit:$RESET
     |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
     |      git -C $StatusRepo difftool --staged
     |  git -C $StatusRepo commit -m $BOLD"$msg"$RESET
     |""".stripMargin
}

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  val cmd = s"${GREEN}getting_things_done$RESET"
  println(s"""$BOLD$cmd - Let's get things done!
             |
             |  $CYAN     clean$RESET : Rewrite the status document
             |  $CYAN  nextWeek$RESET : Add a new week to the status document ${RED_B}TODO$RESET
             |  $CYAN        pr$RESET : Add a PR review to this week ${RED_B}TODO$RESET
             |  $CYAN      stat$RESET : Add a statistic to the document ${RED_B}TODO$RESET
             |  $CYAN      week$RESET : Print the last week status or a specific week ${RED_B}TODO$RESET
             |
             |Usage:
             |
             | $cmd ${CYAN}clean$RESET
             | $cmd ${CYAN}nextWeek$RESET
             | $cmd ${CYAN}pr$RESET avro 9876 1234 "Implemented a thing" REVIEWED
             | $cmd ${CYAN}stat$RESET avro-unread 448
             | $cmd ${CYAN}week$RESET
             | $cmd ${CYAN}week$RESET 2021/03/08
             |""".stripMargin)
}

@arg(doc = "Clean the existing document")
@main
def clean(): Unit = {
  // Read and overwrite the existing document without making any changes.
  val doc = Header.parse(read ! StatusFile)
  write.over(StatusFile, doc.build().toString.trim())
  println(proposeGit(s"feat(status): Beautify the document"))
}
