#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import mainargs.{arg, main}
import os.CommandResult

import scala.io.AnsiColor._
import scala.util._

// ==========================================================================
// Top level variables available to the script

class ColourCfg {
  def ok(in: Any): String = in.toString
  def warn(in: Any): String = in.toString
  def error(in: Any): String = in.toString
  def bold(in: Any): String = in.toString
  def left(in: Any): String = in.toString
  def right(in: Any): String = in.toString
}

object AnsiColourCfg extends ColourCfg {
  override def ok(in: Any): String = s"$GREEN$in$RESET"
  override def warn(in: Any): String = s"$YELLOW$in$RESET"
  override def error(in: Any): String = s"$RED$in$RESET"
  override def bold(in: Any): String = s"$BOLD$in$RESET"
  override def left(in: Any): String = s"$CYAN$in$RESET"
  override def right(in: Any): String = s"$MAGENTA$in$RESET"
}

val Cli = AnsiColourCfg.ok("git_checker.sc")

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  val cfg = AnsiColourCfg
  println(s"""${cfg.bold(Cli)} - Do some analysis on git repositories.
             |
             | ${cfg.left("cherryPick")} : Get a status report on two branches
             |
             |Usage:
             |
             | $Cli ${cfg.left("cherryPick")} [repo] [main] [branch]
             |""".stripMargin)
}

// ==========================================================================
// Cherry pick report

/** Contains information about one git commit
  * @param commit
  *   The commit hash
  * @param authorName
  *   The author name
  * @param authorEmail
  *   The author name
  * @param authorDate
  *   The author date
  * @param committerName
  *   The committer name
  * @param committerEmail
  *   The committer name
  * @param committerDate
  *   The committer date
  * @param refNames
  *   Reference names
  * @param subject
  *   The first line of the commit body
  * @param marker
  *   A marker to indicate where the commit came from (if doing --left-right).
  */
case class Commit(
    commit: String,
    authorName: String,
    authorEmail: String,
    authorDate: String,
    committerName: String,
    committerEmail: String,
    committerDate: String,
    refNames: String,
    subject: String,
    marker: String = ""
) {
  def isLeft: Boolean = marker == "<"
}

object Commit {
  // Build a format string (see https://git-scm.com/docs/git-log#_pretty_formats)
  // In the order of the commit class, noting that the marker might appear at the start of the line.
  val LogFormat =
    Seq("H", "an", "ae", "at", "cn", "ce", "ct", "d", "s").mkString(
      "--pretty=format:%",
      "%x09%",
      ""
    )

  // From a one line string given the log format, create a commit instance
  def apply(in: String): Commit = {
    val tokens = in.split("\t")
    val (marker, commit) = tokens.head.span(!_.isLetterOrDigit)
    Commit(
      commit = commit,
      authorName = tokens(1),
      authorEmail = tokens(2),
      authorDate = tokens(3),
      committerName = tokens(4),
      committerEmail = tokens(5),
      committerDate = tokens(6),
      refNames = tokens(7),
      subject = tokens(8),
      marker = marker.trim
    )
  }
}

case class CherryPickerReport(
    lBranch: String,
    rBranch: String,
    left: Seq[Commit],
    right: Seq[Commit],
    cfg: ColourCfg = AnsiColourCfg
) {

  /** The left branch name with colour and indicator. */
  lazy val lTag = cfg.left(s"$lBranch (LEFT)")

  /** The right branch name with colour and indicator. */
  lazy val RTag = cfg.right(s"$lBranch (RIGHT)")

  lazy val leftSubjects = left.groupBy(_.subject)
  lazy val rightSubjects = right.groupBy(_.subject)

  private[this] def summarizeSubjects(
      commits: Seq[Commit],
      s: Map[String, Seq[Commit]],
      tag: String
  ): String = {
    if (s.size != commits.size) {
      s"* Found duplicate subjects on $tag (${cfg.warn(s.size)} unique subjects)\n" +
        s.map {
          case (subject, commits) if commits.size > 1 =>
            s"  - $subject (${commits.size})\n"
          case _ => ""
        }.mkString
    } else {
      ""
    }
  }

  def summarize(): Unit = {
    println()
    println()
    println(cfg.bold("Summary"))
    println(s"* There are ${cfg.ok(left.size)} on $lTag")
    println(s"* There are ${cfg.ok(right.size)} on $RTag")

    print(summarizeSubjects(left, leftSubjects, lTag))
    print(summarizeSubjects(right, rightSubjects, RTag))

    val actuallyCherryPicked =
      leftSubjects.keySet.intersect(rightSubjects.keySet)
    println(
      s"* There were ${cfg.warn(actuallyCherryPicked.size)} commits that look cherrypicked:"
    )
    print(actuallyCherryPicked.map(msg => s"  - $msg\n").mkString)

    println(s"\n# $lBranch (LEFT)\n")

    left.foreach {
      case cmt if actuallyCherryPicked(cmt.subject) =>
        println(cfg.ok(cmt))
      case cmt => println(cmt)
    }
  }
}

object CherryPickerReport {
  /**
   * Given a git repo and two branchs (left and right), produces a report of how the branches have diverged.
   *
   * @param repo The root directory of the git repo on the local disk.
   * @param lTag The name of the left banch (usually main)
   * @param rTag The name of the right branch (usually branch-1.x)
   * @return The [[CherryPickerReport]] if successful, or the git command output if it failed.
   */
  def fromGit(
      repo: String,
      lTag: String,
      rTag: String
  ): Either[CherryPickerReport, Try[CommandResult]] = {
    Try(
      os.proc(
        "git",
        "--no-pager",
        "log",
        "--left-right",
        "--graph",
        "--cherry-pick",
        Commit.LogFormat,
        s"$lTag...$rTag"
      ).call(os.Path(repo))
    ) match {
      case Success(result) if result.exitCode == 0 =>
        // result.out.lines.foreach(println)
        val (left, right) = result.out.lines.map(Commit(_)).partition(_.isLeft)
        Left(CherryPickerReport(lTag, rTag, left, right))
      case other => Right(other)
    }
  }
}

@arg(doc = "Make a system call")
@main
def cherryPick(
    repo: String,
    lTag: String = "main",
    rTag: String = "branch",
): Unit = {
  CherryPickerReport.fromGit(repo, lTag, rTag) match {
    case Left(status) =>
      status.summarize()
    case Right(Success(result)) =>
      println(s"Unsuccessful ${result.exitCode}")
    case Right(Failure(ex)) =>
      println("Failure!")
      ex.printStackTrace()
  }
}
