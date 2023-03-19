#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import mainargs.{Flag, arg, main}
import os.CommandResult

import scala.util._

// ==========================================================================
// This is how you would add artifacts coming from the local maven repository

interp.repositories() ++= {
  // Get the local Maven repository.
  val localM2: os.Path = Option(sys.props("maven.repo.local"))
    .map(os.Path(_))
    .getOrElse(os.home / ".m2" / "repository")

  Seq(coursierapi.MavenRepository.of(localM2.toIO.toURI.toString))
}

@
import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

class ColourCfg {
  val Black: String = ""
  val Red: String = ""
  val Green: String = ""
  val Yellow: String = ""
  val Blue: String = ""
  val Magenta: String = ""
  val Cyan: String = ""
  val White: String = ""

  val BlackB: String = ""
  val RedB: String = ""
  val GreenB: String = ""
  val YellowB: String = ""
  val BlueB: String = ""
  val MagentaB: String = ""
  val CyanB: String = ""
  val WhiteB: String = ""

  val Bold: String = ""
  val Reset: String = ""

  def style(
      in: Any,
      colour: String = White,
      reset: Boolean = true,
      bold: Boolean = false
  ): String = s"${if (bold) Bold else ""}$colour$in${if (reset) Reset else ""}"

  def black(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Black, reset, bold)
  def red(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Red, reset, bold)
  def green(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Green, reset, bold)
  def yellow(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Yellow, reset, bold)
  def blue(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Blue, reset, bold)
  def magenta(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Magenta, reset, bold)
  def cyan(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Cyan, reset, bold)
  def white(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, White, reset, bold)

  def bold(in: Any, reset: Boolean = true): String = style(in, "", reset, true)

  def ok(in: Any): String = green(in)
  def warn(in: Any): String = yellow(in)
  def error(in: Any): String = red(in)
  def left(in: Any): String = cyan(in)
  def right(in: Any): String = magenta(in)
  def kv(key: Any, value: Any) = magenta(key) + " : " + value
}

object AnsiColourCfg extends ColourCfg {

  import scala.io.AnsiColor
  override val Black: String = AnsiColor.BLACK
  override val Red: String = AnsiColor.RED
  override val Green: String = AnsiColor.GREEN
  override val Yellow: String = AnsiColor.YELLOW
  override val Blue: String = AnsiColor.BLUE
  override val Magenta: String = AnsiColor.MAGENTA
  override val Cyan: String = AnsiColor.CYAN
  override val White: String = AnsiColor.WHITE

  override val BlackB: String = AnsiColor.BLACK_B
  override val RedB: String = AnsiColor.RED_B
  override val GreenB: String = AnsiColor.GREEN_B
  override val YellowB: String = AnsiColor.YELLOW_B
  override val BlueB: String = AnsiColor.BLUE_B
  override val MagentaB: String = AnsiColor.MAGENTA_B
  override val CyanB: String = AnsiColor.CYAN_B
  override val WhiteB: String = AnsiColor.WHITE_B

  override val Bold: String = AnsiColor.BOLD
  override val Reset: String = AnsiColor.RESET

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
  lazy val rTag = cfg.right(s"$rBranch (RIGHT)")

  lazy val leftSubjects = left.groupBy(_.subject)
  lazy val rightSubjects = right.groupBy(_.subject)

  /** Rerun this report using the updated commits. */
  def update(current: CherryPickerReport): CherryPickerReport =
    copy(left = current.left, right = current.right)

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
    println(cfg.bold("Summary:"))
    println(s"* There are ${cfg.ok(left.size)} on $lTag")
    println(s"* There are ${cfg.ok(right.size)} on $rTag")

    print(summarizeSubjects(left, leftSubjects, lTag))
    print(summarizeSubjects(right, rightSubjects, rTag))

    val actuallyCherryPicked =
      leftSubjects.keySet.intersect(rightSubjects.keySet)
    println(
      s"* There were ${cfg.warn(actuallyCherryPicked.size)} commits that look cherrypicked:"
    )
    print(actuallyCherryPicked.map(msg => s"  - $msg\n").mkString)

    println(s"\n# $lTag\n")
    left.foreach {
      case cmt if actuallyCherryPicked(cmt.subject) =>
        println(cfg.ok(cmt))
      case cmt => println(cmt)
    }

    println(s"\n# $rTag\n")
    right.foreach {
      case cmt if actuallyCherryPicked(cmt.subject) =>
        println(cfg.ok(cmt))
      case cmt => println(cmt)
    }
  }
}

object CherryPickerReport {

  val Cmd: Seq[String] = Seq(
    "git",
    "--no-pager",
    "log",
    "--left-right",
    "--graph",
    "--cherry-pick",
    Commit.LogFormat
  )

  /** Given a git repo and two branchs (left and right), produces a report of
    * how the branches have diverged.
    *
    * @param repo
    *   The root directory of the git repo on the local disk.
    * @param lTag
    *   The name of the left banch (usually main)
    * @param rTag
    *   The name of the right branch (usually branch-1.x)
    * @return
    *   The [[CherryPickerReport]] if successful, or the git command output if
    *   it failed.
    */
  def fromGit(
      repo: String,
      lTag: String,
      rTag: String
  ): Either[CherryPickerReport, Try[CommandResult]] = {
    Try(
      os.proc(Cmd :+ s"$lTag...$rTag").call(os.Path(repo))
    ) match {
      case Success(result) if result.exitCode == 0 =>
        // result.out.lines.foreach(println)
        val (left, right) = result.out.lines.map(Commit(_)).partition(_.isLeft)
        Left(CherryPickerReport(lTag, rTag, left, right))
      case other => Right(other)
    }
  }

  /** Extract the information from the report from the markdown document.
    * @param report
    *   A markdown document containing the cherry-pick report
    * @return
    */
  def fromDoc(report: Header): CherryPickerReport = {
    // TODO: Fill in this stub
    CherryPickerReport("", "", Seq.empty, Seq.empty)
  }

}

@arg(doc = "Create a cherry-picking report between two branches")
@main
def cherryPick(
    repo: String,
    lTag: String = "main",
    rTag: String = "branch",
    statusDoc: Option[os.Path] = None,
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {

  val cfg = AnsiColourCfg
  if (verbose.value) {
    println(cfg.bold("Arguments:"))
    println(cfg.kv("      repo", repo))
    println(cfg.kv("      lTag", lTag))
    println(cfg.kv("      rTag", rTag))
    println(cfg.kv(" statusDoc", statusDoc))
    println(cfg.bold("Git command:"))
    println((CherryPickerReport.Cmd :+ s"$lTag...$rTag").mkString(" "))
  }

  // The current state of the git branches
  val current: CherryPickerReport =
    CherryPickerReport.fromGit(repo, lTag, rTag) match {
      case Left(status) => status
      case Right(Success(result)) =>
        println(s"Unsuccessful ${result.exitCode}")
        sys.exit(result.exitCode)
      case Right(Failure(ex)) =>
        println("Failure!")
        ex.printStackTrace()
        sys.exit(1)
    }

  // If there's a status document, merge it with the current state
  val updated = statusDoc match {
    case Some(path) if os.exists(path) =>
      CherryPickerReport.fromDoc(Header.parse(os.read(path))).update(current)
    case _ => current
  }

  statusDoc match {
    case Some(path) => os.write.over(path, "Hello")
    case None       => updated.summarize()
  }

}
