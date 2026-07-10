package com.skraba.byexample.scalatools.gitchecker

import com.tinfoiled.docopt4s.FsPath

import java.nio.file.Path
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
import scala.collection.mutable
import scala.sys.process.{Process, ProcessLogger}

/** Access to the git command line. */
object Git {

  case class GitException(
      exitCode: Int,
      stdout: String,
      stderr: String,
      repo: Path,
      args: Seq[String],
      extraEnv: (String, String)*
  ) extends Exception() {
    lazy val debug: String =
      s"""-------------------------------------------------------------------
         |GitException (exit code: $exitCode, repo: $repo)
         |args: ${args}
         |extraEnv:
         |${extraEnv.map { case (k, v) => s"$k=$v" }.mkString("\n")}
         |stdout ------------------------------------------------------------
         |$stdout
         |stderr ------------------------------------------------------------
         |$stderr
         |-------------------------------------------------------------------
         """.stripMargin
  }

  /** Runs git as a process with arguments, erroring if the exist code is non-zero
    *
    * @param args
    *   The git arguments ("add", ".")
    * @param repo
    *   The directory of the repository to run in
    * @return
    *   The output of the command
    */
  @throws[GitException]
  def apply(repo: Path, args: String*): String = Git(repo, args)

  /** Runs git as a process with arguments and environment, erroring if the exist code is non-zero
    *
    * @param repo
    *   The directory of the repository to run in
    * @param args
    *   The git arguments ("add", ".")
    * @param extraEnv
    *   Environment variables to add to the process
    * @return
    *   The output of the command
    */
  @throws[GitException]
  def apply(repo: Path, args: Seq[String], extraEnv: (String, String)*): String = {
    val stdout = mutable.ListBuffer[String]()
    val stderr = mutable.ListBuffer[String]()
    val logger = ProcessLogger(stdout += _, stderr += _)
    val exitCode = Process("git" +: args, repo.toFile, extraEnv: _*).!(logger)

    if (exitCode != 0) {
      val ex = GitException(exitCode, stdout.mkString("\n"), stderr.mkString("\n"), repo, args, extraEnv: _*)
      Console.err.println(ex.debug)
      throw ex
    }
    stdout.mkString("\n")
  }

  /** Contains information about one git commit
    *
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
      body: String,
      marker: String = ""
  ) {
    def isBump: Boolean = subject.startsWith("Bump")

    def isLeft: Boolean = marker == "<"
  }

  object Commit {
    // Build a format string (see https://git-scm.com/docs/git-log#_pretty_formats)
    // In the order of the commit class, noting that the marker might appear at the start of the line.
    val LogFormat: String =
      Seq("H", "an", "ae", "at", "cn", "ce", "ct", "d", "s", "B").mkString(
        "--pretty=format:%",
        "%x00%",
        "%x00"
      )

    /** @param in
      *   The stdout from the git logs with the log format
      * @return
      *   The stdout parsed into a commit instance.
      */
    def fromGitStdout(in: String): Seq[Commit] = in
      .split("\u0000")
      .grouped(10)
      .map(_.mkString("\u0000").trim)
      .toSeq
      .map(Commit(_))

    // From a one line string given the log format, create a commit instance
    def apply(in: String): Commit = {
      val tokens = in.split("\u0000")
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
        body = tokens(9),
        marker = marker.trim
      )
    }

    /** Get the date for the given commit from the repo.
      * @param commit
      *   The commit hash to look up (or a committish object).
      * @param repo
      *   The directory of the repo
      * @return
      *   The date of that commit
      */
    def getDateFromRepo(commit: String = "HEAD", repo: Path = FsPath.Pwd): LocalDateTime =
      LocalDateTime.parse(
        Git(repo, "log", "-1", commit, "--pretty=format:%aI"),
        DateTimeFormatter.ISO_OFFSET_DATE_TIME
      )
  }

}
