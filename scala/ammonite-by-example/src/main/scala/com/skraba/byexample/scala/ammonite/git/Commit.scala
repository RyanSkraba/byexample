package com.skraba.byexample.scala.ammonite.git

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

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
  def fromGit(in: String): Seq[Commit] = in
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
  def getDateFromRepo(
      commit: String = "HEAD",
      repo: os.Path = os.pwd
  ): LocalDateTime = LocalDateTime.parse(
    os.proc("git", "log", "-1", commit, "--pretty=format:%aI")
      .call(repo)
      .out
      .lines
      .last
      .trim,
    DateTimeFormatter.ISO_OFFSET_DATE_TIME
  )
}
