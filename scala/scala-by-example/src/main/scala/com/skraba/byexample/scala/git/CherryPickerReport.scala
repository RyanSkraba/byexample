package com.skraba.byexample.scala.git

import com.skraba.byexample.scala.markd.{
  Align,
  Header,
  LinkRef,
  Table,
  TableRow
}
import os.CommandResult

import scala.util.{Success, Try}

import CherryPickerReport._

case class CherryPickerReport(
    lBranch: String,
    rBranch: String,
    left: Seq[Commit],
    right: Seq[Commit],
    doc: Header = Header(0, ""),
    cfg: ColourCfg = AnsiColourCfg
) {

  /** The left branch name with colour and indicator. */
  lazy val lTag: String = cfg.left(s"$lBranch (LEFT)")

  /** The right branch name with colour and indicator. */
  lazy val rTag: String = cfg.right(s"$rBranch (RIGHT)")

  lazy val leftSubjects: Map[String, Seq[Commit]] = left.groupBy(_.subject)
  lazy val rightSubjects: Map[String, Seq[Commit]] = right.groupBy(_.subject)

  /** Rerun this report using the updated commits. */
  def update(current: CherryPickerReport): CherryPickerReport =
    copy(left = current.left, right = current.right)

  /** Generate a commit table for the given header, integrating the given list
    * of commits with any existing notes.
    */
  private[this] def addTableOfCommits(
      h1: Header,
      cmts: Seq[Commit],
      otherSubjects: Map[String, Seq[Commit]]
  ): Header = {

    // Create a clean commit table based on the current information, the full commit hash and subject
    val clean: Table = Table(
      Seq.fill(3)(Align.LEFT),
      TableRow.from("Commit", "Subject", "Notes") +: cmts.map(cmt =>
        TableRow.from(
          cmt.commit,
          if (cmt.subject.length < 100) cmt.subject
          else cmt.subject.take(97) + "...",
          ""
        )
      )
    )

    // Update the actual table in two steps, first by replacing it with the "clean one" above, but copying the notes over).
    val h1WithCleaned = h1.mapFirstIn(ifNotFound = clean) {
      case tbl: Table if tbl == clean => clean
      case tbl: Table if tbl.title == "Commit" =>
        clean.copy(mds = clean.mds.map { row =>
          val cmtHash = row.head
          val originalRow = tbl
            .collectFirst { case r: TableRow if refersTo(cmtHash, r.head) => r }
            .getOrElse(TableRow.from())
          row.updated(2, originalRow(2))
        })
    }

    // And in the second step update the rows with additional interesting information
    h1WithCleaned.mapFirstIn() {
      // In the second stepd
      case tbl: Table if tbl.title == "Commit" =>
        tbl.copy(mds = tbl.mds.map {
          case headerRow if headerRow == tbl(0) => headerRow
          case r =>
            val commit = cmts.find(_.commit == r.head).get

            // Add the word Bump in the text if it isn't already noted.
            val r2 =
              if (commit.isBump && !r(2).toLowerCase.contains("bump"))
                r.updated(2, "Bump " + r(2))
              else r

            // If this is a detected cherry pick, add it as a reference.
            val r3 = otherSubjects
              .get(commit.subject)
              .flatMap(_.headOption)
              .map(otherCommit =>
                if (refersTo(otherCommit.commit, r2(2))) r2
                else
                  r2.updated(
                    2,
                    s" [${otherCommit.commit.take(CommitHashSize)}]"
                  )
              )
              .getOrElse(r2)

            // Set the head as a reference and trim the subject if desired.
            r3.updated(0, s"[${r.head.take(CommitHashSize)}]")
              .updated(
                1,
                if (r(1).length < 100) r(1) else r(1).take(97) + "..."
              )
        })
    }
  }

  def toDoc: Header = {
    doc
      .mapFirstIn(ifNotFound = Header(1, "CherryPickerReport")) {
        case h1: Header if h1.title == "CherryPickerReport" =>
          h1.mapFirstIn(ifNotFound =
            // TODO Table.from without alignments
            Seq(
              Table
                .from(Seq.fill(2)(Align.LEFT), TableRow.from("Branch info", ""))
            )
          ) {
            case tbl: Table if tbl.title.startsWith("Branch info") =>
              tbl
                .mapFirstIn(ifNotFound = TableRow.from("Left", "")) {
                  case row if row.head == "Left" =>
                    row.copy(cells = row.cells.updated(1, lBranch))
                }
                .mapFirstIn(ifNotFound = TableRow.from("Left N", "")) {
                  case row if row.head == "Left N" =>
                    row.copy(cells = row.cells.updated(1, left.size.toString))
                }
                .mapFirstIn(ifNotFound = TableRow.from("Left Dups", "")) {
                  case row if row.head == "Left Dups" =>
                    row.copy(cells =
                      row.cells.updated(
                        1,
                        leftSubjects.count(p => p._2.size > 1).toString
                      )
                    )
                }
                .mapFirstIn(ifNotFound = TableRow.from("Right", "")) {
                  case row if row.head == "Right" =>
                    row.copy(cells = row.cells.updated(1, rBranch))
                }
                .mapFirstIn(ifNotFound = TableRow.from("Right N", "")) {
                  case row if row.head == "Right N" =>
                    row.copy(cells = row.cells.updated(1, right.size.toString))
                }
                .mapFirstIn(ifNotFound = TableRow.from("Right Dups", "")) {
                  case row if row.head == "Right Dups" =>
                    row.copy(cells =
                      row.cells.updated(
                        1,
                        rightSubjects.count(p => p._2.size > 1).toString
                      )
                    )
                }
          }
      }
      .mapFirstIn(ifNotFound = Header(1, "Left")) {
        case h1: Header if h1.title == "Left" =>
          addTableOfCommits(h1, left, rightSubjects)
      }
      .mapFirstIn(ifNotFound = Header(1, "Right")) {
        case h1: Header if h1.title == "Right" =>
          addTableOfCommits(h1, right, leftSubjects)
      }
      .mapFirstIn(ifNotFound = Header(1, "References")) {
        case h1: Header if h1.title == "References" =>
          val linkRefs: Seq[LinkRef] = (left ++ right).map(cmt =>
            LinkRef(
              cmt.commit.take(CommitHashSize),
              s"https://github.com/apache/avro/commit/${cmt.commit}",
              cmt.subject
            )
          )
          h1.copy(mds = h1.mds ++ linkRefs)
      }
  }

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

  val CommitHashSize = 10

  val MinCommitHashSize = 7

  private def refersTo(hash: String, text: String): Boolean = {
    for (i <- 7 to hash.length)
      if (text.contains(s"[${hash.take(i)}]")) return true;
    false
  }

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
        val (left, right) =
          Commit.fromGit(result.out.text).partition(_.isLeft)
        Left(CherryPickerReport(lTag, rTag, left, right))
      case other => Right(other)
    }
  }

  /** Extract the information from the report from the markdown document.
    *
    * @param doc
    *   A markdown document containing the cherry-pick report
    * @return
    */
  def fromDoc(doc: Header): CherryPickerReport = {

    val (lBranch, rBranch) = doc
      .collectFirst {
        case tbl @ Table(_, rows) if tbl.title.startsWith("Branch info") =>
          (
            tbl.mds.find(_.head == "Left").map(_.cells(1)).getOrElse(""),
            tbl.mds.find(_.head == "Right").map(_.cells(1)).getOrElse("")
          )
      }
      .getOrElse(("", ""))

    CherryPickerReport(lBranch, rBranch, Seq.empty, Seq.empty, doc = doc)
  }
}
