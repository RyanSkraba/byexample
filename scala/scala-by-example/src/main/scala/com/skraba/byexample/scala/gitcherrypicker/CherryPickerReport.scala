package com.skraba.byexample.scala.gitcherrypicker

import com.skraba.byexample.scala.markd.{
  Align,
  Header,
  LinkRef,
  Table,
  TableRow
}
import os.CommandResult

import scala.util.{Success, Try}

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

  private[this] def xxx(h1: Header, cmts: Seq[Commit]) =
    h1.mapFirstIn(ifNotFound =
      Seq(
        Table.from(
          Seq.fill(3)(Align.LEFT),
          TableRow.from("Commit", "Subject", "Notes")
        )
      )
    ) {
      case tbl: Table if tbl.title.startsWith("Commit") =>
        cmts.foldLeft(tbl)((acc, cmt) =>
          acc.mapFirstIn(ifNotFound =
            TableRow.from(s"[${cmt.commit}]", cmt.subject, "")
          ) {
            case row if row.head.contains(cmt.commit) =>
              TableRow.from(
                s"[${cmt.commit}]",
                cmt.subject,
                row.cells.applyOrElse(2, (_: Int) => "")
              )
          }
        )
    }

  def toDoc(): Header = {
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
        case h1: Header if h1.title == "Left" => xxx(h1, left)
      }
      .mapFirstIn(ifNotFound = Header(1, "Right")) {
        case h1: Header if h1.title == "Right" => xxx(h1, right)
      }
      .mapFirstIn(ifNotFound = Header(1, "References")) {
        case h1: Header if h1.title == "References" =>
          val linkRefs: Seq[LinkRef] = (left ++ right).map(cmt =>
            LinkRef(
              cmt.commit,
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
