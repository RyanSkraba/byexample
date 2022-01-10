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

import java.time.LocalDate
import scala.io.AnsiColor._

interp.repositories() ++= {
  // Get the local Maven repository.
  val localM2: Path = Option(sys.props("maven.repo.local"))
    .map(Path(_))
    .getOrElse(home / ".m2" / "repository")

  Seq(coursierapi.MavenRepository.of(localM2.toIO.toURI.toString))
}

@
// Intellij always removes the following line, which should be
// import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`

import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`
import com.skraba.byexample.scala.markd._
import com.skraba.byexample.scala.markd.GettingThingsDone._

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

/** The list of apache projects. */
val Projects = Set("avro", "beam", "flink", "parquet", "pulsar", "spark")

/** Propose a git commit message for the status page
  * @param msg The git message to propose
  * @return A string to copy and paste to check the changes.
  */
private def proposeGit(msg: String): String = {
  s"""${GREEN}Commit:$RESET
     |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
     |      git -C $StatusRepo difftool --staged
     |  git -C $StatusRepo commit -m $BOLD"$msg"$RESET
     |""".stripMargin
}

object ProjectParserCfg extends ParserCfg {

  /** Clean up the references at the end of a section. */
  override def linkCleaner(links: Seq[LinkRef]) = {
    // Clean the links.
    links
      .map {
        case LinkRef(LinkRef.JiraLinkRefRegex(prj, num), None, title)
            if Projects(prj.toLowerCase) =>
          (
            f"${prj.toUpperCase}-$num%9s",
            LinkRef(
              s"${prj.toUpperCase}-$num",
              Some(
                s"https://issues.apache.org/jira/browse/${prj.toUpperCase}-$num"
              ),
              title
            )
          )
        case l @ LinkRef(LinkRef.JiraLinkRefRegex(prj, num), _, _) =>
          (f"${prj.toUpperCase}-$num%9s", l)
        case LinkRef(LinkRef.GithubPrLinkRefRegex(prj, num), None, title)
            if Projects(prj.toLowerCase) =>
          (
            f"${prj.toUpperCase}-PR$num%9s",
            LinkRef(
              s"${prj.toLowerCase.capitalize} PR#$num",
              Some(
                s"https://github.com/apache/${prj.toLowerCase}/pull/$num"
              ),
              title
            )
          )
        case l @ LinkRef(LinkRef.GithubPrLinkRefRegex(prj, num), _, _) =>
          (f"${prj.toUpperCase}-PR$num%9s", l)
        case other =>
          (other.ref, other)
      }
      .toMap
      .toSeq
      .sortBy(_._1)
      .map(_._2)
  }
}

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  val cmd = s"${GREEN}getting_things_done$RESET"
  println(
    s"""$BOLD$cmd - Let's get things done!
       |
       |  $CYAN     clean$RESET : Rewrite the status document
       |  $CYAN   newWeek$RESET : Add a new week to the status document
       |  $CYAN        pr$RESET : Add a PR review to this week
       |  $CYAN      stat$RESET : Add or update a weekly statistic
       |  $CYAN      task$RESET : Add or update a weekly task ${RED_B}TODO$RESET
       |  $CYAN      week$RESET : Print the last week status or a specific week
       |
       |Usage:
       |
       | $cmd ${CYAN}clean$RESET
       | $cmd ${CYAN}newWeek$RESET
       | $cmd ${CYAN}pr$RESET avro 9876 1234 "Implemented a thing" REVIEWED
       | $cmd ${CYAN}stat$RESET unread 448 [Wed]
       | $cmd ${CYAN}week$RESET
       | $cmd ${CYAN}week$RESET 2021/03/08
       |""".stripMargin)
}

@arg(doc = "Clean the existing document")
@main
def clean(): Unit = {
  // Read and overwrite the existing document without making any changes.
  val doc = Header.parse(read ! StatusFile, ProjectParserCfg)
  write.over(StatusFile, doc.build().toString)
  println(proposeGit(s"feat(status): Beautify the document"))
}

@arg(doc = "Add a new week")
@main
def newWeek(): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(read ! StatusFile, ProjectParserCfg)

  /** Create the new head week from the last week, if any is present. */
  def createHead(oldWeek: Option[Header]): Header = {
    oldWeek
      .map { week =>
        // if there was a last week
        week
          .copy(title = nextWeekStart(Some(week.title)))
          .replaceIn() {
            case (
                  Some(tb @ Table(_, Seq(TableRow(Seq("Stats", _*)), _*))),
                  _
                ) => {
              Seq(tb.replaceIn() {
                case (Some(TableRow(cells)), row)
                    if row > 0 && cells.size > 1 =>
                  Seq(TableRow.from(cells.head))
              })
            }
          }
      }
      .getOrElse(Header(2, nextWeekStart(None)))
  }

  // Add the new head week to the weekly statuses.
  val newDoc = doc.updateH1Weekly { weekly =>
    val headWeek = createHead(weekly.mds.collectFirst {
      case h2 @ Header(_, 2, _) => h2
    })
    weekly.flatMapFirstIn(ifNotFound = headWeek +: weekly.mds) {
      case h2 @ Header(_, 2, _) if h2 != headWeek => Seq(headWeek, h2)
    }
  }

  write.over(StatusFile, newDoc.doc.build().toString.trim() + "\n")
}

@arg(doc = "Start working on a new PR")
@main
def pr(
    @arg(doc = "The JIRA tag for the project")
    prj: String,
    @arg(doc = "The PR number being worked on")
    prNum: Int,
    @arg(doc = "The corresponding JIRA number being worked on")
    jira: Int,
    @arg(doc = "A short description for the PR")
    description: String,
    @arg(doc = "The status of the work on the PR")
    status: String = "TOREVIEW"
): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(read ! StatusFile, ProjectParserCfg)

  // The reference and task snippets to add to the file.
  val fullJira = if (jira != 0) Some(s"${prj.toUpperCase}-$jira") else None
  val prjPretty = prj.toLowerCase.capitalize
  val fullPr = if (prNum != 0) Some(s"$prjPretty PR#$prNum") else None
  val task = (fullJira, fullPr) match {
    case (Some(refJira), Some(refPr)) =>
      s"| 🔶$prjPretty   | **[$refJira]**:[$refPr] $description `$status` |"
    case (Some(refJira), None) =>
      s"| 🔶$prjPretty   | **[$refJira]** $description `$status` |"
    case (None, Some(refPr))   =>
      s"| 🔶$prjPretty   | [$refPr] $description `$status` |"
    case (None, None)          =>
      s"| 🔶$prjPretty   | $description `$status` |"
  }

  val docWithLinks = doc.updateH1Weekly { topWeekly =>
    // Add the two JIRA to the weekly status section.  Their URLs will be filled in
    // automatically on cleanup.
    topWeekly.copyMds(
      fullJira.map(LinkRef(_, None, Some(description))).toSeq ++
        fullPr.map(LinkRef(_, None, Some(description))) ++ topWeekly.mds
    )
  }

  val newDoc = docWithLinks.updateTopWeek { weekly =>
    // Update the most recent week.
    weekly.copyMds(weekly.mds :+ Paragraph(task))
  }

  val cleanedNewDoc = Header.parse(newDoc.doc.build().toString, ProjectParserCfg)

  println(
    proposeGit(
      s"feat(status): PR ${fullJira.orElse(fullPr).getOrElse("")} $description"
    )
  )
  write.over(StatusFile, cleanedNewDoc.build().toString.trim() + "\n")
}

@arg(doc = "Update a statistic in a table, typically for the day of the week")
@main
def stat(
          @arg(doc = "Update the statistic on this row (matches first element.")
          rowStat: String,
          @arg(doc = "The new value to put in the row")
          cell: String,
          @arg(doc = "The column to update or None for today")
          colStat: Option[String] = None): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(read ! StatusFile, ProjectParserCfg)
  val newDoc = doc.updateTopWeekStats(rowStat, cell, colStat)
  val cleanedNewDoc = Header.parse(newDoc.doc.build().toString, ProjectParserCfg)

  println(
    proposeGit(
      s"feat(status): Update $rowStat"
    )
  )
  write.over(StatusFile, cleanedNewDoc.build().toString.trim() + "\n")

}

@arg(doc = "Print the status for this week")
@main
def week(
    @arg(doc = "The week to list or none for this week.")
    week: Option[String] = None
): Unit = {
  // Read the existing document.
  val doc = Header.parse(read ! StatusFile, ProjectParserCfg)
  val topWeek: Seq[Markd] = doc.mds.flatMap {
    case h @ Header(title, 1, _) if title.startsWith(H1Weekly) =>
      h.mds.find {
        case Header(title, 2, _)
            if week.map(title.startsWith).getOrElse(title.length >= 10) =>
          true
        case _ => false
      }
    case _ => None
  }

  topWeek.headOption.map(_.build().toString).foreach(println(_))
}
