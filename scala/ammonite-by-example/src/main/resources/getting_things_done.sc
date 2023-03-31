// #!/usr/bin/env amm
// Disable the shebang before developing in IntelliJ
// https://youtrack.jetbrains.com/issue/SCL-13279

/** A user script for interacting with status sheets and todo lists.
  *
  * This assumes that https://ammonite.io/ is installed.
  *
  * Ammonite includes:
  *   - requests (https://github.com/lihaoyi/requests-scala)
  *   - upickle (https://github.com/lihaoyi/upickle)
  */

import mainargs.{arg, main}

import scala.io.AnsiColor._

interp.repositories() ++= {
  // Get the local Maven repository.
  val localM2: os.Path = Option(sys.props("maven.repo.local"))
    .map(os.Path(_))
    .getOrElse(os.home / ".m2" / "repository")

  Seq(coursierapi.MavenRepository.of(localM2.toIO.toURI.toString))
}

@
// Intellij always removes the following line, which should be
// import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`
import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`
import com.skraba.byexample.scala.markd.GettingThingsDone._
import com.skraba.byexample.scala.markd._

/** A tag used to distinguish between documents. */
val StatusTag: String = sys.env.getOrElse("GTD_TAG", "GTD")

/** Git root directory for the status file. */
val StatusRepo: os.Path = sys.env
  .get(s"${StatusTag}_STATUS_REPO")
  .map(os.Path(_))
  .getOrElse(os.home / "Documents")

/** The actual status file to update. */
val StatusFile: os.Path = sys.env
  .get(s"${StatusTag}_STATUS_FILE")
  .map(os.Path(_))
  .getOrElse(StatusRepo / "todo" / "status.md")

/** The list of apache projects. */
val Projects: Map[String, Int] = Map(
  "avro" -> 1, // TODO: These will be replaced with project-specific info
  "beam" -> 1,
  "flink" -> 1,
  "parquet" -> 1,
  "pulsar" -> 1,
  "spark" -> 1
);

/** Some text that maps to to do task states */
val TextToToDoStates: Map[String, GettingThingsDone.ToDoState] =
  Map("MERGED" -> DoneToDo, "FIXED" -> DoneToDo, "DONE" -> DoneToDo)

/** Propose a git commit message for the status page
  * @param msg
  *   The git message to propose
  * @return
  *   A string to copy and paste to check the changes.
  */
private def proposeGit(msg: String): String = {
  s"""${GREEN}Commit:$RESET
     |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
     |      git -C $StatusRepo difftool --staged
     |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
     |      git -C $StatusRepo commit -m $BOLD"$msg"$RESET
     |""".stripMargin
}

object ProjectParserCfg extends ParserCfg {

  /** Group JIRA together by the project. */
  override def linkSorter(): PartialFunction[LinkRef, (String, LinkRef)] = {
    case LinkRef(LinkRef.JiraLinkRefRegex(prj, num), None, title)
      if Projects.contains(prj.toLowerCase) =>
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
    case l@LinkRef(LinkRef.JiraLinkRefRegex(prj, num), _, _) =>
      (f"${prj.toUpperCase}-$num%9s", l)
    case LinkRef(LinkRef.GithubPrLinkRefRegex(prj, num), None, title)
      if Projects.contains(prj.toLowerCase) =>
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
    case l@LinkRef(LinkRef.GithubPrLinkRefRegex(prj, num), _, _) =>
      (f"${prj.toUpperCase}-PR$num%9s", l)
  }
}

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  val cmd = s"${GREEN}getting_things_done$RESET"
  println(s"""$BOLD$cmd - Let's get things done!
       |
       |  $CYAN       clean$RESET : Beautify the status document
       |  $CYAN        edit$RESET : Open the status document in a editor (Visual Code).
       |  $CYAN     newWeek$RESET : Add a new week to the status document
       |  $CYAN          pr$RESET : Add a PR review to this week
       |  $CYAN        stat$RESET : Add or update a weekly statistic
       |  $CYAN statExtract$RESET : Extract a statistic from the document
       |  $CYAN        task$RESET : Add or update a weekly task ${RED_B}TODO$RESET
       |  $CYAN        week$RESET : Print the last week status or a specific week
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
  val doc = Header.parse(os.read(StatusFile), ProjectParserCfg)
  os.write.over(StatusFile, doc.build().toString)
  println(proposeGit(s"feat(status): Beautify the document"))
}

@arg(doc = "Open the document in an editor")
@main
def edit(): Unit = {
  os.proc(
    "code",
    "--new-window",
    StatusFile.toString()
  ).call(StatusRepo )
}

@arg(doc = "Add a new week")
@main
def newWeek(): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  /** Create the new head week from the last week, if any is present. */
  def createHead(oldWeek: Option[Header]): Header = {
    oldWeek
      .map { week =>
        // if there was a last week
        week
          .copy(title = nextWeekStart(Some(week.title)))
          .replaceIn() {
            // Copy the Stats table, but empty out any values in the rows.
            case (Some(tb : Table), _) if tb.title == TableStats =>
              Seq(tb.replaceIn() {
                case (Some(TableRow(cells)), row)
                    if row > 0 && cells.size > 1 =>
                  Seq(TableRow.from(cells.head))
              })
            // Copy the To Do table, but remove any done elements.
            case (Some(tb : Table), _) if tb.title == TableToDo =>
              Seq(tb.replaceIn() {
                case (Some(TableRow(Seq(taskText, _*))), row)
                    if row > 0 && taskText.startsWith(DoneToDo.txt) =>
                  Seq()
              })
          }
      }
      .getOrElse(Header(2, nextWeekStart(None)))
  }

  /** Create the new head week from the last week, if any is present. */
  def updateLastHead(oldWeek: Header): Header = {
    oldWeek
      .replaceIn() {
        // Copy the To Do table, but update all Later tasks.
        case (Some(tb : Table), _) if tb.title == TableToDo =>
          Seq(tb.replaceIn() {
            case (Some(TableRow(cells @ Seq(taskText, _*))), row)
                if row > 0 && taskText.startsWith(MaybeToDo.txt) =>
              Seq(
                TableRow(
                  cells.updated(
                    0,
                    taskText.replaceAllLiterally(MaybeToDo.txt, LaterToDo.txt)
                  )
                )
              )
          })
      }
  }

  // Add the new head week to the weekly statuses.
  val newDoc = doc.updateWeeklies { weeklies =>
    val headWeek = createHead(weeklies.mds.collectFirst {
      case h2 @ Header(_, 2, _) => h2
    })
    weeklies.flatMapFirstIn(ifNotFound = headWeek +: weeklies.mds, replace=true) {
      case h2 @ Header(_, 2, _) if h2 != headWeek =>
        Seq(headWeek, updateLastHead(h2))
    }
  }

  println(
    proposeGit(
      s"feat(status): Add new week ${newDoc.topWeek.headOption.map(_.title).getOrElse("")}"
    )
  )

  os.write.over(StatusFile, newDoc.doc.build().toString.trim() + "\n")
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
  val doc = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  // The reference and task snippets to add to the file.
  val fullJira = if (jira != 0) Some(s"${prj.toUpperCase}-$jira") else None
  val prjPretty = prj.toLowerCase.capitalize
  val fullPr = if (prNum != 0) Some(s"$prjPretty PR#$prNum") else None
  val task = (fullJira, fullPr) match {
    case (Some(refJira), Some(refPr)) => s"**[$refJira]**:[$refPr]"
    case (Some(refJira), None)        => s"**[$refJira]**"
    case (None, Some(refPr))          => s"[$refPr]"
    case (None, None)                 => ""
  }

  val docWithLinks = doc.updateHeader1("References") { refSection =>
    // Add the two JIRA to the weekly status section.  Their URLs will be filled in
    // automatically on cleanup.
    refSection.copyMds(
      fullJira.map(LinkRef(_, None, Some(description))).toSeq ++
        fullPr.map(LinkRef(_, None, Some(description))) ++ refSection.mds
    )
  }

  val newDoc =
    docWithLinks.addTopWeekToDo(
      prjPretty,
      s"$task $description `$status`",
      TextToToDoStates.getOrElse(status, MaybeToDo)
    )

  val cleanedNewDoc =  ProjectParserCfg.clean(newDoc.doc)
  println(
    proposeGit(
      s"feat(status): PR ${fullJira.orElse(fullPr).getOrElse("")} $description"
    )
  )
  os.write.over(StatusFile, cleanedNewDoc.build().toString.trim() + "\n")
}

@arg(doc = "Update a statistic in a table, typically for the day of the week")
@main
def stat(
    @arg(doc = "Update the statistic on this row (matches first element.")
    rowStat: String,
    @arg(doc = "The new value to put in the row")
    cell: String,
    @arg(doc = "The column to update or None for today")
    date: Option[String] = None
): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  // TODO: If date is in a YYYY/MM/DD format, then to the correct date
  val newDoc = doc.updateTopWeekStats(rowStat, cell, date)
  val cleanedNewDoc = ProjectParserCfg.clean(newDoc.doc)

  println(
    proposeGit(
      s"feat(status): Update $rowStat"
    )
  )
  os.write.over(StatusFile, cleanedNewDoc.build().toString.trim() + "\n")
}

@arg(doc = "Update many statistics for today.")
@main
def statsToday(
    @arg(doc = "Key/value list of statistics to be updated")
    stats: String*
): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  val newDoc = stats.grouped(2).foldLeft(doc) {
    (gtd: GettingThingsDone, list: Seq[String]) =>
      gtd.updateTopWeekStats(list.head, list.tail.headOption.getOrElse(""))
  }
  val cleanedNewDoc =  ProjectParserCfg.clean(newDoc.doc)

  println(
    proposeGit(
      s"feat(status): Update ${stats.grouped(2).map(_.head).mkString(",")}"
    )
  )
  os.write.over(StatusFile, cleanedNewDoc.build().toString.trim() + "\n")
}

@arg(doc = "Extract a statistic in the table as a time-series")
@main
def statExtract(
    @arg(doc = "Update the statistic on this row (matches first element.")
    rowStat: String
): Unit = {
  // Read the existing document.
  val doc = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  println("date,value")
  for ((date, value) <- doc.extractStats(rowStat))
    println(s"$date,$value")
}

@arg(doc = "Print the status for this week")
@main
def week(
    @arg(doc = "The week to list or none for this week.")
    week: Option[String] = None
): Unit = {
  // Read the existing document.
  val doc = Header.parse(os.read(StatusFile), ProjectParserCfg)
  val topWeek: Seq[Markd] = doc.mds.flatMap {
    case h @ Header(title, 1, _) if title.startsWith(H1Weeklies) =>
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
