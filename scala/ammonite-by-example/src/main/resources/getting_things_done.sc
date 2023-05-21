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

import mainargs.{Flag, arg, main}

import scala.io.AnsiColor._
import scala.util.matching.Regex

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
import com.skraba.byexample.scala.gtd._
import com.skraba.byexample.scala.gtd.GettingThingsDone._
import com.skraba.byexample.scala.gtd.ThunderbirdMailbox.getNumberOfMessagesFromMailbox
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

/** Store projects in a configurable JSON object. */
private[this] val ProjectsJson: String = sys.env
  .getOrElse(
    s"${StatusTag}_PROJECTS",
    """{ "avro": {"ghRepo": "apache/avro", "jira": "AVRO"},
      |  "beam": {"ghRepo": "apache/avro", "jira": "BEAM"},
      |  "flink": {"ghRepo": "apache/flink", "jira": "FLINK"},
      |  "flink-web": {"ghRepo": "apache/flink-web", "jira": "FLINK"},
      |  "parquet-mr": {"ghRepo": "apache/parquet-mr", "jira": "PARQUET"},
      |  "spark": {"ghRepo": "apache/spark", "jira": "SPARK"}
      |}""".stripMargin
  )
val Projects: ujson.Obj = ujson.read(ProjectsJson).obj
// TODO: replace the contents with project-specific info

/** Some text that maps to to do task states */
val TextToToDoStates: Map[String, GettingThingsDone.ToDoState] =
  Map("MERGED" -> DoneToDo, "FIXED" -> DoneToDo, "DONE" -> DoneToDo)

/** Write the GettingThingsDone document to disk, optionally providing git
  * commands to check in the changes.
  *
  * @param gtd
  *   The document to write to disk
  * @param gitStatus
  *   The git status message to use, or none if no suggestion should be made.
  * @param compressTable
  *   Apply table compression to the document.
  */
private def writeGtd(
    gtd: GettingThingsDone,
    gitStatus: Option[String] = None,
    compressTable: Boolean = false
): Unit = {
  val before = os.read(StatusFile)

  val asText = ProjectParserCfg.clean(gtd.h0).build().toString
  val after = if (compressTable) asText.replaceAll(" +( \\|)", "$1") else asText

  os.write.over(StatusFile, after)
  gitStatus
    .map(msg => s"""${GREEN}Commit:$RESET
       |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
       |      git -C $StatusRepo difftool --staged
       |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
       |      git -C $StatusRepo commit -m $BOLD"$msg"$RESET
       |""".stripMargin)
    .foreach(println)

  // Some debugging for when an emoji is overwritten unexpectedly
  val written = os.read(StatusFile)
  if (written.contains("??")) {
    println(s"""${RED_B}Warning:$RESET
       |  The file was written with an unexpected ?? replacement""".stripMargin)

    if (after.contains("??"))
      println(
        s"""${RED_B}Warning:$RESET
           |  The text already contained the characters before writing the file""".stripMargin
      )

    if (before.contains("??"))
      println(s"""${RED_B}Warning:$RESET
           |  The file already contained the characters""".stripMargin)

    if (gtd.h0.build().toString.contains("??"))
      println(s"""${RED_B}Warning:$RESET
           |  The modified doc contains the characters""".stripMargin)
  }
}

object ProjectParserCfg extends ParserCfg {

  /** Regex used to find Jira-style link references. */
  val JiraLinkRefRegex: Regex = "^(\\S+)-(\\d+)$$".r

  /** Regex used to find GitHub PR-style link references. */
  val GitHubLinkRefRegex: Regex = "^([^/]+/[^/]+)#(\\d+)$$".r

  /** Group JIRA together by the project. */
  override def linkSorter(): PartialFunction[LinkRef, (String, LinkRef)] = {
    case LinkRef(JiraLinkRefRegex(prj, num), None, title)
        if Projects.value.contains(prj.toLowerCase) =>
      (
        f"0 ${prj.toUpperCase}-0 $num%9s",
        LinkRef(
          s"${prj.toUpperCase}-$num",
          Some(
            s"https://issues.apache.org/jira/browse/${prj.toUpperCase}-$num"
          ),
          title
        )
      )
    case l @ LinkRef(JiraLinkRefRegex(prj, num), _, _) =>
      (f"1 ${prj.toUpperCase}-0 $num%9s", l)
    case LinkRef(GitHubLinkRefRegex(prj, num), None, title)
        if Projects.value.contains(prj.toLowerCase) =>
      (
        f"0 ${prj.toUpperCase}-1 $num%9s",
        LinkRef(
          s"${prj.toLowerCase}#$num",
          Some(
            s"https://github.com/apache/${prj.toLowerCase}/pull/$num"
          ),
          title
        )
      )
    case l @ LinkRef(GitHubLinkRefRegex(prj, num), _, _) =>
      (f"1 ${prj.toUpperCase}-1 $num%9s", l)
    case l =>
      // All non matching links are sent to the bottom
      (s"2 ${l.ref}", l)
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
def clean(compress: Flag): Unit = {
  // Read and overwrite the existing document without making any changes.
  writeGtd(
    GettingThingsDone(os.read(StatusFile), ProjectParserCfg),
    Some("feat(status): Beautify the document"),
    compressTable = compress.value
  )
}

@arg(doc = "Open the document in an editor")
@main
def edit(): Unit = {
  os.proc(
    "code",
    "--new-window",
    StatusFile.toString()
  ).call(StatusRepo)
}

@arg(doc = "Add a new week")
@main
def newWeek(): Unit = {
  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  /** Create the new head week from the last week, if any is present. */
  def createHead(oldWeek: Option[Header]): Header = {
    oldWeek
      .map { week =>
        // if there was a last week
        week
          .copy(title = nextWeekStart(Some(week.title)))
          .replaceIn() {
            // Copy the Stats table, but empty out any values in the rows.
            case (Some(tb: Table), _) if tb.title == TableStats =>
              Seq(tb.replaceIn() {
                case (Some(TableRow(cells)), row)
                    if row > 0 && cells.size > 1 =>
                  Seq(TableRow.from(cells.head))
              })
            // Copy the To Do table, but remove any done elements.
            case (Some(tb: Table), _) if tb.title == TableToDo =>
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
        case (Some(tb: Table), _) if tb.title == TableToDo =>
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
  val gtdUpdated = gtd.updateWeeklies { weeklies =>
    val headWeek = createHead(weeklies.mds.collectFirst {
      case h2 @ Header(_, 2, _) => h2
    })
    weeklies.flatMapFirstIn(
      ifNotFound = headWeek +: weeklies.mds,
      replace = true
    ) {
      case h2 @ Header(_, 2, _) if h2 != headWeek =>
        Seq(headWeek, updateLastHead(h2))
    }
  }

  writeGtd(
    gtdUpdated,
    Some(
      s"feat(status): Add new week ${gtdUpdated.topWeek.map(_.title).getOrElse("")}"
    )
  )
}

@arg(doc = "Start working on a new PR")
@main
def pr(
    @arg(doc = "The tag for the project")
    prj: String,
    @arg(doc = "The PR number being worked on")
    prNum: String,
    @arg(doc = "The corresponding JIRA number being worked on")
    jira: String,
    @arg(doc = "A short description for the PR")
    description: String,
    @arg(doc = "The status of the work on the PR")
    status: String = "TOREVIEW"
): Unit = {
  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  // The reference and task snippets to add to the file.
  val fullJira =
    if (jira != "0" && jira != "") Some(s"${prj.toUpperCase}-$jira") else None
  val fullPr =
    if (prNum != "0" && prNum != "") Some(s"apache/${prj.toLowerCase}#$prNum")
    else None
  val task = (fullJira, fullPr) match {
    case (Some(refJira), Some(refPr)) => s"**[$refJira]**:[$refPr]"
    case (Some(refJira), None)        => s"**[$refJira]**"
    case (None, Some(refPr))          => s"[$refPr]"
    case (None, None)                 => ""
  }

  val gtdWithLinks = gtd.updateHeader1("References") { refSection =>
    // Add the two JIRA to the weekly status section.  Their URLs will be filled in
    // automatically on cleanup.
    refSection.copyMds(
      fullJira.map(LinkRef(_, None, Some(description))).toSeq ++
        fullPr.map(LinkRef(_, None, Some(description))) ++ refSection.mds
    )
  }

  val gtdUpdated =
    gtdWithLinks.addTopWeekToDo(
      prj.toLowerCase.capitalize,
      s"$task $description `$status`",
      TextToToDoStates.getOrElse(status, MaybeToDo)
    )

  writeGtd(
    gtdUpdated,
    Some(
      s"feat(status): PR ${fullJira.orElse(fullPr).getOrElse("")} $description"
    )
  )
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
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  // TODO: If date is in a YYYY/MM/DD format, then to the correct date
  val gtdUpdated = gtd.updateTopWeekStats(rowStat, cell, date)
  writeGtd(gtdUpdated, Some(s"feat(status): Update $rowStat"))
}

@arg(doc = "Update many statistics for today.")
@main
def statsToday(
    @arg(doc = "Key/value list of statistics to be updated")
    stats: String*
): Unit = {
  // Group the stats by two, and print them
  val groupedStats = stats.grouped(2).toSeq
  val maxKeySize = groupedStats.map(_.head.length).max + 1
  groupedStats.foreach {
    case Seq(k, v) => printf(s"$MAGENTA%${maxKeySize}s$RESET: %s\n", k, v)
    case Seq(k)    => printf(s"$MAGENTA%${maxKeySize}s$RESET:\n", k)
  }

  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  val gtdUpdated = groupedStats.foldLeft(gtd) {
    (acc: GettingThingsDone, list: Seq[String]) =>
      acc.updateTopWeekStats(list.head, list.tail.headOption.getOrElse(""))
  }
  writeGtd(
    gtdUpdated,
    Some(s"feat(status): Update ${stats.grouped(2).map(_.head).mkString(",")}")
  )
}

@arg(doc = "Update the daily statistics.")
@main
def statsDaily(): Unit = {
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  // Get the daily stats configuration
  val cfgStats: Option[Table] = gtd.cfg.flatMap(_.collectFirstRecursive {
    case tbl: Table if tbl.title == TableStats =>
      tbl
  })

  val UnreadEmail = """\s*Unread email \((\S+)(\s*(\S+)?)\)\s*""".r
  val GitHubOpenPR = """\s*GitHub open PR \((\S+)\)\s*""".r

  // Convert them into arguments for the daily statistics
  val args: Option[Seq[String]] = cfgStats.map {
    _.mds
      .drop(1)
      .collect {
        case TableRow(Seq(rowHead, UnreadEmail(filename, _, refiner))) =>
          Seq(
            rowHead,
            getNumberOfMessagesFromMailbox(
              ".thunderbird",
              filename,
              Option(refiner).getOrElse("")
            ).toString
          )
        case TableRow(Seq(rowHead, GitHubOpenPR(spec))) =>
          val prsResponse = requests.get(
            s"https://api.github.com/search/issues?q=repo:$spec%20state:open%20is:pr"
          )
          val prs = ujson.read(prsResponse.text())
          Seq(rowHead, prs("total_count").toString)
        case _ => Seq.empty
      }
      .flatten
  }

  // Add them as daily statistics
  args.foreach(statsToday(_: _*))
}

@arg(doc = "Extract a statistic in the table as a time-series")
@main
def statExtract(
    @arg(doc = "Update the statistic on this row (matches first element.")
    rowStat: String
): Unit = {
  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  println("date,value")
  for ((date, value) <- gtd.extractStats(rowStat))
    println(s"$date,$value")
}

@arg(doc = "Print the status for this week")
@main
def week(
    @arg(doc = "The week to list or none for this week.")
    week: Option[String] = None
): Unit = {
  // Read the existing document.
  val gtd = Header.parse(os.read(StatusFile), ProjectParserCfg)
  val topWeek: Seq[Markd] = gtd.mds.flatMap {
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
