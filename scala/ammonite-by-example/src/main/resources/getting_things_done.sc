#!/usr/bin/env amm

/** A user script for interacting with status sheets and task lists. */

import mainargs.{Flag, arg, main}

import java.time.{Instant, LocalDate}
import scala.io.AnsiColor._
import scala.util.matching.Regex

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("scala-by-example")
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ConsoleCfg
import com.skraba.byexample.scala.ammonite.gtd.GettingThingsDone._
import com.skraba.byexample.scala.ammonite.gtd.ThunderbirdMailbox.getNumberOfMessagesFromMailbox
import com.skraba.byexample.scala.ammonite.gtd._
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

/** A tag used to distinguish between documents. */
val StatusTag: String =
  sys.props.get("GTD_TAG").orElse(sys.env.get("GTD_TAG")).getOrElse("GTD")

/** Git root directory for the status file. */
val StatusRepo: os.Path = sys.props
  .get(s"${StatusTag}_STATUS_REPO")
  .orElse(sys.env.get(s"${StatusTag}_STATUS_REPO"))
  .map(os.Path(_))
  .getOrElse(os.home / "Documents")

/** The actual status file to update. */
val StatusFile: os.Path = sys.props
  .get(s"${StatusTag}_STATUS_FILE")
  .orElse(sys.env.get(s"${StatusTag}_STATUS_FILE"))
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
    if (before.contains("??"))
      println(s"""${RED_B}Warning:$RESET
           |  The file already contained the characters ??""".stripMargin)

    // These are very likely to occur together
    if (gtd.h0.build().toString.contains("??"))
      println(s"""${RED_B}Warning:$RESET
           |  The built text contains ??""".stripMargin)
    if (after.contains("??"))
      println(
        s"""${RED_B}Warning:$RESET
           |  The new text before writing contains ??""".stripMargin
      )

    // This is the issue that we'd like to debug, if it occurs alone.
    println(
      s"""${RED_B}Warning:$RESET
         |  The file was written with an unexpected ?? replacement""".stripMargin
    )
  }
}

object ProjectParserCfg extends ParserCfg {

  /** Regex used to find Jira-style link references. */
  val JiraLinkRefRegex: Regex = "^(\\S+)-(\\d+)$$".r

  /** Regex used to find GitHub PR-style link references. */
  val GitHubLinkRefRegex: Regex = "^([^/]+/[^/]+)#(\\d+)$$".r

  /** Group JIRA together by the project. */
  override def linkSorter(): PartialFunction[LinkRef, (String, LinkRef)] = {
    case LinkRef(JiraLinkRefRegex(prj, num), url, title)
        if Projects.value.contains(prj.toLowerCase) =>
      (
        f"0 ${prj.toUpperCase}-0 $num%9s",
        LinkRef(
          s"${prj.toUpperCase}-$num",
          Some(
            url.getOrElse(
              s"https://issues.apache.org/jira/browse/${prj.toUpperCase}-$num"
            )
          ),
          title
        )
      )
    case l @ LinkRef(JiraLinkRefRegex(prj, num), _, _) =>
      (f"1 ${prj.toUpperCase}-0 $num%9s", l)
    case LinkRef(GitHubLinkRefRegex(prj, num), url, title)
        if Projects.value.contains(prj.toLowerCase) =>
      (
        f"0 ${prj.toUpperCase}-1 $num%9s",
        LinkRef(
          s"${prj.toLowerCase}#$num",
          Some(
            url.getOrElse(
              s"https://github.com/apache/${prj.toLowerCase}/pull/$num"
            )
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

// ==========================================================================
// help

@arg(doc = "Print help to the console.")
@main
def help(cfg: ConsoleCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "getting_things_done.sc"
  println(
    cfg.helpHeader(
      cli,
      "Let's get things done!",
      "clean" -> "Beautify the status document",
      "edit" -> "Open the status document in a editor (Visual Code)",
      "newWeek" -> "Add a new week to the status document",
      "pr" -> "Add a PR review to this week",
      "stat" -> "Add or update a weekly statistic",
      "statsDaily" -> "Update a list of configured statistics (if any)",
      "statsExtract" -> "Extract a statistic from the document",
      "task" -> s"Add or update a weekly task ${cfg.redBg("TODO")}",
      "week" -> "Print the last week status or a specific week"
    )
  )

  // Usage examples
  println(cfg.helpUse(cli, "clean"))
  println(cfg.helpUse(cli, "newWeek"))
  println(
    cfg.helpUse(
      cli,
      "pr",
      "avro",
      "9876",
      "1234",
      "\"Implemented a thing\"",
      "REVIEWED"
    )
  )
  println(cfg.helpUse(cli, "stat", "unread", "448", "[Wed]"))
  println(cfg.helpUse(cli, "week", "[2021/03/08]"))
  println()
}

// ==========================================================================
// clean

@arg(doc = "Clean the existing document")
@main
def clean(
    @arg(doc = "Write the document with less whitespace")
    compress: Flag
): Unit = {
  // Read and overwrite the existing document without making any changes.
  writeGtd(
    GettingThingsDone(os.read(StatusFile), ProjectParserCfg),
    Some("feat(status): Beautify the document"),
    compressTable = compress.value
  )
}

// ==========================================================================
// edit

@arg(doc = "Open the document in an editor")
@main
def edit(): Unit = {
  os.proc(
    "code",
    "--new-window",
    StatusFile.toString()
  ).call(StatusRepo)
}

// ==========================================================================
// newWeek

@arg(doc = "Add a new week")
@main
def newWeek(
    @arg(doc = "Continue to add new weeks until we reach the current date")
    now: Flag
): Unit = {
  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  val gtdUpdated: GettingThingsDone = if (now.value) {
    val token = GettingThingsDone.Pattern.format(Instant.now())
    gtd.newWeek(Some(token))
  } else {
    // Simply add one week to the document.
    gtd.newWeek(None)
  }
  writeGtd(
    gtdUpdated,
    Some(
      s"feat(status): Add new week ${gtdUpdated.topWeek.map(_.title).getOrElse("")}"
    )
  )
}

// ==========================================================================
// pr

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

// ==========================================================================
// stat

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

// ==========================================================================
// statsToday

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

// ==========================================================================
// statsDaily

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

/** Given an optional parameter, calculates the date range that corresponds to
  * that month.
  *
  * If the month is not present, the range is two [[None]] values.
  *
  * If the month is positive, the range is the first and last day of that month
  * in this year. January is "1" and December is "12".
  *
  * If the month is zero or negative, the selected month is relative to the
  * current month (where "0" is this month, and "-1" is last month).
  *
  * @param month
  *   The optional month parameter.
  * @return
  *   The date range for the first and last day of that month, or None(s) if the
  *   parameter isn't specified.
  */
private def getMonth(
    month: Option[Int]
): (Option[LocalDate], Option[LocalDate]) = {
  month
    .map {
      case m if m > 0 =>
        LocalDate.now().withMonth(m).withDayOfMonth(1)
      case m =>
        LocalDate.now().plusMonths(m).withDayOfMonth(1)
    }
    .map { from =>
      (Some(from), Some(from.plusMonths(1).minusDays(1)))
    }
    .getOrElse(None, None)
}

// ==========================================================================
// statsExtract

@arg(doc = "Extract a statistic in the table as a time-series")
@main
def statExtract(
    @arg(doc = "Update the statistic on this row (matches first element.")
    rowStat: String = "",
    @arg(doc = "Print the output as CSV.")
    csv: Flag,
    @arg(
      doc = "Limit the statistics to a specific month, where '1' is January. " +
        "If non-positive (including zero), counts relative to the current month. "
    )
    month: Option[Int] = None
): Unit = {
  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  // If a specific month was set, then extract for that month only.
  val (from: Option[LocalDate], to: Option[LocalDate]) = getMonth(month)
  val stats = gtd.extractStats(name = rowStat, from = from, to = to)

  if (csv.value) {
    if (rowStat.isEmpty) {
      println("date,stat,value")
      for ((date, stat, value) <- stats)
        println(s"${date.format(Pattern)},$stat,$value")
    } else {
      println("date,value")
      for ((date, stat, value) <- stats)
        println(s"${date.format(Pattern)},$value")
    }
  } else {
    if (rowStat.isEmpty) {
      println(
        Table(
          Seq.fill(3)(Align.LEFT),
          TableRow.from("Date", "Stat", "Value") +: stats.map {
            case (date, stat, value) =>
              TableRow.from(date.format(Pattern), stat, value)
          }
        ).build().toString
      )
    } else {
      println(
        Table(
          Seq.fill(2)(Align.LEFT),
          TableRow.from("Date", "Value") +: stats.map { case (date, _, value) =>
            TableRow.from(date.format(Pattern), value)
          }
        ).build().toString
      )
    }
  }
}

// ==========================================================================
// todoExtract

@arg(doc = "Extract the To Do tasks table")
@main
def todoExtract(
    @arg(doc = "Print the output as CSV.")
    csv: Flag,
    @arg(doc = "Only list completed tasks")
    completed: Flag,
    @arg(
      doc = "Limit the statistics to a specific month, where '1' is January. " +
        "If non-positive (including zero), counts relative to the current month. "
    )
    month: Option[Int] = None,
    @arg(doc = "Only list tasks that match the given unanchored regex")
    task: Option[String] = None
): Unit = {
  // Read the existing document.
  val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)

  // If a specific month was set, then extract for that month only.
  val (from: Option[LocalDate], to: Option[LocalDate]) = getMonth(month)
  val tasks0 = gtd.extractToDo(
    from = from,
    to = to,
    completed = if (completed.value) Some(true) else None
  )

  // Apply the filter to get the final list of tasks (if any filter was given)
  val tasks = task
    .map { re =>
      val taskNameRe = new Regex(re).unanchored
      tasks0.filter(t => taskNameRe.matches(t._3))
    }
    .getOrElse(tasks0)

  if (csv.value) {
    println("date,state,category,notes")
    for ((date, state, category, notes) <- tasks)
      println(s"${date.format(Pattern)},${state.txt},$category,$notes")
  } else {
    println(
      Table(
        Seq.fill(4)(Align.LEFT),
        TableRow.from("Date", "State", "Category", "Notes") +: tasks
          .map { case (date, state, category, notes) =>
            TableRow.from(date.format(Pattern), state.txt, category, notes)
          }
      ).build().toString
    )
  }
}

// ==========================================================================
// week

@arg(doc = "Print the status for this week")
@main
def week(
    @arg(doc = "The week to list or none for this week.")
    week: Option[String] = None,
    @arg(doc = "Print extra information to the screen.")
    verbose: Flag
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

  topWeek.headOption.map(_.build().toString).foreach(println)

  if (verbose.value)
    topWeek.headOption
      .collect { case h: Header => h }
      .map(week =>
        println(s"""${GREEN}Commit:$RESET
             |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
             |      git -C $StatusRepo difftool --staged
             |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
             |      git -C $StatusRepo commit -m $BOLD"feat(${StatusFile.baseName
          .stripSuffix(StatusFile.ext)}): Updated ${week.title}"$RESET
             |""".stripMargin)
      )
}
