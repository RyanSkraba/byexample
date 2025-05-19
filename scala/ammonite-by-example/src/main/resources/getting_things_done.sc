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
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ConsoleCfg
import com.skraba.byexample.scala.ammonite.gtd.GettingThingsDone._
import com.skraba.byexample.scala.ammonite.gtd.ThunderbirdMailbox.getNumberOfMessagesFromMailbox
import com.skraba.byexample.scala.ammonite.gtd._
import com.tinfoiled.markd._

// ==========================================================================
// Top level variables available to the script

/** A tag used to distinguish between documents. */
val StatusTag: String = sys.props.get("GTD_TAG").orElse(sys.env.get("GTD_TAG")).getOrElse("GTD")

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

lazy val StatusContents = os.read(StatusFile)

lazy val Gtd = GettingThingsDone(StatusContents, ProjectParserCfg)

/** Configuration for a project (usually) that can be assigned tasks.
  *
  * @param tag
  *   The short key to refer to the project, usually all lowercase.
  * @param titleOpt
  *   The title of the project to use in the weekly To Do, usually in CamelCase. If it's not explicitly present, this is
  *   created from the tag.
  * @param issueRefOpt
  *   The text to use for an reference to an issue on this project. If it's not explicitly present, this is created from
  *   the tag.
  * @param issueLinkOpt
  *   The URL to use for an issue on this project. If it's not explicitly present, this is created from the tag.
  * @param prRefOpt
  *   The text to use for a reference to a PR on this project.
  * @param prLinkOpt
  *   The URL to use for a PR on this project.
  */
case class PrjTask(
    tag: String,
    titleOpt: Option[String],
    issueRefOpt: Option[String],
    issueLinkOpt: Option[String],
    prRefOpt: Option[String],
    prLinkOpt: Option[String]
) {

  /** The title of the project to use in the weekly To Do list. */
  val title: String = titleOpt.getOrElse(tag.toLowerCase.capitalize)

  /** The user-visible way this issue is referenced in the To Do list. */
  val issueRef: String = issueRefOpt.getOrElse(tag.toUpperCase + "-")

  /** The URL used to construct the link to the issue number. If this contains a %s, the issue number will replace it,
    * otherwise the issue number is appended.
    */
  val issueLink: String = issueLinkOpt.getOrElse(s"https://issues.apache.org/jira/browse/${tag.toUpperCase}-")

  /** The user-visible way the PR is references in the To Do list. */
  val prRef: String = prRefOpt.getOrElse(s"apache/${tag.toLowerCase}#")

  /** The URL used to construct the link to the PR. If this contains a %s, the PR number will replace it, otherwise the
    * PR number is appended.
    */
  val prLink: String = prLinkOpt.getOrElse(s"https://github.com/apache/${tag.toLowerCase}/pull/")

  private def replace(tmpl: String, in: String): String = if (tmpl.contains("%s")) tmpl.format(in) else tmpl + in
  def issueRefOf(in: String) = replace(issueRef, in)
  def issueLinkOf(in: String) = replace(issueLink, in)
  def prRefOf(in: String) = replace(prRef, in)
  def prLinkOf(in: String) = replace(prLink, in)
}

object PrjTask {

  def apply(
      tag: String,
      title: String,
      issueRef: String,
      issueLink: String,
      prRef: String,
      prLink: String
  ): PrjTask =
    PrjTask(
      tag,
      if (title.isEmpty) None else Some(title),
      if (issueRef.isEmpty) None else Some(issueRef),
      if (issueLink.isEmpty) None else Some(issueLink),
      if (prRef.isEmpty) None else Some(prRef),
      if (prLink.isEmpty) None else Some(prLink)
    )

  /** Logical defaults for a root ASF project. */
  def asf(tag: String): PrjTask = PrjTask(tag, None, None, None, None, None)
  def asf(tag: String, title: String, jira: String, repo: String): PrjTask = PrjTask(
    tag = tag,
    title = title,
    issueRef = s"$jira-",
    issueLink = s"https://issues.apache.org/jira/browse/$jira-",
    prRef = s"apache/$repo#",
    prLink = s"https://github.com/apache/$repo/pull/"
  )
}

lazy val PrjTasks: Map[String, PrjTask] = GettingThingsDone(StatusContents).cfg
  .flatMap(_.collectFirstRecursive {
    case tbl: Table if tbl.title == "Projects" =>
      for (row <- 1 until tbl.rowSize)
        yield PrjTask(tbl(0, row), tbl(1, row), tbl(2, row), tbl(3, row), tbl(4, row), tbl(5, row))
  })
  .getOrElse(
    // Defaults if there is no project configuration
    Seq(
      PrjTask.asf("avro"),
      PrjTask.asf("beam"),
      PrjTask.asf("flink"),
      PrjTask.asf("flink-web", "Flink", "FLINK", "flink-web"),
      PrjTask.asf("parquet", "Parquet", "PARQUET", "parquet-mr"),
      PrjTask.asf("spark")
    )
  )
  .map(p => p.tag -> p)
  .toMap

/** The configuration for the parser. */
object ProjectParserCfg extends ParserCfg {

  /** Regex used to find Jira-style link references. */
  val JiraLinkRefRegex: Regex = "^(\\S+-)(\\d+)$".r

  /** Regex used to find GitHub PR-style link references. */
  val GitHubLinkRefRegex: Regex = "^([^/]+/[^/]+#)(\\d+)$".r

  /** Group JIRA together by the project. */
  override def linkSorter(): PartialFunction[LinkRef, (String, LinkRef)] = {
    case l @ LinkRef(JiraLinkRefRegex(tag, num), url, title) =>
      PrjTasks.find(_._2.issueRef == tag) match {
        case Some((_, prj)) =>
          (
            f"0 ${prj.tag}-0 $num%9s",
            LinkRef(l.ref, Some(url.getOrElse(prj.issueLinkOf(num))), title)
          )
        case None => (f"1 ${tag.toUpperCase}-0 $num%9s", l)
      }
    case l @ LinkRef(GitHubLinkRefRegex(tag, num), url, title) =>
      PrjTasks.find(_._2.prRef == tag) match {
        case Some((_, prj)) =>
          (
            f"0 ${prj.tag}-1 $num%9s",
            LinkRef(l.ref, Some(url.getOrElse(prj.prLinkOf(num))), title)
          )
        case None => (f"1 ${tag.toUpperCase}-1 $num%9s", l)
      }
    case l =>
      // All non matching links are sent to the bottom
      (s"2 ${l.ref}", l)
  }
}

/** Some text that maps to to do task states */
val TextToToDoStates: Map[String, GettingThingsDone.ToDoState] =
  Map("MERGED" -> DoneToDo, "FIXED" -> DoneToDo, "DONE" -> DoneToDo)

/** Write the GettingThingsDone document to disk, optionally providing git commands to check in the changes.
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
    out: ConsoleCfg,
    gitStatus: Option[String] = None,
    compressTable: Boolean = false
): Unit = {
  val asText = ProjectParserCfg.clean(gtd.h0).build().toString
  val after = if (compressTable) asText.replaceAll(" +( \\|)", "$1") else asText

  os.write.over(StatusFile, after)

  gitStatus
    .map(msg => s"""${out.green("Commit:")}
         |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
         |      git -C $StatusRepo difftool --staged
         |  git -C $StatusRepo add ${StatusFile.relativeTo(StatusRepo)} &&
         |      git -C $StatusRepo commit -m ${out.bold(s"\"$msg\"")}
         |""".stripMargin)
    .foreach(println)

  // Some debugging for when an emoji is overwritten unexpectedly
  val written = os.read(StatusFile)
  if (written.contains("??")) {
    if (StatusContents.contains("??"))
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

// ==========================================================================
// help

@arg(doc = "Print help to the console.")
@main
def help(out: ConsoleCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "getting_things_done.sc"
  println(
    out.helpHeader(
      cli,
      s"Let's get things done!\n(${out.msg(StatusTag)}:${out.msg(StatusFile)})",
      "clean" -> "Beautify the status document",
      "edit" -> "Open the status document in a editor (Visual Code)",
      "addWeek" -> "Add a new week to the status document",
      "link" -> "Add a new link and task for this week",
      "pr" -> "Add a PR review to this week",
      "stat" -> "Add or update a weekly statistic",
      "statsDaily" -> "Update a list of configured statistics (if any)",
      "statsExtract" -> "Extract a statistic from the document",
      "task" -> s"Add or update a weekly task ${out.redBg("TODO")}",
      "week" -> "Print the last week status or a specific week"
    )
  )

  // Usage examples
  println(out.helpUse(cli, "clean"))
  println(out.helpUse(cli, "addWeek"))
  println(out.helpUse(cli, "pr", "avro", "9876", "1234", "\"Implemented a thing\"", "REVIEWED"))
  println(out.helpUse(cli, "link", "https://example.com", "Example link"))
  println(out.helpUse(cli, "stat", "unread", "448", "[Wed]"))
  println(out.helpUse(cli, "week", "[2021-03-08]"))
  println()
}

// ==========================================================================
// clean

@arg(doc = "Clean the existing document")
@main
def clean(
    @arg(doc = "Write the document with less whitespace")
    compress: Flag,
    out: ConsoleCfg
): Unit = {
  // Read and overwrite the existing document without making any changes.
  writeGtd(
    GettingThingsDone(StatusContents, ProjectParserCfg),
    out,
    Some("doc(status): Beautify the document"),
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
// addWeek

@arg(doc = "Adds new weeks to the status document, up to the current date.")
@main
def addWeek(
    @arg(doc = "If set, just add one single new week instead of to the current date")
    single: Flag,
    out: ConsoleCfg
): Unit = {
  val gtdUpdated = if (!single.value) {
    val token = GettingThingsDone.Pattern.format(Instant.now())
    out.vPrintln(s"Updating the top week up to $token")
    Gtd.addWeek(Some(token))
  } else {
    // Simply add one week to the document.
    Gtd.addWeek(None)
  }
  val verb = if (Gtd == gtdUpdated) {
    println(
      out.ok(s"No weeks were added:", s"current top week is '${Gtd.topWeek.map(_.title).getOrElse("Unknown")}'")
    )
    "Update"
  } else "Add"

  writeGtd(
    gtdUpdated,
    out,
    Some(
      s"doc(status): $verb new week ${gtdUpdated.topWeek.map(_.title).getOrElse("")}"
    )
  )
}

// ==========================================================================
// link

@arg(doc = "Add a new link to this week.")
@main
def link(
    @arg(doc = "The http(s): link to add to the weekly status")
    linkUrl: String,
    @arg(doc = "The text corresponding to the link")
    linkText: String,
    @arg(doc = "The prefix for the link reference, which will have a -NN appended")
    linkRefPrefix: Option[String] = None,
    out: ConsoleCfg
): Unit = {
  val token = linkRefPrefix.getOrElse(GettingThingsDone.Pattern.format(Instant.now()).replaceAll("-", "")) + "-"

  val linkRef: String = Gtd.topWeek
    .map(weekly => {
      val linkRefs: Set[String] = weekly.mds.collect { case LinkRef(ref, _, _) if ref.startsWith(token) => ref }.toSet
      LazyList.from(1).map(i => s"$token$i").filterNot(linkRefs).head
    })
    .getOrElse(s"${token}1")
  val gtdWithLink = Gtd.updateTopWeek(weekly => weekly.copyMds(weekly.mds :+ LinkRef(linkRef, linkUrl, linkText)))

  val gtdUpdated = gtdWithLink.addTopWeekToDo("TODO", s"[$linkText][$linkRef]", MaybeToDo)

  writeGtd(gtdUpdated, out, Some(s"doc(status): Add '$linkText' to the weekly status"))
}

// ==========================================================================
// pr

@arg(doc = "Start working on a new PR")
@main
def pr(
    @arg(doc = "The tag for the project")
    tag: String,
    @arg(doc = "The PR number being worked on")
    prNum: String,
    @arg(doc = "The corresponding JIRA number being worked on")
    issueNum: String,
    @arg(doc = "A short description for the PR")
    description: String,
    @arg(doc = "The status of the work on the PR")
    status: String = "TOREVIEW",
    out: ConsoleCfg
): Unit = {
  // Use the project configuration for the tag, or create a default one for ASF projects
  val prj = PrjTasks.get(tag).getOrElse(PrjTask.asf(tag))

  // The reference and task snippets to add to the file.
  val fullIssue = if (issueNum != "0" && issueNum != "") Some(prj.issueRefOf(issueNum)) else None
  val fullPr = if (prNum != "0" && prNum != "") Some(prj.prRefOf(prNum)) else None
  val task = (fullIssue, fullPr) match {
    case (Some(refJira), Some(refPr)) => s"**[$refJira]**:[$refPr]"
    case (Some(refJira), None)        => s"**[$refJira]**"
    case (None, Some(refPr))          => s"[$refPr]"
    case (None, None)                 => ""
  }

  val gtdWithLinks = Gtd.updateHeader1("References") { refSection =>
    // Add the link references to the reference section.
    refSection.copyMds(
      fullIssue.map(LinkRef(_, prj.issueLinkOf(issueNum), description)).toSeq ++
        fullPr.map(LinkRef(_, prj.prLinkOf(prNum), description)) ++
        refSection.mds
    )
  }

  val gtdUpdated = gtdWithLinks.addTopWeekToDo(
    prj.title,
    s"$task $description `$status`",
    TextToToDoStates.getOrElse(status, MaybeToDo)
  )

  writeGtd(gtdUpdated, out, Some(s"doc(status): PR ${fullIssue.orElse(fullPr).getOrElse("")} $description"))
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
    date: Option[String] = None,
    out: ConsoleCfg
): Unit = {
  // TODO: If date is in a YYYY-MM-DD format, then to the correct date
  val gtdUpdated = Gtd.updateTopWeekStats(rowStat, cell, date)
  writeGtd(gtdUpdated, out, Some(s"doc(status): Update $rowStat"))
}

// ==========================================================================
// statsToday

@arg(doc = "Update many statistics for today.")
@main
def statsToday(
    out: ConsoleCfg,
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

  lazy val gtd = GettingThingsDone(os.read(StatusFile), ProjectParserCfg)
  val gtdUpdated = groupedStats.foldLeft(gtd) { (acc: GettingThingsDone, list: Seq[String]) =>
    acc.updateTopWeekStats(list.head, list.tail.headOption.getOrElse(""))
  }
  writeGtd(
    gtdUpdated,
    out,
    Some(s"doc(status): Update ${stats.grouped(2).map(_.head).mkString(",")}")
  )
}

// ==========================================================================
// statsDaily

@arg(doc = "Update the daily statistics.")
@main
def statsDaily(out: ConsoleCfg): Unit = {

  // Ensure we are on the current week to add today's stats.
  addWeek(Flag(false), out)

  // Get the daily stats configuration
  val cfgStats: Option[Table] = Gtd.cfg.flatMap(_.collectFirstRecursive {
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
  args.foreach(statsToday(out, _: _*))
}

/** Given an optional parameter, calculates the date range that corresponds to that month.
  *
  * If the month is not present, the range is two [[None]] values.
  *
  * If the month is positive, the range is the first and last day of that month in this year. January is "1" and
  * December is "12".
  *
  * If the month is zero or negative, the selected month is relative to the current month (where "0" is this month, and
  * "-1" is last month).
  *
  * @param month
  *   The optional month parameter.
  * @return
  *   The date range for the first and last day of that month, or None(s) if the parameter isn't specified.
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
  // If a specific month was set, then extract for that month only.
  val (from: Option[LocalDate], to: Option[LocalDate]) = getMonth(month)
  val stats = Gtd.extractStats(name = rowStat, from = from, to = to)

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
          TableRow.from("Date", "Stat", "Value") +: stats.map { case (date, stat, value) =>
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
        case Header(title, 2, _) if week.map(title.startsWith).getOrElse(title.length >= 10) =>
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
