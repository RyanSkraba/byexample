package com.skraba.byexample.scala.gtd

import com.skraba.byexample.scala.gtd.GettingThingsDone._
import com.skraba.byexample.scala.markd.{Comment, _}

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate}
import scala.util.Try

/** A markdown document that helps organising yourself.
  *
  * The document can contain any number of headers and sections using the
  * [[Markd]] model, but provides some additional semantics.
  *
  * The {{{getting_things_done.sc}}} ammonite script uses this class to provide
  * a CLI.
  *
  * =Weekly Status (a.k.a. weeklies)=
  *
  * A top level (Header 1) section that contains a subsection for each week.
  * Some methods in this class are used to update the latest or a specific week.
  *
  * {{{
  * Weekly Status
  * ==============================================================================
  *
  * 2021/09/13
  * ------------------------------------------------------------------------------
  *
  * <... This weeks status reported here.  This can contain statistics tables,
  * to do lists, notes, links, etc. ...>
  *
  * 2021/09/6
  * ------------------------------------------------------------------------------
  *
  * <... This weeks status reported here. ...>
  * }}}
  *
  * =Statistic tables=
  *
  * In a weekly status, you can have per-day statistics in a condensed table.
  * Helper methods make it easy to add a new row or update a value for today.
  *
  * {{{
  * 2021/09/13
  * ------------------------------------------------------------------------------
  *
  * | Stats  | Mon | Tue | Wed | Thu | Fri | Sat | Sun |
  * |--------|-----|-----|-----|-----|-----|-----|-----|
  * | unread |     |     |     | 449 |     |     |     |
  * }}}
  * =To Do task tables=
  *
  * In a weekly status, you can list tasks in a table, along with a status.
  *
  * {{{
  * 2021/09/13
  * ------------------------------------------------------------------------------
  *
  * | To Do      | Notes 🟢🔶🟥⤴️🕒                                             |
  * |------------|--------------------------------------------------------------|
  * | 🟢Tech     | **Did the thing** and some notes                             |
  * | 🔶Health   | **Ready to take** or paused, or to think about               |
  * | 🟥Personal | **Won't do** and here's why                                  |
  * | ⤴️Personal | **Read Getting Things Done Chapter 4/12** Moved to later     |
  * | 🕒Proj     | **[PROJ-1234]**:[org/proj#4321] Fix all the things `WAITING` |
  * | Pro        | **Another task** With some [details][YYYYMMDD-1]             |
  *
  * }}}
  */
case class GettingThingsDone(h0: Header, cfg: Option[Header]) {

  /** The top heading (H1) section containing all of the weekly statuses. */
  lazy val weeklies: Option[Header] = {
    h0.mds.collectFirst {
      case weeklies @ Header(title, 1, _) if title.startsWith(H1Weeklies) =>
        weeklies
    }
  }

  /** The last weekly status. */
  lazy val topWeek: Option[Header] = {
    weeklies.flatMap(_.mds.collectFirst { case weekly @ Header(title, 2, _) =>
      weekly
    })
  }

  /** Helper function to update only the weekly statuses section of the
    * document, adding the top-level section if necessary. All of the weekly
    * statuses should be contained in returned section.
    *
    * @param fn
    *   A function that modifies only the weekly statuses (a Header 1)
    * @return
    *   The entire document with only the function applied to the weekly
    *   statuses.
    */
  def updateWeeklies(fn: Header => Header): GettingThingsDone =
    copy(h0 = h0.mapFirstIn(ifNotFound = Header(1, H1Weeklies)) {
      case weeklies @ Header(title, 1, _) if title.startsWith(H1Weeklies) =>
        fn(weeklies)
    })

  /** Helper function to update or add a top-level section (Header 1) that
    * exactly matches the given name. If it isn't present, it will be added to
    * the bottom of the document.
    * @param name
    *   The name of the top level section
    * @param fn
    *   A function to apply to the section.
    * @return
    *   The entire document with the function applied to that top-level section.
    */
  def updateHeader1(name: String)(fn: Header => Header): GettingThingsDone =
    copy(h0 = h0.mapFirstIn(ifNotFound = Header(1, name)) {
      case h1 @ Header(`name`, 1, _) => fn(h1)
    })

  /** Helper function to update only the last week section of the statuses
    * document, adding one if necessary.
    *
    * @param fn
    *   A function that modifies only the weekly statuses (a Header 2)
    * @return
    *   The entire document with only the function applied to the last week.
    */
  def updateTopWeek(fn: Header => Header): GettingThingsDone =
    updateWeeklies { weeklies =>
      weeklies.mapFirstIn(ifNotFound =
        Seq(Header(2, GettingThingsDone.nextWeekStart(None)))
      ) { case topWeek @ Header(_, 2, _) =>
        fn(topWeek)
      }
    }

  /** Update a statistics table in the top week.
    * @param rowHead
    *   The name of the row to update in the statistics table.
    * @param cell
    *   The new value for that statistic.
    * @param col
    *   The day to apply that new value to.
    * @return
    *   This document with the statistics updated.
    */
  def updateTopWeekStats(
      rowHead: String,
      cell: String,
      col: Option[String] = None
  ): GettingThingsDone = updateTopWeek { weekly =>
    import java.time.LocalDate
    weekly.mapFirstIn(
      replace = true,
      ifNotFound = TableStatsEmpty +: weekly.mds
    ) {
      // Matches the table with the given name.
      case tbl: Table if tbl.title == TableStats =>
        col match {
          case Some(dow) => tbl.updated(dow, rowHead, cell)
          case None =>
            tbl.updated(LocalDate.now.getDayOfWeek.getValue, rowHead, cell)
        }
    }
  }

  /** Add a To Do task in the top week.
    * @param category
    *   The major category for the task.
    * @param notes
    *   The notes to apply to the task.
    * @param state
    *   The state of the task.
    * @return
    *   This document with the To Do table updated.
    */
  def addTopWeekToDo(
      category: String,
      notes: String,
      state: ToDoState = NoToDoState
  ): GettingThingsDone =
    updateTopWeekToDo(Int.MaxValue, Some(category), Some(notes), Some(state))

  /** Add or updates a To Do task in the top week.
    * @param row
    *   the number of the row to update. Add to the end if it doesn't exist, or
    *   to the start if is it negative.
    * @param category
    *   The major category for the task, or None to not update the category.
    * @param notes
    *   The notes to apply to the task, or None to not update the notes.
    * @param state
    *   The state of the task, or None to not update the state.
    * @return
    *   This document with the To Do table updated.
    */
  def updateTopWeekToDo(
      row: Int,
      category: Option[String] = None,
      notes: Option[String] = None,
      state: Option[ToDoState] = None
  ): GettingThingsDone = updateTopWeek { weekly =>
    weekly.mapFirstIn(
      replace = true,
      ifNotFound = TableToDoEmpty +: weekly.mds
    ) {
      // Matches the table with the given name.
      case tbl: Table if tbl.title == TableToDo =>
        // Insert a new row if prepending to the top
        val tblToUpdate =
          if (row >= 0) tbl
          else tbl.copyMds(tbl.mds.patch(1, Seq(TableRow(Seq.empty)), 0))

        // Find the actual row to be updated
        val nRow = {
          if (row == Int.MaxValue || row + 1 > tbl.mds.size) tbl.mds.size
          else if (row < 0) 1
          else row + 1
        }

        // Extract the state and category from the first row column
        val (oldState, oldCategory, oldNotes) = (tbl.mds.lift(nRow) match {
          case Some(TableRow(Seq(cat, n, _*))) =>
            (for (state <- AllStates if cat.startsWith(state.txt))
              yield (state, cat.drop(state.txt.length), n)).headOption
          case _ => None
        }).getOrElse(NoToDoState, "", "")

        tblToUpdate
          .updated(
            0,
            nRow,
            state.getOrElse(oldState).txt + category.getOrElse(oldCategory)
          )
          .updated(1, nRow, notes.getOrElse(oldNotes))
    }
  }

  /** Add a new week to the top of the weeklies section. Tasks and stats are
    * rolled over, cleaned and pruned correctly.
    *
    * @return
    *   A document with the new week present.
    */
  def newWeek(): GettingThingsDone = {

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
                      if row > 0 && ToDoState(taskText).complete =>
                    Seq.empty
                })
            }
        }
        .getOrElse(Header(2, nextWeekStart(None)))
    }

    /** Create the new head week from the last week, if any is present. */
    def updateLastHead(oldWeek: Header): Header = {
      oldWeek
        .replaceIn() {
          // Copy the To Do table, but update all maybe tasks.
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
    updateWeeklies { weeklies =>
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
  }

  def extractStats(
      name: String,
      from: Option[LocalDate] = None,
      to: Option[LocalDate] = None
  ): Seq[(String, String)] = {

    // Find all of the weekly reports
    weeklies.toSeq
      .flatMap(_.mds.collect { case weekly @ Header(title, 2, _) =>
        // If the title is a parseable date
        Try {
          LocalDate.parse(title.substring(0, 10), Pattern)
        }.toOption.toSeq
          .flatMap { startOfWeek: LocalDate =>
            // Then find the Stats table in the weekly report
            weekly.mds
              .collectFirst {
                case tbl: Table if tbl.title == TableStats =>
                  tbl(name).cells.zipWithIndex.collect {
                    case (value, i) if (i > 0 && value.nonEmpty) =>
                      // And all the non-empty values in the table
                      (startOfWeek.plusDays(i - 1), value)
                  }
              }
              .getOrElse(Nil)
              // Filter by the dates if any are specified
              .filter { case (d, _) => from.forall(_.compareTo(d) <= 0) }
              .filter { case (d, _) => to.forall(_.compareTo(d) >= 0) }
              .map { case (d, v) => (d.format(Pattern), v) }
          }
      })
      .flatten
      .sortBy(_._1)
  }
}

object GettingThingsDone {

  /** A date pattern */
  val Pattern: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy/MM/dd")

  /** The name of the statistics table, the value found in the upper left
    * column.
    */
  val TableStats: String = "Stats"

  /** The name of the tasks table, the value found in the upper left column. */
  val TableToDo: String = "To Do"

  /** The structure of an empty Stats table, used to collect weekly statistics.
    */
  lazy val TableStatsEmpty: Table = Table.from(
    Seq.fill(8)(Align.LEFT),
    TableRow.from(
      TableStats,
      "Mon",
      "Tue",
      "Wed",
      "Thu",
      "Fri",
      "Sat",
      "Sun"
    )
  )

  /** The structure of an empty To Do table, used to collect stats. */
  lazy val TableToDoEmpty: Table = Table.from(
    Seq(Align.LEFT, Align.LEFT),
    TableRow.from(TableToDo, "Notes " + AllStates.map(_.txt).mkString)
  )

  /** The header with the weekly statuses. */
  val H1Weeklies: String = "Weekly Status"

  /** A tag for a configuration comment, which can be found anywhere in the
    * document
    */
  val CommentConfig: String = "Getting Things Done configuration"

  /** Create an instance from a markdown string.
    *
    * @param content
    *   The string contents
    * @param cfg
    *   Helper for configuring the model
    * @return
    *   A [[GettingThingsDone]] based on the model
    */
  def apply(
      content: String,
      cfg: ParserCfg = new ParserCfg()
  ): GettingThingsDone = GettingThingsDone(Header.parse(content, cfg))

  /** Create an instance from a single Header markdown element.
    *
    * If there a configuration comment is discovered in the model, it will be
    * extracted and used, and the comment will be reformatted correctly.
    *
    * @param h0
    *   The main markdown element
    * @return
    *   A [[GettingThingsDone]] based on the model
    */
  def apply(h0: Header): GettingThingsDone = {
    // If there is a configuration section, then extract it and reformat it internally to the doc.
    val gtdWithConfig: Option[GettingThingsDone] = h0.collectFirstRecursive {
      case Comment(gtdCfgContent)
          if gtdCfgContent.trim.startsWith(CommentConfig) =>
        // Parse the config section as a
        val gtdConfigSection = Header.parse(gtdCfgContent)
        // Rewrite the document with the comment formatted.
        val reformatted: Header = h0.mapFirstIn() { case h1 @ Header(_, 1, _) =>
          h1.mapFirstIn() {
            case Comment(content) if content.trim.startsWith(CommentConfig) =>
              Comment(" " + gtdConfigSection.build().toString + "\n")
          }
        }
        GettingThingsDone(reformatted, Some(gtdConfigSection))
    }

    gtdWithConfig.getOrElse(GettingThingsDone(h0, None))
  }

  /** @return
    *   A clean, useful status document to start from.
    */
  def apply(): GettingThingsDone = {
    // Create an example table in a document
    val tableToDoExample = GettingThingsDone("")
      .addTopWeekToDo(
        "Tech",
        "**Did the thing** and some notes",
        state = DoneToDo
      )
      .addTopWeekToDo(
        "Finance",
        "**Thing was did** Done by someone else, or worked around",
        state = DoneSimpleToDo
      )
      .addTopWeekToDo(
        "Health",
        "**Ready to take** or paused, or to think about",
        state = MaybeToDo
      )
      .addTopWeekToDo(
        "Personal",
        "**Won't do** and here's why",
        state = StoppedToDo
      )
      .addTopWeekToDo(
        "Personal",
        "**Read Getting Things Done Chapter 4/12** Moved to later",
        state = LaterToDo
      )
      .addTopWeekToDo(
        "Proj",
        "**[PROJ-1234]**:[org/proj#4321] Fix all the things `WAITING`",
        state = WaitingToDo
      )
      .addTopWeekToDo(
        "Pro",
        "**Another task** With some [details][YYYYMMDD-1] "
      )

    // Extract the Table as text.
    val tableToDoExampleComment = tableToDoExample.h0.mds
      .collectFirst {
        case Header(_, _, Seq(Header(_, _, Seq(tb @ Table(_, _))))) => tb
      }
      .map("\n" + _.build().toString)
      .map(Comment)

    GettingThingsDone(
      Header(0, "", Header(1, H1Weeklies, tableToDoExampleComment.get))
    ).updateTopWeekStats("pushups", "").updateTopWeek { weekly =>
      weekly.copyMds(weekly.mds :+ Paragraph("* Something I did this week"))
    }
  }

  /** A state can be deduced from a task by the prefix of the category.
    *
    * @param txt
    *   The prefix to match for the category.
    * @param complete
    *   True if the state means that the task is finished this week.
    */
  sealed class ToDoState(val txt: String, val complete: Boolean = false)
  case object NoToDoState extends ToDoState("")
  case object DoneToDo extends ToDoState("🟢", complete = true)
  case object DoneSimpleToDo extends ToDoState("🔵", complete = true)
  case object MaybeToDo extends ToDoState("🔶")
  case object StoppedToDo extends ToDoState("🟥", complete = true)
  case object WaitingToDo extends ToDoState("🕒")
  case object LaterToDo extends ToDoState("⤴️")
  val AllStates: Seq[ToDoState] =
    Seq(
      DoneToDo,
      DoneSimpleToDo,
      MaybeToDo,
      StoppedToDo,
      LaterToDo,
      WaitingToDo,
      NoToDoState
    )

  object ToDoState {
    def apply(category: String): ToDoState =
      AllStates.find(tds => category.startsWith(tds.txt)).getOrElse(NoToDoState)
  }

  /** Calculate either next Monday or the monday 7 days after the Date in the
    * String.
    */
  def nextWeekStart(
      date: Option[String],
      dow: DayOfWeek = DayOfWeek.MONDAY
  ): String = {
    // Use the time classes to find the next date.
    import java.time.LocalDate._
    ofEpochDay(
      nextWeekStartByEpoch(
        date.map(ptn => parse(ptn.substring(0, 10), Pattern).toEpochDay),
        dow
      )
    ).format(Pattern)
  }

  def nextWeekStartByEpoch(
      epoch: Option[Long] = None,
      dow: DayOfWeek = DayOfWeek.MONDAY
  ): Long = {
    // Use the time classes to find the next date.
    import java.time.LocalDate
    import java.time.temporal.TemporalAdjusters
    val monday = epoch
      .map(LocalDate.ofEpochDay)
      .getOrElse(LocalDate.now)
      .plusDays(1)
      .`with`(TemporalAdjusters.previous(dow))
      .plusDays(7)
      .toEpochDay
    monday
  }
}
