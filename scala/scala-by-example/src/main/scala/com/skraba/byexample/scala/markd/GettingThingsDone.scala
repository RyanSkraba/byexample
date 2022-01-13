package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.GettingThingsDone._

import java.time.DayOfWeek

/** A markdown document that helps organising yourself.
  *
  * The document can contain any number of headers and sections using the [[Markd]] model, but
  * provides some additional semantics.
  *
  * The {{{getting_things_done.sc}}} ammonite script uses this class to provide a CLI.
  *
  * =Weekly Status=
  *
  * A top level (Header 1) section that contains a subsection for each week. Some methods in
  * this class are used to update the latest or a specific week.
  *
  * {{{
  * Weekly Status
  * ==============================================================================
  *
  * 2021/09/13
  * ------------------------------------------------------------------------------
  *
  * <... weekly status reported here ...>
  *
  * 2021/09/6
  * ------------------------------------------------------------------------------
  *
  * <... weekly status reported here ...>
  * }}}
  *
  * =Statistic tables=
  *
  * In a weekly status, you can have per-day statistics in a condensed table.  Helper methods
  * make it easy to add a new row or update a value for today.
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
  * | To Do      | Notes                                                                     |
  * |------------|---------------------------------------------------------------------------|
  * | 🟢Tech     | **Do the thing** Notes on how it was done                                 |
  * | 🔶Personal | **Maybe doable** Or paused, or to think about for next week, or in danger |
  * | 🟥Health   | **Not done** Here's why                                                   |
  * | ⤴️Personal | **Read Getting Things Done Chapter 4/12** Moved to next week              |
  * | Pro        | **Another task** With some [details][20210913-1]                          |
  * }}}
  */
case class GettingThingsDone(doc: Header) {

  /** Helper function to update only the weekly statuses section of the document, adding
    * it if necessary.
    *
    * @param fn A function that modifies only the weekly statuses (a Header 1)
    * @return The entire document with only the function applied to the weekly statuses.
    */
  def updateH1Weekly(fn: Header => Header): GettingThingsDone =
    GettingThingsDone(
      doc.mapFirstIn(ifNotFound = doc.mds :+ Header(1, H1Weekly)) {
        case weekly @ Header(title, 1, _) if title.startsWith(H1Weekly) =>
          fn(weekly)
      }
    )

  /** Helper function to update only the last week section of the statuses document, adding one
    * if necessary.
    *
    * @param fn A function that modifies only the weekly statuses (a Header 2)
    * @return The entire document with only the function applied to the last week.
    */
  def updateTopWeek(fn: Header => Header): GettingThingsDone =
    updateH1Weekly { weeklies =>
      weeklies.mapFirstIn(ifNotFound =
        weeklies.mds :+ Header(2, GettingThingsDone.nextWeekStart(None))
      ) { case topWeek @ Header(_, 2, _) =>
        fn(topWeek)
      }
    }

  /** Update a statistics table in the top week.
    * @param row The name of the row to update in the statistics table.
    * @param cell The new value for that statistic.
    * @param col The day to apply that new value to.
    * @return This document with the statistics updated.
    */
  def updateTopWeekStats(
      row: String,
      cell: String,
      col: Option[String] = None
  ): GettingThingsDone = updateTopWeek { weekly =>
    import java.time.LocalDate
    weekly.mapFirstIn(ifNotFound = TableStatsEmpty +: weekly.mds) {
      // Matches the table with the given name.
      case tb @ Table(_, Seq(TableRow(Seq(a1: String, _*)), _*))
          if a1 == TableStats =>
        val statsRow =
          tb.mds.indexWhere(_.cells.headOption.contains(row))
        val nRow = if (statsRow != -1) statsRow else tb.mds.size

        val statsCol = col
          .map(c =>
            tb.mds.headOption
              .map(_.cells.indexWhere(_ == c))
              .getOrElse(-1)
          )
          .getOrElse(
            LocalDate.now.getDayOfWeek.getValue
          )
        val nCol =
          if (statsCol != -1) statsCol
          else LocalDate.now.getDayOfWeek.getValue

        if (statsRow == -1)
          tb.updated(nCol, nRow, cell).updated(0, nRow, row)
        else tb.updated(nCol, nRow, cell)
    }
  }

  /** Add a To Do task in the top week.
    * @param category The major category for the task.
    * @param notes The notes to apply to the task.
    * @param state The state of the task.
    * @return This document with the To Do table updated.
    */
  def updateTopWeekToDo(
      category: String,
      notes: String,
      state: ToDoState = NoToDoState
  ): GettingThingsDone = updateTopWeek { weekly =>
    weekly.mapFirstIn(ifNotFound = TableToDoEmpty +: weekly.mds) {
      // Matches the table with the given name.
      case tb @ Table(_, Seq(TableRow(Seq(a1: String, _*)), _*))
          if a1 == TableToDo =>
        val row = tb.mds.size
        tb.updated(0, row, state.txt + category).updated(1, row, notes)
    }
  }
}

object GettingThingsDone {

  /** The structure of an empty Stats table, used to collect weekly statistics. */
  lazy val TableStatsEmpty: Table = Table.from(
    Seq.fill(8)(Align.LEFT),
    TableRow.from(
      "Stats",
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
    TableRow.from("To Do", "Notes")
  )

  /** The header with the weekly statuses. */
  val H1Weekly: String = "Weekly Status"
  val TableStats: String = TableStatsEmpty.mds.head.cells.head
  val TableToDo: String = TableToDoEmpty.mds.head.cells.head

  /** Create an instance from a markdown string.
    *
    * @param content The string contents
    * @param cfg
    * @return
    */
  def apply(
      content: String,
      cfg: ParserCfg = new ParserCfg()
  ): GettingThingsDone = {
    GettingThingsDone(Header.parse(content, cfg))
  }

  sealed class ToDoState(val txt: String)
  case object NoToDoState extends ToDoState("")
  case object DoneToDo extends ToDoState("🟢")
  case object MaybeToDo extends ToDoState("🔶")
  case object StoppedToDo extends ToDoState("🟥")
  case object LaterToDO extends ToDoState("⤴️")

  /** Calculate either next Monday or the monday 7 days after the Date in the String. */
  def nextWeekStart(
      date: Option[String],
      dow: DayOfWeek = DayOfWeek.MONDAY
  ): String = {
    // Use the time classes to find the next date.
    import java.time.LocalDate
    import java.time.format.DateTimeFormatter
    import java.time.temporal.TemporalAdjusters
    val pattern = DateTimeFormatter.ofPattern("yyyy/MM/dd")
    val monday = date
      .map(ptn => LocalDate.parse(ptn.substring(0, 10), pattern))
      .getOrElse(LocalDate.now)
      .plusDays(1)
      .`with`(TemporalAdjusters.previous(dow))
      .plusDays(7)
      .format(pattern)
    monday
  }

}
