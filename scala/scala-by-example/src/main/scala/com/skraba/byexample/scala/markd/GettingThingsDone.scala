package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.GettingThingsDone._

import java.time.DayOfWeek

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
  * | To Do      | Notes                                                       |
  * |------------|-------------------------------------------------------------|
  * | 🟢Tech     | **Did the thing** and some notes                            |
  * | 🔶Personal | ****Ready to take** or paused, or to think about            |
  * | 🟥Health   | **Won't do** and here's why                                 |
  * | ⤴️Personal | **Read Getting Things Done Chapter 4/12** Moved to later    |
  * | ⌛Proj      | **[PROJ-1234]**:[Proj PR#4321] Fix all the things `WAITING` |
  * | Pro        | **Another task** With some [details][YYYYMMDD-1]            |
  * }}}
  */
case class GettingThingsDone(doc: Header) {

  /** The top heading (H1) section containing all of the weekly statuses. */
  lazy val weeklies: Option[Header] = {
    doc.mds.collectFirst {
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
    copy(doc = doc.mapFirstIn(ifNotFound = doc.mds :+ Header(1, H1Weeklies)) {
      case weeklies @ Header(title, 1, _) if title.startsWith(H1Weeklies) =>
        fn(weeklies)
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
        weeklies.mds :+ Header(2, GettingThingsDone.nextWeekStart(None))
      ) { case topWeek @ Header(_, 2, _) =>
        fn(topWeek)
      }
    }

  /** Update a statistics table in the top week.
    * @param row
    *   The name of the row to update in the statistics table.
    * @param cell
    *   The new value for that statistic.
    * @param col
    *   The day to apply that new value to.
    * @return
    *   This document with the statistics updated.
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
    weekly.mapFirstIn(ifNotFound = TableToDoEmpty +: weekly.mds) {
      // Matches the table with the given name.
      case tb @ Table(_, Seq(TableRow(Seq(a1: String, _*)), _*))
          if a1 == TableToDo =>
        // Insert a new row if prepending to the top
        val tbToUpdate =
          if (row >= 0) tb
          else tb.copyMds(tb.mds.patch(1, Seq(TableRow(Seq.empty)), 0))

        // Find the actual row to be updated
        val nRow = {
          if (row == Int.MaxValue || row + 1 > tb.mds.size) tb.mds.size
          else if (row < 0) 1
          else row + 1
        }

        // Extract the state and category from the first row column
        val (oldState, oldCategory, oldNotes) = (tb.mds.lift(nRow) match {
          case Some(TableRow(Seq(cat, n, _*))) =>
            (for (state <- AllStates if cat.startsWith(state.txt))
              yield (state, cat.drop(state.txt.length), n)).headOption
          case _ => None
        }).getOrElse(NoToDoState, "", "")

        tbToUpdate
          .updated(
            0,
            nRow,
            state.getOrElse(oldState).txt + category.getOrElse(oldCategory)
          )
          .updated(1, nRow, notes.getOrElse(oldNotes))
    }
  }
}

object GettingThingsDone {

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
    TableRow.from(TableToDo, "Notes")
  )

  /** The header with the weekly statuses. */
  val H1Weeklies: String = "Weekly Status"

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
  ): GettingThingsDone = {
    GettingThingsDone(Header.parse(content, cfg))
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
        "Personal",
        "****Ready to take** or paused, or to think about",
        state = MaybeToDo
      )
      .addTopWeekToDo(
        "Health",
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
        "**[PROJ-1234]**:[Proj PR#4321] Fix all the things `WAITING`",
        state = WaitingToDo
      )
      .addTopWeekToDo(
        "Pro",
        "**Another task** With some [details][YYYYMMDD-1] "
      )

    // Extract the Table as text.
    val tableToDoExampleComment = tableToDoExample.doc.mds
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

  sealed class ToDoState(val txt: String)
  case object NoToDoState extends ToDoState("")
  case object DoneToDo extends ToDoState("🟢")
  case object MaybeToDo extends ToDoState("🔶")
  case object StoppedToDo extends ToDoState("🟥")
  case object WaitingToDo extends ToDoState("⌛")
  case object LaterToDo extends ToDoState("⤴️")
  val AllStates: Seq[ToDoState] =
    Seq(DoneToDo, MaybeToDo, StoppedToDo, LaterToDo, NoToDoState)

  /** Calculate either next Monday or the monday 7 days after the Date in the
    * String.
    */
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
