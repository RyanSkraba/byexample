package com.skraba.byexample.scala.markd

import com.skraba.byexample.scala.markd.GettingThingsDone._

import java.time.DayOfWeek

/** Backing class for the getting_things_done.sc script. */
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
    * @param rowStat The name of the row to update in the statistics table.
    * @param cell The new value for that statistic.
    * @param colStat The day to apply that new value to.
    * @return This document with the statistics updated.
    */
  def updateTopWeekStats(
      rowStat: String,
      cell: String,
      colStat: Option[String] = None
  ): GettingThingsDone = updateTopWeek { weekly =>
    import java.time.LocalDate
    weekly.mapFirstIn(ifNotFound = TableStatsEmpty +: weekly.mds) {
      // Matches the table with the given name.
      case tb @ Table(_, Seq(TableRow(Seq(a1: String, _*)), _*))
          if a1 == TableStats =>
        val statsRow =
          tb.mds.indexWhere(_.cells.headOption.contains(rowStat))
        val row = if (statsRow != -1) statsRow else tb.mds.size

        val statsCol = colStat
          .map(c =>
            tb.mds.headOption
              .map(_.cells.indexWhere(_ == c))
              .getOrElse(-1)
          )
          .getOrElse(
            LocalDate.now.getDayOfWeek.getValue
          )
        val col =
          if (statsCol != -1) statsCol
          else LocalDate.now.getDayOfWeek.getValue

        if (statsRow == -1)
          tb.updated(col, row, cell).updated(0, row, rowStat)
        else tb.updated(col, row, cell)
    }
  }
}

object GettingThingsDone {

  /** The header with the weekly statuses. */
  val H1Weekly: String = "Weekly Status"

  val TableStats: String = "Stats"

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

  def apply(
      content: String,
      cfg: ParserCfg = new ParserCfg()
  ): GettingThingsDone = {
    GettingThingsDone(Header.parse(content, cfg))
  }

  /** Calculate either next Monday or the monday 7 days after the Date in the String. */
  def nextWeekStart(
      date: Option[String],
      dow: DayOfWeek = DayOfWeek.MONDAY
  ): String = {
    // Use the time classes to find the next date.
    import java.time.format.DateTimeFormatter
    import java.time.temporal.TemporalAdjusters
    import java.time.LocalDate
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
