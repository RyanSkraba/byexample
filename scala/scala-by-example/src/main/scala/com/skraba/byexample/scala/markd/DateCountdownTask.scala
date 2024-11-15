package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters._
import scala.util.Try
import scala.util.matching.Regex

object DateCountdownTask extends DocoptCliGo.Task {

  val Cmd = "datecount"

  val Description = "Adjust countdown cells in tables."

  val Doc: String =
    """Look for tables with dates in a countdown format and fix them.
      |
      |Usage:
      |  MarkdGo datecount FILE...
      |
      |Options:
      |  -h --help       Show this screen.
      |  --version       Show version.
      |  FILE            File(s) to find and add date countdowns.
      |
      |If a table has a cell with a value "T 2024-06-03" (for example), all of the
      |cells in the same column with the format "T-100 2023-08-22" will have the date
      |adjusted to fit the countdown.  In this case, the value will be modified to
      |"T-100 2024-02-20".
      |
      |All of the cells in the same column with the format "T-XX 2023-08-22" will have
      |the countdown modified to fit the date.  In this case, the value would be
      |modified to "T-286 2023-08-22".
      |
      |Other cells will not be modified.
      |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {

    val files: Seq[String] = opts.x.get("FILE").asInstanceOf[java.lang.Iterable[String]].asScala.toSeq

    MarkdGo.processMd(files) { f =>
      f.writeAll(
        Header
          .parse(f.slurp())
          .replaceRecursively { case tbl: Table =>
            process(tbl)
          }
          .build()
          .toString
      )
    }
  }

  val YyyyMmDd: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  val TnumDateRegex: Regex =
    raw"(.*?)\b([*_(\[]*)T([-+]?[\d+X]+)?([*_)\]]*)\s+([*_(\[]*)(\d\d\d\d[-/]\d\d[-/]\d\d)([*_)\]]*)(.*)".r

  val DateTnumRegex: Regex =
    raw"(.*?)([*_(\[]*)(\d\d\d\d[-/]\d\d[-/]\d\d)([*_)\]]*)\s+([*_(\[]*)T([-+]?[\d+X]+)?([*_)\]]*)(.*)".r

  /** Given any table, update any rows with date information. */
  def process(tbl: Table): Table = {

    // A two dimensional ragged array of all the parsed table cells.
    val parsed = tbl.mds.map(row =>
      row.cells.map {
        case TnumDateRegex(before, preT, t, postT, pre, date, post, after) =>
          Some(
            (
              Try(if (t == null) 0 else t.toLong).getOrElse(Long.MaxValue),
              before,
              preT,
              t,
              postT,
              pre,
              date,
              post,
              after
            )
          )
        case DateTnumRegex(before, pre, date, post, preT, t, postT, after) =>
          Some(
            (
              Try(if (t == null) 0 else t.toLong).getOrElse(Long.MaxValue),
              before,
              preT,
              t,
              postT,
              pre,
              date,
              post,
              after
            )
          )
        case _ => None
      }
    )

    // Find the T0 date for each column, or None if there isn't any.  The T0 date is the measured relative
    // to the smallest magnitude T number discovered in the parsed cells for that column
    val t0s: Seq[Option[LocalDate]] = Seq.tabulate(tbl.colSize) { col =>
      parsed
        .filter(_.isDefinedAt(col))
        .flatMap(_(col))
        .sortBy(_._1.abs)
        .headOption
        .map(x => LocalDate.parse(x._7, YyyyMmDd).minusDays(x._1))
    }

    // For every single cell, if it has been parsed, then rewrite either the t number or the date according to the T0 value
    val mds = tbl.mds.zipWithIndex.map { row =>
      TableRow.from(row._1.cells.zipWithIndex.map {
        case (cell, col) if col < tbl.colSize && parsed.isDefinedAt(row._2) && parsed(row._2).isDefinedAt(col) =>
          parsed(row._2)(col)
            .map { x =>
              val base = t0s(col).getOrElse(LocalDate.now)

              // Resolve either the t number or the date according to the base.
              val (t, date) = if (x._1 == Long.MaxValue) {
                val date = LocalDate.parse(x._7, YyyyMmDd)
                (ChronoUnit.DAYS.between(base, date), date)
              } else { (x._1, base.plusDays(x._1)) }

              val number = if (t == 0) "T" else if (t > 0) s"T+$t" else s"T$t"

              s"${x._2}${x._3}$number${x._5} ${x._6}${YyyyMmDd.format(date)}${x._8}${x._9}"
            }
            .getOrElse(cell)
        case (cell, _) => cell
      }: _*)
    }
    tbl.copy(mds = mds)
  }

}
