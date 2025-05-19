package com.skraba.byexample.scala.markd
import com.tinfoiled.docopt4s.Task
import com.tinfoiled.markd._

import scala.util.Try
import scala.util.matching.Regex

/** Command-line driver for sorting a table in a Markdown file.
  */
object SortTableTask extends Task {

  val Cmd = "sortTable"

  val Description = "Find a table in the markdown file and sort it."

  val Doc: String =
    s"""Find a table in the markdown file and sort it.
       |
       |Usage:
       |  ${MarkdGo.Name} $Cmd FILE TABLE [COL...] [--ignore] [-]
       |
       |Options:
       |  -h --help  Show this screen.
       |  --version  Show version.
       |  FILE       File(s) to beautify.
       |  TABLE      The title of the table to sort, which is the first cell in the
       |             first row. All tables with this title will be sorted, but you can
       |             pick the Nth table by using a specifier such as "TABLE:2".
       |  COL        A column name or number to sort by. This option can be repeated to
       |             sort by multiple columns. [Default: 0].
       |  --ignore   Ignore missing tables or columns.
       |  -          If present, print to stdout instead of overwriting the file.
       |
       |Tables and columns are zero-indexed.  MyTable:2 will sort the third table,
       |and sorting on column 0 sorts on the first column.
       |""".stripMargin.trim

  /** Specifies a way to sort a table by column
    * @param col
    *   A named column in the table or a number.
    * @param ascending
    *   true if the sort is ascending, false if descending.
    * @param numeric
    *   true if the sort should be as a number, false if as a string.
    * @param ignoreCase
    *   true if the sort should ignore case.
    */
  case class SortBy(col: String, ascending: Boolean, numeric: Boolean, ignoreCase: Boolean) {

    def sort(tbl: Table, ignore: Boolean): Table = {
      // Find the index of the column to sort by, looking at the headers and matching a name first.
      val colNum = tbl.mds.head.cells.indexWhere(_ == col) match {
        case -1 => col.toIntOption.getOrElse(-1)
        case n  => n
      }

      if (colNum < 0 && !ignore)
        throw new IllegalArgumentException(s"Column not found in table '${tbl.title}': '$col'")
      else if (colNum < 0) tbl // Ignore if not failing.
      else {

        import java.text.Normalizer._

        val rows0 = tbl.mds.tail.map(tr => tr(colNum) -> tr)
        val rows1 = if (!ignoreCase) rows0 else rows0.map { case (cell, tr) => cell.toLowerCase -> tr }
        val rows =
          if (numeric) {
            rows1
              .map { case (cell, tr) => cell.toLongOption -> tr }
              .sortBy(_._1)(if (ascending) Ordering.Option[Long] else Ordering.Option[Long].reverse)
          } else {
            rows1
              .map { case (cell, tr) =>
                normalize(cell, Form.NFD).replaceAll("\\p{InCombiningDiacriticalMarks}", "") -> tr
              }
              .sortBy(_._1)(if (ascending) Ordering.String else Ordering.String.reverse)
          }

        tbl.copy(mds = tbl.mds.head +: rows.map(_._2))
      }
    }
  }

  object SortBy {

    val SortSpecifier: Regex =
      raw"^(?i)(|(asc|/)|(?<desc>desc|\\))(|(?<num>num|#)|(alpha|a))(|(?<caseless>i))$$".r

    def apply(delimiter: String)(arg: String): SortBy = {
      arg.lastIndexOf(delimiter) match {
        case -1 => SortBy(arg, ascending = true, numeric = false, ignoreCase = false)
        case index =>
          val specifier = arg.substring(index + 1)
          val (ascending, numeric, ignoreCase) = {
            SortSpecifier
              .findFirstMatchIn(specifier)
              .map(m => (m.group("desc") == null, m.group("num") != null, m.group("caseless") != null))
              .getOrElse(throw new IllegalArgumentException(s"Bad sort specifier: '$arg'"))
          }
          SortBy(col = arg.substring(0, index), ascending = ascending, numeric = numeric, ignoreCase = ignoreCase)
      }
    }
  }

  def go(opts: TaskOptions): Unit = {

    val file: String = opts.getString("FILE")
    val tableArg: String = opts.getString("TABLE")
    val ignore: Boolean = opts.getBoolean("--ignore")

    val (stdout, sortByArgs) = {
      val cols = opts.getStrings("COL")
      if (cols.lastOption.contains("-")) (true, cols.init)
      else (false, cols)
    }
    val sortBys: Iterable[SortBy] = (if (sortByArgs.isEmpty) Seq("0") else sortByArgs).map(SortBy.apply(":"))

    val (table, tableNum) = tableArg.lastIndexOf(":") match {
      case -1 => (tableArg, None)
      case index if !ignore =>
        Try {
          (tableArg.substring(0, index), tableArg.substring(index + 1).toIntOption)
        }.getOrElse(throw new IllegalArgumentException(s"Bad table specifier: '$tableArg'"))
      case index => (tableArg.substring(0, index), tableArg.substring(index + 1).toIntOption)
    }

    // Fail fast if the table is not found.
    if (!ignore && tableNum.exists(_ < 0))
      throw new IllegalArgumentException(s"Bad table specifier: '$tableArg'")

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        var count = 0
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table && (tableNum.isEmpty || tableNum.contains(count)) =>
            count = count + 1
            // Apply the sorts in order starting from the last
            sortBys.foldRight(tbl)((sortBy, tbl) => sortBy.sort(tbl, ignore))
          case tbl: Table if tbl.title == table && !tableNum.contains(count) =>
            count = count + 1
            tbl
        })

        if (!ignore && count == 0)
          throw new IllegalArgumentException(s"Table not found: '$tableArg'")
        if (!ignore && tableNum.exists(_ >= count))
          throw new IllegalArgumentException(s"Bad table specifier: '$tableArg'")

        if (stdout) print(sorted.build()) else f.writeAll(sorted.build().toString)
      }
    }
  }
}
