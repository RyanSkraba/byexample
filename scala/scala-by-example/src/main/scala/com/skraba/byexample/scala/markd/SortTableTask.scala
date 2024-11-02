package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.jdk.CollectionConverters._
import scala.math.Ordered.orderingToOrdered
import scala.util.Try

/** Command-line driver for sorting a table in a Markdown file.
  */
object SortTableTask extends DocoptCliGo.Task {

  val Cmd = "sortTable"

  val Description = "Find a table in the markdown file and sort it."

  val Doc: String =
    """Find a table in the markdown file and sort it.
      |
      |Usage:
      |  MarkdGo sortTable FILE TABLE [--sortBy=COL]... [--failOnMissing]
      |
      |Options:
      |  -h --help        Show this screen.
      |  --version        Show version.
      |  FILE             File(s) to beautify.
      |  TABLE            The title of the table to sort, which is the first cell in
      |                   the first row. All tables with this title will e sorted, but
      |                   you can pick the Nth table by using a specifier such as
      |                   "TABLE:2".
      |  --sortBy=COL     A column name or number to sort by. This option can be
      |                   repeated to sort by multiple columns. [Default: 0].
      |  --failOnMissing  Fail if the table or column is not found.
      |
      |Tables and columns are zero-indexed.  MyTable:2 will sort the third table,
      | and sorting on column 0 sorts on the first column.
      |""".stripMargin.trim

  /** Specifies a way to sort a table by column
    * @param col
    *   A named column in the table or a number.
    * @param ascending
    *   true if the sort is ascending, false if descending.
    * @param numeric
    *   true if the sort should be as a number, false if as a string.
    */
  case class SortBy(col: String, ascending: Boolean, numeric: Boolean) {

    def sort(tbl: Table, failOnMissing: Boolean): Table = {
      // Find the index of the column to sort by, looking at the headers and matching a name first.
      val colNum = tbl.mds.head.cells.indexWhere(_ == col) match {
        case -1 => col.toIntOption.getOrElse(-1)
        case n  => n
      }

      if (colNum < 0 && failOnMissing)
        throw new IllegalArgumentException(s"Column not found in table '${tbl.title}': '$col'")
      else if (colNum < 0) tbl // Ignore if not failing.
      else
        tbl.copy(mds = tbl.mds.head +: tbl.mds.tail.sortWith { case (a, b) =>
          val cmp =
            if (numeric) a(colNum).toIntOption.compareTo(b(colNum).toIntOption)
            else a(colNum).compareTo(b(colNum))
          if (ascending) cmp < 0 else cmp > 0
        })
    }
  }

  object SortBy {
    // TODO(rskraba): Improve the handling of a specifier
    def apply(delimiter: String)(arg: String): SortBy = {
      arg.lastIndexOf(delimiter) match {
        case -1 => SortBy(arg, ascending = true, numeric = false)
        case index =>
          val specifier = arg.substring(index + 1)
          val ascending = specifier.toLowerCase() != "desc"
          val numeric = specifier.toLowerCase() == "num"
          SortBy(col = arg.substring(0, index), ascending = ascending, numeric = numeric)
      }
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val file: String = opts.get("FILE").asInstanceOf[String]
    val tableArg: String = opts.get("TABLE").asInstanceOf[String]
    val failOnMissing: Boolean = opts.get("--failOnMissing").toString.toBoolean
    val sortBys: Seq[SortBy] =
      opts.get("--sortBy").asInstanceOf[java.util.List[String]].asScala.toSeq.map(SortBy.apply(":"))

    val (table, tableNum) = tableArg.lastIndexOf(":") match {
      case -1 => (tableArg, None)
      case index if failOnMissing =>
        Try {
          (tableArg.substring(0, index), tableArg.substring(index + 1).toIntOption)
        }.getOrElse(throw new IllegalArgumentException(s"Bad table specifier: '$tableArg'"))
      case index => (tableArg.substring(0, index), tableArg.substring(index + 1).toIntOption)
    }

    // Fail fast if the table is not found.
    if (failOnMissing && tableNum.exists(_ < 0))
      throw new IllegalArgumentException(s"Bad table specifier: '$tableArg'")

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        var count = 0
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table && (tableNum.isEmpty || tableNum.contains(count)) =>
            count = count + 1
            // Apply the sorts in order starting from the last
            sortBys.foldRight(tbl)((sortBy, tbl) => sortBy.sort(tbl, failOnMissing))
          case tbl: Table if tbl.title == table && !tableNum.contains(count) =>
            count = count + 1
            tbl
        })

        if (failOnMissing && count == 0)
          throw new IllegalArgumentException(s"Table not found: '$tableArg'")
        if (failOnMissing && tableNum.exists(_ >= count))
          throw new IllegalArgumentException(s"Bad table specifier: '$tableArg'")

        f.writeAll(sorted.build().toString)
      }
    }
  }
}
