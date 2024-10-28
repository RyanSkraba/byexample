package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.jdk.CollectionConverters._
import scala.math.Ordered.orderingToOrdered

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
      |  TABLE            The contents in the upper left cell of the table.
      |  --sortBy=COL     A column name or number to sort by.  This option can
      |                   be repeated to sort by multiple columns.
      |                   [Default: 0].
      |  --failOnMissing  Fail if the table or column is not found.
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
    val table: String = opts.get("TABLE").asInstanceOf[String]
    val failOnMissing: Boolean = opts.get("--failOnMissing").toString.toBoolean

    val sortBys: Seq[SortBy] =
      opts.get("--sortBy").asInstanceOf[java.util.List[String]].asScala.toSeq.map(SortBy.apply(":"))

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        var found = false
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table =>
            found = true
            // Apply the sorts in order starting from the last
            sortBys.foldRight(tbl)((sortBy, tbl) => sortBy.sort(tbl, failOnMissing))
        })

        if (failOnMissing && !found)
          throw new IllegalArgumentException(s"Table not found: '$table'")

        f.writeAll(sorted.build().toString)
      }
    }
  }
}
