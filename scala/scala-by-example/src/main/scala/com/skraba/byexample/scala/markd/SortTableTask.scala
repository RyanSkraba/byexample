package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.jdk.CollectionConverters._

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

  // TODO(rskraba): Different sorts (alphabet, numeric)

  case class SortBy(col: String, ascending: Boolean) {

    /** Find the index of the column in the table, by priority searching by name and falling back to number.
      * @param tbl
      *   The table to look for the column in.
      * @return
      *   The index of the column in the table or -1 if it can't be found.
      */
    def find(tbl: Table): Int =
      tbl.mds.head.cells.indexWhere(_ == col) match {
        case -1 => col.toIntOption.getOrElse(-1)
        case n  => n
      }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val file: String = opts.get("FILE").asInstanceOf[String]
    val table: String = opts.get("TABLE").asInstanceOf[String]
    val failOnMissing: Boolean = opts.get("--failOnMissing").toString.toBoolean

    // TODO(rskraba): Sort ascending or descending
    val sortBys: Seq[SortBy] =
      opts.get("--sortBy").asInstanceOf[java.util.List[String]].asScala.toSeq.map(arg => SortBy(arg, ascending = true))

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        var found = false
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table =>
            found = true

            // Apply all of the sorts in order starting from the last
            sortBys.foldRight(tbl)((sortBy, tbl) => {
              val col = sortBy.find(tbl)
              if (col < 0 && failOnMissing)
                throw new IllegalArgumentException(s"Column not found in table '$table': '${sortBy.col}'")
              else if (col < 0) tbl
              else
                tbl.copy(mds = tbl.mds.head +: tbl.mds.tail.sortWith { case (a, b) =>
                  (if (sortBy.ascending) 1 else -1) * a(col).compareTo(b(col)) < 0
                })
            })
        })

        if (failOnMissing && !found)
          throw new IllegalArgumentException(s"Table not found: '$table'")

        f.writeAll(sorted.build().toString)
      }
    }
  }
}
