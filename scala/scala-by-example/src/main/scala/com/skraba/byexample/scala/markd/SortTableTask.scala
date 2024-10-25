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

  // TODO(rskraba): Sort ascending or descending
  // TODO(rskraba): Different sorts (alphabet, numeric)

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val file: String = opts.get("FILE").asInstanceOf[String]
    val table: String = opts.get("TABLE").asInstanceOf[String]
    val failOnMissing: Boolean = opts.get("--failOnMissing").toString.toBoolean
    val sortByCol: Seq[String] = opts.get("--sortBy").asInstanceOf[java.util.List[String]].asScala.toSeq

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        var found = false
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table =>
            found = true
            // Use the header in the first matching table to convert the columns into numbers
            val sortByColNum: Seq[Int] =
              sortByCol.map(col => {
                tbl.mds.head.cells.indexWhere(_ == col) match {
                  case -1 => col.toIntOption.getOrElse(-1)
                  case n  => n
                }
              })

            if (failOnMissing) {
              val invalidIndex = sortByColNum.indexWhere(!tbl.mds.head.cells.indices.contains(_))
              if (invalidIndex != -1)
                throw new IllegalArgumentException(
                  s"Column names or numbers not found in table '$table': '${sortByCol(invalidIndex)}'"
                )
            }

            sortByColNum.foldRight(tbl)((col, tbl) => {
              if (col < 0) tbl
              else
                tbl.copy(mds = tbl.mds.head +: tbl.mds.tail.sortWith { case (a, b) => a(col).compareTo(b(col)) < 0 })
            })
        })

        if (failOnMissing && !found)
          throw new IllegalArgumentException(s"Table not found: '$table'")

        f.writeAll(sorted.build().toString)
      }
    }
  }
}
