package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.jdk.CollectionConverters._
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
      |  MarkdGo sortTable FILE TABLE [--sortBy=COLS]
      |
      |Options:
      |  -h --help       Show this screen.
      |  --version       Show version.
      |  FILE            File(s) to beautify.
      |  TABLE           The contents in the upper left cell of the table.
      |  --sortBy=COLS   The column name or number to sort by [Default: 0].
      |""".stripMargin.trim

  // TODO(rskraba): Sort by multiple columns separated by ,
  // TODO(rskraba): Sort ascending or descending
  // TODO(rskraba): Different sorts (alphabet, numeric)

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val file: String = opts.get("FILE").asInstanceOf[String]
    val table: String = opts.get("TABLE").asInstanceOf[String]

    val sortByCol: Seq[String] = opts.get("--sortBy").asInstanceOf[String].split(",").toSeq

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table =>
            // Use the header in the first matching table to convert the columns into numbers
            val sortByColNum: Seq[Int] =
              sortByCol.flatMap(col => {
                tbl.mds.head.cells.indexWhere(_ == col) match {
                  case -1 => col.toIntOption
                  case n  => Some(n)
                }
              })
            // Just sort by the first discovered column for now
            val sortBy = sortByColNum.headOption.getOrElse(Int.MaxValue)
            tbl.copy(mds = tbl.mds.head +: tbl.mds.tail.sortWith((a, b) => a(sortBy).compareTo(b(sortBy)) < 0))
        })
        f.writeAll(sorted.build().toString)
      }
    }
  }
}
