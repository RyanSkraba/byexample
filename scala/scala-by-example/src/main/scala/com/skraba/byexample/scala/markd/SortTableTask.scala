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
      |  MarkdGo sortTable FILE TABLE [--sortBy=COLS]
      |
      |Options:
      |  -h --help       Show this screen.
      |  --version       Show version.
      |  FILE            File(s) to beautify.
      |  TABLE           The contents in the upper left cell of the table.
      |  --sortBy=COLS   The column number to sort by [Default: 0].
      |""".stripMargin.trim

  // TODO(rskraba): Sort by multiple columns separated by ,
  // TODO(rskraba): Sort by column names or numbers separated by ,
  // TODO(rskraba): Sort ascending or descending
  // TODO(rskraba): Different sorts (alphabet, numeric)

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val file: String = opts.get("FILE").asInstanceOf[String]
    val table: String = opts.get("TABLE").asInstanceOf[String]
    val sortBy: Int = opts.get("--sortBy").asInstanceOf[String].toInt

    MarkdGo.processMd(Seq(file)) { f =>
      {
        val md = Header.parse(f.slurp())
        val sorted = md.replaceRecursively({
          case tbl: Table if tbl.title == table =>
            tbl.copy(mds =
              tbl.mds.head +: tbl.mds.tail.sortWith((a, b) => a.cells(sortBy).compareTo(b.cells(sortBy)) < 0)
            )
        })
        f.writeAll(sorted.build().toString)
      }
    }
  }
}
