package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import java.io.InputStreamReader
import scala.io.Source
import scala.reflect.io.File

/** Extracts text from a markdown file. */
object QueryTask extends DocoptCliGo.Task {

  val Cmd = "query"

  val Description = "Extracts text from a markdown file via a query path."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${MarkdGo.Cli} $Cmd --query QUERY (FILE|-)
       |
       |Options:
       |  -h --help      Show this screen.
       |  --version      Show version.
       |  --fail         Fail instead of returning empty.
       |  FILE           File to query
       |  --query=QUERY  A query specification
       |
       |By default, if the query can't be satisfied, no text is returned and the
       |command is successful. If the --fail flag is present, an error message is
       |printed and the system exit is non-zero.  An unknown query is a failure.
       |
       |Examples:
       |
       |  ${MarkdGo.Cli} $Cmd --query $$'#One/Two/Three' markd.md
       |
       |Find the level one header with the name "One", with a subheader named "Two" and
       |a third-level header named "Three" and return its contents.
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {

    val query: String = opts.getString("--query")

    val file: String = opts.getString("FILE")
    val md = Header.parse(File(file).slurp())

    // For now only nested headers are supported.
    if (query.startsWith("#")) {
      query.tail
        .split("/")
        .zipWithIndex
        .toSeq
        .foldLeft(Option(md)) { case (Some(acc), (txt: String, idx: Int)) =>
          acc.collectFirstRecursive {
            case h @ Header(title, level, _) if title == txt && level == idx + 1 => h.copy(level = 0)
          }
        }
        .map(_.build().toString.trim)
        .foreach(print)
    } else {
      sys.error(s"Unrecognized query: $query")
    }
  }
}
