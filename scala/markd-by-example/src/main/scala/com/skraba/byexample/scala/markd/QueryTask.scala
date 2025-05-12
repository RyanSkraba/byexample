package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import java.io.InputStreamReader
import scala.io.Source
import scala.reflect.io.File
import scala.util.matching.Regex

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
       |  FILE           File to query or '-' for STDIN
       |  --query=QUERY  A query specification
       |
       |By default, if the query can't be satisfied, no text is returned and the
       |command is successful. If the --fail flag is present, an error message is
       |printed and the system exit is non-zero.  An unknown query is a failure.
       |
       |Examples:
       |
       |  ${MarkdGo.Cli} $Cmd --query One.Two.Three markd.md
       |
       |Find the level one header with the name "One", with a subheader named "Two" and
       |a third-level header named "Three" and return its contents.
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {

    val queryx: String = opts.getString("--query")

    val file: String = opts.getString("FILE")
    val md = Header.parse(
      if (file == "-") Iterator.continually(Console.in.readLine()).takeWhile(_ != null).mkString("\n")
      else File(file).slurp()
    )

    print(query(queryx, md).build().toString.trim)

//      sys.error(s"Unrecognized query: $query")
  }

  def query(query: String, md: Markd): Markd = {
    val HeaderRegex: Regex = raw"^(?<head>[^.]+)(?<extra>\[[^.]])?.?(?<rest>.*)$$".r

    def queryInternal(in: (String, Option[Markd])): (String, Option[Markd]) = in match {
      case (query, md) if query.head == '.' => (query.tail, md)
      case (HeaderRegex(head, _, rest), Some(h: Header)) =>
        (rest, h.mds.collectFirst { case h @ Header(title, _, _) if title == head => h.copy(level = 0) })
    }

    LazyList
      .iterate((query, Option(md))) { queryInternal }
      .dropWhile(acc => acc._1.nonEmpty && acc._2.nonEmpty)
      .head match {
      case ("", Some(md)) => md
      case (_, None)      => Paragraph("")
    }
  }
}
