package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.{Docopt, Task}
import com.tinfoiled.markd.ql.MarkdQL
import com.tinfoiled.markd.ql.MarkdQL.query
import com.tinfoiled.markd.Markd

import scala.reflect.io.File

/** Extracts text from a markdown file. */
object QueryTask extends Task {

  val Cmd = "query"

  val Description = "Extracts text from a markdown file via a query path."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${MarkdGo.Name} $Cmd --query QUERY [options] (FILE|-)
       |
       |Options:
       |  -h --help      Show this screen.
       |  --verbose      Enable verbose logging
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
       |  ${MarkdGo.Name} $Cmd --query One.Two.Three[*] markd.md
       |
       |Find the level one header with the name "One", with a subheader named "Two" and
       |a third-level header named "Three" and return its contents.
       |
       |  ${MarkdGo.Name} $Cmd --query Top markd.md
       |
       |Find and return the level one header with the title "Top"
       |
       |  ${MarkdGo.Name} $Cmd --query Weekly..2025-02-14 markd.md
       |
       |Find the level one header with the title "Weekly" and return the first
       |subheader named "2025-02-14" at any level inside
       |
       |  ${MarkdGo.Name} $Cmd --query "Weekly[0]" markd.md
       |
       |Find the level one header with the title "Weekly" and return the first element
       |it contains.
       |
       |TODO:
       |  ${MarkdGo.Name} $Cmd --query "Weekly[code][0]" markd.md
       |
       |Find the level one header with the title "Weekly" and return the first
       |code block it contains.
       |
       |  ${MarkdGo.Name} $Cmd --query "Weekly!To Do" markd.md
       |
       |Find the level one header with the title "Weekly" and return the To Do table
       |that it contains.
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    val qx: String = opt.string.get("--query")
    val file: String = opt.string.get("FILE")

    val md = Markd.parse(
      if (file == "-") Iterator.continually(Console.in.readLine()).takeWhile(_ != null).mkString("\n")
      else File(file).slurp()
    )

    if (opt.flag("--verbose")) {
      val (inProgress, _) = LazyList.iterate(MarkdQL.Query(rest = qx, mds = Seq(md))) { _.next }.span(!_.isDone)
      for (query <- inProgress) {
        print("Query ")
        if (query.regex) print("(regex) ")
        if (query.recursive) print("(recursive) ")
        print(s"${query.separator}${query.token}")
        if (query.index.nonEmpty) print(s"[${query.index}]")
        if (query.rest.nonEmpty) print(s"|${query.rest}")
        if (query.mds.size > 1) print(s"mds.size=${query.mds.size} mds.head=")
        // TODO: Improve verbose info about currently matched classes
        query.mds match {
          case Seq()   =>
          case Seq(md) => print(s" ${md.getClass.getSimpleName}")
          case md      => print(s" md[0/${md.size}]=${md.getClass.getSimpleName}")
        }
        println()
      }
    }

    print(query(qx, md).map(_.build().toString.trim).mkString)
  }
}
