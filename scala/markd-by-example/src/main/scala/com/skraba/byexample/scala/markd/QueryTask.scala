package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.Task
import com.tinfoiled.markd.{Header, Markd, MultiMarkd}

import scala.reflect.io.File
import scala.util.matching.Regex

/** Extracts text from a markdown file. */
object QueryTask extends Task {

  val Cmd = "query"

  val Description = "Extracts text from a markdown file via a query path."

  val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${MarkdGo.Name} $Cmd --query QUERY (FILE|-)
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
       |  ${MarkdGo.Name} $Cmd --query One.Two.Three[*] markd.md
       |
       |Find the level one header with the name "One", with a subheader named "Two" and
       |a third-level header named "Three" and return its contents.
       |
       |  ${MarkdGo.Name} $Cmd --query Top markd.md
       |
       |Find and return the level one header with the title "Top"
       |
       |TODO:
       |  ${MarkdGo.Name} $Cmd --query Weekly..2025-02-14 markd.md
       |
       |Find the level one header with the title "Weekly" and return the first
       |subheader named "2025-02-14" at any level inside
       |
       |TODO:
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
       |TODO:
       |  ${MarkdGo.Name} $Cmd --query "Weekly!To Do" markd.md
       |
       |Find the level one header with the title "Weekly" and return the To Do table
       |that   it contains.
       |
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {

    val qx: String = opts.getString("--query")
    val file: String = opts.getString("FILE")

    val md = Header.parse(
      if (file == "-") Iterator.continually(Console.in.readLine()).takeWhile(_ != null).mkString("\n")
      else File(file).slurp()
    )

    print(query(qx, md).map(_.build().toString.trim).mkString)
  }

  def query(query: String, md: Markd): Seq[Markd] = {

    val QueryRegex: Regex = raw"^(?<sep>\.*)(?<token>[^.\[]*)(?<rest>(\[(?<index>[^]]+)])?.*)$$".r

    def queryInternal(in: (String, Seq[Markd])): (String, Seq[Markd]) = in match {
      case ("[*]", Seq(md: MultiMarkd[_])) => ("", md.mds)
      case (q, md) if q.head == '.'        => (q.tail, md)
      case (QueryRegex(sep, token, rest, _*), Seq(h: MultiMarkd[_])) if token.nonEmpty =>
        (rest, h.mds.collectFirst { case h @ Header(title, _, _) if title == token => h }.toSeq)
      case _ => sys.error(s"Unrecognized query: $query")
    }

    LazyList
      .iterate((query, Seq(md))) { queryInternal }
      .dropWhile(acc => acc._1.nonEmpty && acc._2.nonEmpty)
      .head match {
      case ("", md)   => md
      case (_, Seq()) => Seq()
    }
  }
}
