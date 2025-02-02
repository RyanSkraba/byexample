package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.jdk.CollectionConverters._

/** Command-line driver for beautifying a markdown file.
  */
object BeautifyTask extends DocoptCliGo.Task {

  val Cmd = "beautify"

  val Description = "Reformat a markdown file."

  val Doc: String =
    s"""Beautify a markdown file.
       |
       |Usage:
       |  ${MarkdGo.Cli} $Cmd [--sortLinkRefs] FILE...
       |
       |Options:
       |  -h --help       Show this screen.
       |  --version       Show version.
       |  --sortLinkRefs  Sort the link references in the file (off by default)
       |  FILE            File(s) to beautify.
       |""".stripMargin.trim

  def go(opts: TaskOptions): Unit = {

    val files: Iterable[String] = opts.getStrings("FILE")
    val cfg: ParserCfg = new ParserCfg(sortLinkRefs = opts.getBoolean("--sortLinkRefs"))

    MarkdGo.processMd(files) { f =>
      {
        val md = Header.parse(f.slurp(), cfg)
        f.writeAll(md.build().toString)
      }
    }
  }
}
