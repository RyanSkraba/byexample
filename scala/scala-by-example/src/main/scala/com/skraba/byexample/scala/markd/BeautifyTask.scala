package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.jdk.CollectionConverters._

/** Command-line driver for beautifying a markdown file.
  */
object BeautifyTask extends DocoptCliGo.Task {

  val Cmd = "beautify"

  val Description = "Reformat a markdown file."

  val Doc: String =
    """Beautify a markdown file.
      |
      |Usage:
      |  MarkdGo beautify [--sortLinkRefs] FILE...
      |
      |Options:
      |  -h --help       Show this screen.
      |  --version       Show version.
      |  --sortLinkRefs  Sort the link references in the file (off by default)
      |  FILE            File(s) to beautify.
      |""".stripMargin.trim

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: Seq[String] = opts.get("FILE").asInstanceOf[java.lang.Iterable[String]].asScala.toSeq
    val cfg: ParserCfg = new ParserCfg(sortLinkRefs = opts.get("--sortLinkRefs").toString.toBoolean)

    MarkdGo.processMd(files) { f =>
      {
        val md = Header.parse(f.slurp(), cfg)
        f.writeAll(md.build().toString)
      }
    }
  }
}
