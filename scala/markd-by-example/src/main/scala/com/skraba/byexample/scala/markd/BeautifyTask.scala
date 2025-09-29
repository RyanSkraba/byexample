package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.{Docopt, Task}
import com.tinfoiled.markd.{Markd, ParserCfg}

/** Command-line driver for beautifying a markdown file.
  */
object BeautifyTask extends Task {

  val Cmd = "beautify"

  val Description = "Reformat a markdown file."

  val Doc: String =
    s"""Beautify a markdown file.
       |
       |Usage:
       |  ${MarkdGo.Name} $Cmd [options] FILE...
       |
       |Options:
       |  -h --help       Show this screen.
       |  --version       Show version.
       |  --sortLinkRefs  Sort the link references in the file (off by default)
       |  --verbose       Print the files that are modified.
       |  --dryRun        Like --verbose but without modifying any files.
       |  FILE            File(s) to beautify.
       |""".stripMargin.trim

  def go(opt: Docopt): Unit = {

    val files: Iterable[String] = opt.strings.get("FILE")
    val cfg: ParserCfg = new ParserCfg(sortLinkRefs = opt.flag("--sortLinkRefs"))
    val dryRun: Boolean = opt.flag("--dryRun")
    val verbose: Boolean = dryRun || opt.flag("--verbose")

    MarkdGo.processMd(files) { f =>
      {
        val original = f.slurp()
        val md = Markd.parse(original, cfg)
        val rewritten = md.build().toString()
        val modified = original != rewritten

        if (modified) {
          if (verbose) println(s"Modifying $f")
          if (!dryRun) f.writeAll(rewritten)
        }
      }
    }
  }
}
