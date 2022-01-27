package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.ScalatagsGo
import com.skraba.byexample.scalatags.Svg.wrapSvg

/** Command-line driver for generating a SVG Cheatsheet from Duolingo chinese
  * lessons.
  */
object DuolingoCnCheatsheetTask {

  val Doc: String =
    """Generate a simple vocabulary cheatsheet for Duolingo Chinese lessons.
    |
    |Usage:
    |  ScalatagsGo cheatsheet [--section=NN] [--lesson=LESSION] [--words=WORDS]
    |
    |Options:
    |  -h --help            Show this screen.
    |  --version            Show version.
    |  --section=NN         Only show words from the specified section.
    |  --lesson=LESSION     Only show words from the specified lesson.
    |  --words=WORDS        Only show the specified ideograms.
    |
    |""".stripMargin.trim

  val Cmd = "cheatsheet"

  val Description =
    "Generate a simple vocabulary sheet for Duolingo Chinese lessons."

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    // Methods to apply if the section or lession filters are in use.
    def filterOnSection(s: Any): Cheatsheet.Vocab => Boolean = _.section == s
    def filterOnLesson(l: Any): Cheatsheet.Vocab => Boolean = _.lesson == l

    val vocab: Seq[Cheatsheet.Vocab] = Cheatsheet.All.vocab
      .filter(
        Option(opts.get("--section")).map(filterOnSection).getOrElse(_ => true)
      )
      .filter(
        Option(opts.get("--lesson")).map(filterOnLesson).getOrElse(_ => true)
      )

    // If the --words filter is in use, then it might contain commas
    val words = Option(opts.get("--words"))
      .map {
        case str: String if str.contains(",") => str.split(',')
        case str: String                      => str.split("")
        case _                                => Array()
      }
      .map(words => {
        val byCn: Map[String, Cheatsheet.Vocab] =
          vocab.map(v => v.cn -> v).toMap
        words.flatMap(byCn.get).toSeq
      })
      .getOrElse(vocab)

    println(wrapSvg(new Cheatsheet(vocab = words).toSvg()).render)
  }

  val Task: ScalatagsGo.Task = ScalatagsGo.Task(Doc, Cmd, Description, go)
}
