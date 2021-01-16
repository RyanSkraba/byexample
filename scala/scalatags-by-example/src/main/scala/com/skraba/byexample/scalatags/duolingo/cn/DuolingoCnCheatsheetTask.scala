package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.ScalatagsGo
import com.skraba.byexample.scalatags.Svg.wrapSvg

/** Command-line driver for generating a SVG Cheatsheet from Duolingo chinese lessons.
  */
object DuolingoCnCheatsheetTask {

  val Doc: String =
    """Generate a simple vocabulary cheatsheet for Duolingo Chinese lessons.
    |
    |Usage:
    |  ScalatagsGo cheatsheet [--section=NN] [--lesson=LESSION]
    |
    |Options:
    |  -h --help            Show this screen.
    |  --version            Show version.
    |  --section=NN         Only show words from the specified section.
    |  --lesson=LESSION     Only show words from the specified lesson.
    |
    |""".stripMargin.trim

  val Cmd = "cheatsheet"

  val Description =
    "Generate a simple vocabulary sheet for Duolingo Chinese lessons."

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    def filterOnSection(s: Any): Cheatsheet.Vocab => Boolean = _.section == s
    def filterOnLesson(l: Any): Cheatsheet.Vocab => Boolean = _.lesson == l

    val vocab: Seq[Cheatsheet.Vocab] = Cheatsheet.All.vocab
      .filter(
        Option(opts.get("--section")).map(filterOnSection).getOrElse(_ => true)
      )
      .filter(
        Option(opts.get("--lesson")).map(filterOnLesson).getOrElse(_ => true)
      )

    println(wrapSvg(new Cheatsheet(vocab = vocab).toSvg()).render)
  }

  val Task: ScalatagsGo.Task = ScalatagsGo.Task(Doc, Cmd, Description, go)
}
