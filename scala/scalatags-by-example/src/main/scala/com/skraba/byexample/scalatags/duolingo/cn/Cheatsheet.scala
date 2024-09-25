package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.Svg.Attrs
import com.skraba.byexample.scalatags.duolingo.cn.Cheatsheet._
import scalatags.Text.implicits._
import scalatags.Text.svgTags._

import scala.io.Source
import scala.reflect.io.File

/** A group of related worlds in the cheatsheet
  * @param lesson
  *   The list of related words to put in the spreadsheet.
  * @param offset
  *   A helpful offset for laying out the words in relation to the upper-left document corner, or the upper-left of the
  *   last laid out group of words.
  * @param out
  *   Configuration for drawing the sheet.
  */
case class SvgLessonGroup(
    lesson: Lesson,
    offset: Option[(Double, Double)] = None,
    out: SvgLayoutCfg = SvgLayoutCfg()
) {

  /** Draws the vocab group, with the title on top moving down one line for each vocab. */
  def toSvg: Tag = {
    g((lesson.title.map(out.title).toSeq ++ lesson.vocab.map(out.vocab)).zipWithIndex.map { case (tag, y) =>
      tag(Svg.attrTranslate(0, y * out.lineHeight))
    })
  }

}

/** A vocabulary cheat sheet for duolingo chinese lessons.
  * @param vocab
  *   the list of words to include in the cheat sheet.
  * @param out
  *   Configuration for drawing the sheet.
  */
case class Cheatsheet(vocab: Seq[Vocab], out: SvgLayoutCfg = SvgLayoutCfg()) {

  /** Draws the cheatsheet. The first column of words is centered at 0, 0, and the entire page will likely need to be
    * translated.
    */
  def toSvg: Tag = {
    // Group the words according to their lesson
    val byLesson: List[List[Vocab]] = vocab.foldRight[List[List[Vocab]]](Nil) {
      case (v, head :: tail) if head.headOption.map(_.lesson).contains(v.lesson) => (v :: head) :: tail
      case (v, xs)                                                               => (v :: Nil) :: xs
    }

    // Set the words in a group vertically, and the lessons horizontally.
    val groupTags: Seq[Tag] = byLesson.zipWithIndex.map { case (vs, x) =>
      val vocabTags =
        vs.map(out.vocab).zipWithIndex.map { case (tag, y) => tag(Svg.attrTranslate(0, y * out.lineHeight)) }
      g(vocabTags: _*)(Svg.attrTranslate(x * 150, 0))
    }
    g(groupTags: _*)
  }

}

object Cheatsheet {

  /** A cheatsheet autoloaded with all the words and the default config. This may make a call out to a remote resource.
    */
  lazy val All: Cheatsheet = all()

  /** @param toneHex
    *   An array of hex codes to colour tones for syllables and characters. This should be a five element array and the
    *   first value is used for no tone.
    * @param text
    *   The text tag to use for writing.
    * @param cnDx
    *   The width of the chinese text in the middle of the column.
    * @param lineHeight
    *   The height of a drawn line.
    * @param linesPerColumn
    *   The number of lines drawn per column.
    * @param columnDx
    *   If more than one column, then the distance between the columns.
    */
  case class SvgLayoutCfg(
      toneHex: Seq[String] = Seq("000000", "6a245c", "498c13", "3543a6", "c8361e"),
      text: Svg.Text = Svg.Text(family = "Noto Sans SC"),
      cnDx: Double = 40,
      lineHeight: Double = 10,
      linesPerColumn: Int = Integer.MAX_VALUE,
      columnDx: Double = 100
  ) {

    private[this] val pinyinTxt = text.right(-cnDx / 2, 0)(attr("xml:space") := "preserve")
    private[this] val cnTxt = text.center(0, 0)
    private[this] val enTxt = text.left(cnDx / 2, 0)
    private[this] val titleTxt = text.copy(weight = "bold")

    def filledTspan(in: String, tones: Seq[Int]): Seq[Tag] = {

      val spans =
        if (in.length == tones.size) in.split("")
        else {
          val syllables = in.split("\\s+")
          syllables.head +: syllables.tail.map(" " + _)
        }

      spans
        .zip(tones)
        .zipWithIndex
        .map { case ((txt, tone), _) => tspan(Attrs.fill := s"#${toneHex(tone)}", txt) }
    }

    def title(title: String): Tag = { titleTxt.center(0, 0)(title) }

    def vocab(v: Vocab): Tag = {
      val tones = Pinyin.tones(v.pinyin)
      g(
        cnTxt(filledTspan(v.cn, tones)),
        pinyinTxt(filledTspan(v.pinyin, tones)),
        enTxt(v.en)
      )
    }

  }

  /** Create the cheatsheet fully initialized by the vocabulary contents.
    */
  def all(): Cheatsheet = {
    val allWords: Seq[Array[String]] = contents().map(_.split('\t'))

    val cn = allWords.head.indexOf("Simplified Chinese")
    val pinyin = allWords.head.indexOf("Selected Pinyin")
    val en = allWords.head.indexOf("Simple English Definition")
    val section = allWords.head.indexOf("DL Section")
    val lesson = allWords.head.indexOf("DL Lesson")

    Cheatsheet(
      allWords.tail.map(info =>
        Vocab(
          cn = info(cn).trim,
          pinyin = info(pinyin).trim,
          en = info(en).trim,
          section = info(section).trim,
          lesson = info(lesson).trim
        )(info = info)
      )
    )
  }

  /** @return
    *   The TSV contents of the Duolingo classes.
    */
  private[this] def contents(): Seq[String] = {
    val cached = File("/tmp/duolingo-chinese-words-anki-deck.tsv")
    cached
      .safeSlurp()
      .getOrElse {
        val html =
          Source.fromURL("https://raw.githubusercontent.com/RyanSkraba/anki-deck-for-duolingo-chinese/master/words.tsv")
        // There a minor adjustment to make with one line!
        val raw = html.mkString.replace("\"\n", "\"")
        cached.writeAll(raw)
        raw
      }
      .split('\n')
  }
}
