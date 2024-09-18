package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.Svg.Attrs
import com.skraba.byexample.scalatags.duolingo.cn.Cheatsheet._
import scalatags.Text.implicits._
import scalatags.Text.svgTags._

import scala.io.Source
import scala.reflect.io.File

/** A single word or phrase in the cheatsheet
  * @param cn
  *   The symbol for the word.
  * @param pinyin
  *   The representation of the word in pinyin.
  * @param en
  *   The meaning of the word in english.
  * @param section
  *   The section number for the lesson.
  * @param lesson
  *   The lesson name
  * @param info
  *   Any additional information for the word.
  */
case class Vocab(cn: String, pinyin: String, en: String, section: String = "", lesson: String = "")(
    info: Array[String] = Array.empty
)

object Vocab {

  /** Alternative constructor from a single string to be split into the fields.
    *
    * @param in
    *   The input string.
    * @param splitter
    *   The regex token specifier used for splitting.
    */
  def apply(in: String, splitter: String): Vocab = {
    val tokens: Array[String] = in.split(splitter).map(_.trim)
    Vocab(
      cn = tokens(0),
      pinyin = Pinyin.toAccented(tokens(1)),
      en = tokens(2),
      section = tokens.lift(3).getOrElse(""),
      lesson = tokens.lift(4).getOrElse("")
    )(info = tokens.drop(5))
  }

  /** Alternative constructor from a single string to be split by ":" into the fields
    * @param in
    *   The input string.
    */
  def apply(in: String): Vocab = apply(in, ":")
}

/** A group of related worlds in the cheatsheet
  * @param words
  *   The list of related words.
  * @param title
  *   A title for these words (or None to omit).
  * @param offset
  *   A helpful offset for laying out the words in relation to the upper-left document corner, or the upper-left of the
  *   last laid out group of words.
  * @param out
  *   Configuration for drawing the sheet.
  */
case class VocabGroup(
    words: Seq[Vocab],
    title: Option[String] = None,
    offset: Option[(Double, Double)] = None,
    out: Config = Config()
) {

  /** Draws the vocab group, with the title on top moving down one line for each vocab. */
  def toSvg: Tag = {
    g((title.map(out.title).toSeq ++ words.map(out.vocab)).zipWithIndex.map { case (tag, y) =>
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
case class Cheatsheet(vocab: Seq[Vocab], out: Config = Config()) {

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
  case class Config(
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
