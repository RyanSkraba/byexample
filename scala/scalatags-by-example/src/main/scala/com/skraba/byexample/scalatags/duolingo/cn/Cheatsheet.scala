package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.Svg.Attrs
import com.skraba.byexample.scalatags.duolingo.cn.Cheatsheet._
import scalatags.Text.implicits._
import scalatags.Text.svgTags._

import scala.io.Source
import scala.reflect.io.File

/** A vocabulary cheat sheet for duolingo chinese lessons.
  *
  * @param vocab the list of words to include in the cheat sheet.
  * @param cfg Configuration for drawing the sheet.
  */
case class Cheatsheet(vocab: Seq[Vocab], cfg: Config = Config()) {

  /** Draws the cheatsheet.  The first column of words is centered at 0, 0, and the entire page
    * will likely need to be translated.
    */
  def toSvg: Tag = {
    // Group the words according to their lesson
    val byLesson: List[List[Vocab]] = vocab.foldRight[List[List[Vocab]]](Nil) {
      case (v, head :: tail)
          if head.headOption.map(_.lesson).contains(v.lesson) =>
        (v :: head) :: tail
      case (v, xs) => (v :: Nil) :: xs
    }

    // Set the words in a group vertically, and the lessons horizontally.
    val groupTags: Seq[Tag] = byLesson.zipWithIndex.map { case (vs, x) =>
      val vocabTags = vs
        .map(cfg.vocab)
        .zipWithIndex
        .map { case (tag, y) =>
          tag(Svg.attrTranslate(0, y * cfg.lineHeight))
        }
      g(vocabTags: _*)(Svg.attrTranslate(x * 150, 0))
    }
    g(groupTags: _*)
  }

}

object Cheatsheet {

  /** Memo of vowels with tone markings (from tone 1 to tone 4). */
  private[this] val ToneVowels: Seq[String] =
    Seq("āēīōū", "áéíóú", "ǎěǐǒǔǚ", "àìèòùǜ")

  /** Memo map from tone-marked vowel to its tone. */
  private[this] lazy val Tones: Map[Char, Int] = {
    for ((s, i) <- ToneVowels.zipWithIndex; c <- s) yield (c, i + 1)
  }.toMap

  /**  A cheatsheet autoloaded with all the words and the default config.  This may make a call
    *  out to a remote resource.
    */
  lazy val All: Cheatsheet = all()

  /** A single word in the cheatsheet. */
  case class Vocab(
      cn: String,
      pinyin: String,
      en: String,
      info: Array[String]
  ) {
    lazy val section: String = info(1)
    lazy val lesson: String = info(2)
  }

  /** @param toneHex An array of hex codes to colour tones for syllables and characters.  This
    *                should be a five element array and the first value is used for no tone.
    * @param text The text tag to use for writing.
    * @param cnDx The width of the chinese text in the middle of the column.
    * @param lineHeight The height of a drawn line.
    * @param linesPerColumn The number of lines drawn per column.
    * @param columnDx If more than one column, then the distance between the columns.
    */
  case class Config(
      toneHex: Seq[String] =
        Seq("000000", "6a245c", "498c13", "3543a6", "c8361e"),
      text: Svg.Text = Svg.Text(family = "Source Han Sans CN"),
      cnDx: Double = 40,
      lineHeight: Double = 10,
      linesPerColumn: Int = Integer.MAX_VALUE,
      columnDx: Double = 100
  ) {

    private[this] val pinyinTxt =
      text.right(-cnDx / 2, 0)(attr("xml:space") := "preserve")
    private[this] val cnTxt = text.center(0, 0)
    private[this] val enTxt = text.left(cnDx / 2, 0)

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
        .map { case ((txt, tone), i) =>
          tspan(Attrs.fill := s"#${toneHex(tone)}", txt)
        }
    }

    def vocab(v: Vocab): Tag = {
      val tones = Cheatsheet.tones(v.pinyin)
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

    Cheatsheet(
      allWords.tail.map(info =>
        Vocab(info(cn).trim, info(pinyin).trim, info(en).trim, info)
      )
    )
  }

  /** @return the tones found in the pinyin syllables */
  def tones(pinyin: String): Seq[Int] = {
    // Luckily, the pinyin syllables are all split into separate words.
    pinyin.split("\\s+").map {
      _.find(Tones.contains).map(Tones.apply).getOrElse(0)
    }
  }

  /** @return The TSV contents of the Duolingo classes.
    */
  private[this] def contents(): Seq[String] = {
    val cached = File("/tmp/duolingo-chinese-words-anki-deck.tsv")
    cached
      .safeSlurp()
      .getOrElse {
        val html = Source.fromURL(
          "https://raw.githubusercontent.com/anki-decks/anki-deck-for-duolingo-chinese/master/words.tsv"
        )
        // There a minor adjustment to make with one line!
        val raw = html.mkString.replaceAllLiterally("\"\n", "\"")
        cached.writeAll(raw)
        raw
      }
      .split('\n')
  }

}
