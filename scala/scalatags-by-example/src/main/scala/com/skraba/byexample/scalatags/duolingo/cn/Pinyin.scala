package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.Svg.Attrs
import com.skraba.byexample.scalatags.duolingo.cn.Cheatsheet._
import scalatags.Text.implicits._
import scalatags.Text.svgTags._

import scala.io.Source
import scala.reflect.io.File

/** Helpers to manage Pinyin text
  *
  * https://pinyin.info/rules/initials_finals.html
  */
object Pinyin {

  /** Memo of vowels to tone markings. The first is the bare vowel, followed by the four tones. */
  private[this] val ToneVowels: Seq[String] = Seq("aāáǎà", "eēéěè", "iīíǐì", "oōóǒò", "uūúǔù", "ü..ǚǜ")

  /** Memo map from tone-marked vowel to its bare character and tone. */
  private[this] lazy val Tones: Map[Char, (Char, Int)] = {
    for (s <- ToneVowels; (c, i) <- s.zipWithIndex if i > 0 && c != '.')
      yield Seq(c -> (s(0), i), c.toUpper -> (s(0).toUpper, i))
  }.flatten.toMap

  /** @return the tones found in the pinyin syllables */
  def tones(pinyin: String): Seq[Int] = {
    // Luckily, the pinyin syllables are all split into separate words.
    pinyin.split("\\s+").map { _.find(Tones.contains).map(Tones(_)._2).getOrElse(0) }
  }

  /** @return
    *   convert all pinyin text with numbered vowels converted into unicode accented characters
    */
  def toAccented(pinyin: String): String = {
    // Move superscripts to plain numbers
    val unsuperscripted: String =
      pinyin.replace("⁰", "0").replace("¹", "1").replace("²", "2").replace("³", "3").replace("⁴", "4")
    Tones.foldLeft(unsuperscripted) { case (acc, (accented, (bare, tone))) =>
      acc.replace(s"$bare$tone", accented.toString)
    }
  }

  def toNumbered(pinyin: String, superscript: Boolean = false): String = {
    val numbered = Tones.foldLeft(pinyin) { case (acc, (accented, (bare, tone))) =>
      acc.replace(accented.toString, s"$bare$tone")
    }
    if (!superscript) numbered.replace("⁰", "0").replace("¹", "1").replace("²", "2").replace("³", "3").replace("⁴", "4")
    else numbered.replace("0", "⁰").replace("1", "¹").replace("2", "²").replace("3", "³").replace("4", "⁴")
  }

  /** @return
    *   The TSV contents of the Duolingo classes.
    */
  private[this] def contents(): Seq[String] = {
    val cached = File("/tmp/duolingo-chinese-words-anki-deck.tsv")
    cached
      .safeSlurp()
      .getOrElse {
        val html = Source.fromURL(
          "https://raw.githubusercontent.com/RyanSkraba/anki-deck-for-duolingo-chinese/master/words.tsv"
        )
        // There a minor adjustment to make with one line!
        val raw = html.mkString.replace("\"\n", "\"")
        cached.writeAll(raw)
        raw
      }
      .split('\n')
  }
}
