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
  *   - https://pinyin.info/rules/initials_finals.html The table of sounds to syllables.
  *   - https://pinyin.info/rules/where.html Where to place tone markers.
  */
object Pinyin {

  /** Memo of vowels to tone markings. The first is the bare vowel, followed by the four tones. */
  private[this] val ToneVowels: Seq[String] = Seq("aāáǎà", "eēéěè", "iīíǐì", "oōóǒò", "uūúǔù", "ü..ǚǜ")

  /** Memo map from tone-marked vowel to its bare character and tone. */
  private[this] lazy val Tones: Map[Char, (Char, Int)] = {
    for (s <- ToneVowels; (c, i) <- s.zipWithIndex if i > 0 && c != '.')
      yield Seq(c -> (s(0), i), c.toUpper -> (s(0).toUpper, i))
  }.flatten.toMap

  /** A table of valid initial and final sound combinations in Mandarin as their pinyin equivalents. The top row are the
    * initials, and the left column are the finals. The rightmost column is without any initial sound. This table is
    * missing er.
    */
  private[this] val PinyinTableMarkdown: String =
    s"""    |b   |p   |m   |f   |d   |t   |n    |l    |g    |k    |h    |z   |c   |s   |zh    |ch    |sh    |r   |j    |q    |x    | 
       !----|----|----|----|----|----|----|-----|-----|-----|-----|-----|----|----|----|------|------|------|----|-----|-----|-----|----
       !a   |ba  |pa  |ma  |fa  |da  |ta  |na   |la   |ga   |ka   |ha   |za  |ca  |sa  |zha   |cha   |sha   |    |     |     |     |a
       !o   |bo  |po  |mo  |fo  |    |    |     |     |     |     |     |    |    |    |      |      |      |    |     |     |     |o
       !e   |    |    |me  |    |de  |te  |ne   |le   |ge   |ke   |he   |ze  |ce  |se  |zhe   |che   |she   |re  |     |     |     |e
       !ai  |bai |pai |mai |    |dai |tai |nai  |lai  |gai  |kai  |hai  |zai |cai |sai |zhai  |chai  |shai  |    |     |     |     |ai
       !ei  |bei |pei |mei |fei |dei |tei |nei  |lei  |gei  |kei  |hei  |zei |    |    |zhei  |      |shei  |    |     |     |     |ei
       !ao  |bao |pao |mao |    |dao |tao |nao  |lao  |gao  |kao  |hao  |zao |cao |sao |zhao  |chao  |shao  |rao |     |     |     |ao
       !ou  |    |pou |mou |fou |dou |tou |nou  |lou  |gou  |kou  |hou  |zou |cou |sou |zhou  |chou  |shou  |rou |     |     |     |ou
       !an  |ban |pan |man |fan |dan |tan |nan  |lan  |gan  |kan  |han  |zan |can |san |zhan  |chan  |shan  |ran |     |     |     |an
       !ang |bang|pang|mang|fang|dang|tang|nang |lang |gang |kang |hang |zang|cang|sang|zhang |chang |shang |rang|     |     |     |ang
       !en  |ben |pen |men |fen |den |    |nen  |     |gen  |ken  |hen  |zen |cen |sen |zhen  |chen  |shen  |ren |     |     |     |en
       !eng |beng|peng|meng|feng|deng|teng|neng |leng |geng |keng |heng |zeng|ceng|seng|zheng |cheng |sheng |reng|     |     |     |eng
       !ong |    |    |    |    |dong|tong|nong |long |gong |kong |hong |zong|cong|song|zhong |chong |      |rong|     |     |     |
       !u   |bu  |pu  |mu  |fu  |du  |tu  |nu   |lu   |gu   |ku   |hu   |zu  |cu  |su  |zhu   |chu   |shu   |ru  |     |     |     |wu
       !ua  |    |    |    |    |    |    |     |     |gua  |kua  |hua  |    |    |    |zhua  |chua  |shua  |rua |     |     |     |wa
       !uo  |    |    |    |    |duo |tuo |nuo  |luo  |guo  |kuo  |huo  |zuo |cuo |suo |zhuo  |chuo  |shuo  |ruo |     |     |     |wo
       !uai |    |    |    |    |    |    |     |     |guai |kuai |huai |    |    |    |zhuai |chuai |shuai |    |     |     |     |wai
       !ui  |    |    |    |    |dui |tui |     |     |gui  |kui  |hui  |zui |cui |sui |zhui  |chui  |shui  |rui |     |     |     |wei
       !uan |    |    |    |    |duan|tuan|nuan |luan |guan |kuan |huan |zuan|cuan|suan|zhuan |chuan |shuan |ruan|     |     |     |wan
       !uang|    |    |    |    |    |    |     |     |guang|kuang|huang|    |    |    |zhuang|chuang|shuang|    |     |     |     |wang
       !un  |    |    |    |    |dun |tun |nun  |lun  |gun  |kun  |hun  |zun |cun |sun |zhun  |chun  |shun  |run |     |     |     |wen
       !ueng|    |    |    |    |    |    |     |     |     |     |     |    |    |    |      |      |      |    |     |     |     |weng
       !i   |bi  |pi  |mi  |    |di  |ti  |ni   |li   |     |     |     |zi  |ci  |si  |zhi   |chi   |shi   |ri  |ji   |qi   |xi   |yi
       !ia  |    |    |    |    |dia |    |     |lia  |     |     |     |    |    |    |      |      |      |    |jia  |qia  |xia  |ya
       !ie  |bie |pie |mie |    |die |tie |nie  |lie  |     |     |     |    |    |    |      |      |      |    |jie  |qie  |xie  |ye
       !iao |biao|piao|miao|    |diao|tiao|niao |liao |     |     |     |    |    |    |      |      |      |    |jiao |qiao |xiao |yao
       !iu  |    |    |miu |    |diu |    |niu  |liu  |     |     |     |    |    |    |      |      |      |    |jiu  |qiu  |xiu  |you
       !ian |bian|pian|mian|    |dian|tian|nian |lian |     |     |     |    |    |    |      |      |      |    |jian |qian |xian |yan
       !iang|    |    |    |    |    |    |niang|liang|     |     |     |    |    |    |      |      |      |    |jiang|qiang|xiang|yang
       !in  |bin |pin |min |    |    |    |nin  |lin  |     |     |     |    |    |    |      |      |      |    |jin  |qin  |xin  |yin
       !ing |bing|ping|ming|    |ding|ting|ning |ling |     |     |     |    |    |    |      |      |      |    |jing |qing |xing |ying
       !iong|    |    |    |    |    |    |     |     |     |     |     |    |    |    |      |      |      |    |jiong|qiong|xiong|yong
       !ü   |    |    |    |    |    |    |nü   |lü   |     |     |     |    |    |    |      |      |      |    |ju   |qu   |xu   |yu
       !üe  |    |    |    |    |    |    |nüe  |lüe  |     |     |     |    |    |    |      |      |      |    |jue  |que  |xue  |yue
       !üan |    |    |    |    |    |    |     |     |     |     |     |    |    |    |      |      |      |    |juan |quan |xuan |yuan
       !ün  |    |    |    |    |    |    |     |     |     |     |     |    |    |    |      |      |      |    |jun  |qun  |xun  |yun
       !""".stripMargin('!')

  lazy private[this] val PinyinTable: Array[Array[String]] =
    PinyinTableMarkdown.split('\n').map(_.split('|').map(_.trim)).filterNot(_.head.contains("-"))

  /** The initial sounds that are available in pinyin (including "" for none). */
  lazy val Initials: Seq[String] = PinyinTable(0).tail

  /** The final sounds that are available in pinyin. */
  lazy val Finals: Seq[String] = PinyinTable.tail.map(_.head)

  /** Valid pinyin words formed from initial and final sounds, including 'er'. */
  lazy val Valid: Set[String] = PinyinTable.tail.flatMap(_.tail).toSet.filter(_.nonEmpty) + "er"

  /** The longest valid pinyin syllable. */
  lazy val LongestValid: Int = Valid.map(_.length).max

  /** A map of initial and final sounds in pinyin, mapped to their combination if any. */
  lazy val FinalInitialMap: Map[(String, String), String] = {
    for (
      r <- PinyinTable.indices if r != 0; c <- PinyinTable(r).indices if c != 0; cell = PinyinTable(r)(c)
      if cell.nonEmpty
    )
      yield (PinyinTable(0)(c), PinyinTable(r)(0)) -> PinyinTable(r)(c)
  }.toMap

  /** @return the tones found in the pinyin syllables */
  def tones(pinyin: String): Seq[Int] = {
    // Luckily, the pinyin syllables are all split into separate words.
    pinyin.split("\\s+").map { _.find(Tones.contains).map(Tones(_)._2).getOrElse(0) }
  }

  /** Checks that the substring is a syllable containing no more than one tone marker. */
  def isSyllable(ss: String): Boolean =
    ss.headOption.exists(_.isLetter) && Valid.contains(ss.filterNot(_.isDigit).toLowerCase) && ss.count(_.isDigit) <= 1

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

  /** Converts pinyin text with accented characters to numbered pinyin.
    *
    * @param pinyin
    *   the pinyin text to convert
    * @param superscript
    *   if true, use superscript numbers for tones
    * @return
    *   the pinyin text with numbered tones
    */
  def toNumbered(pinyin: String, internalize: Boolean = false, superscript: Boolean = false): String = {
    val numbered =
      if (!internalize) split(pinyin).mkString
      else
        Tones.foldLeft(pinyin) { case (acc, (accented, (bare, tone))) =>
          acc.replace(accented.toString, s"$bare$tone")
        }
    if (!superscript)
      numbered.replace("⁰", "0").replace("¹", "1").replace("²", "2").replace("³", "3").replace("⁴", "4")
    else numbered.replace("0", "⁰").replace("1", "¹").replace("2", "²").replace("3", "³").replace("4", "⁴")
  }

  def split(input: String): Seq[String] = {
    // Use the numbered form by default and clean up any whitespace
    val in = toNumbered(input, internalize = true).replaceAll("\\s+", " ").trim
    // Check if the input has any spaces and presplit the strings if it does
    if (in.contains(" ")) return in.split(" ").map(split).flatMap(_ :+ " ").dropRight(1)
    if (in.isEmpty) return Seq.empty

    // Every key in the accumulator corresponds to an index in the input string.  The value associated with the key as
    // a tuple means that we can split the input cleanly into pinyin words up-to and including that index (exclusive).
    // The value is the start character that ends at that index.
    val splittables = in.indices.map(_ + 1).foldLeft(Seq(0 -> Int.MinValue)) { case (acc, end) =>
      acc
        .find { case (i, _) => end - i <= LongestValid && isSyllable(in.substring(i, end)) }
        .map(end -> _._1)
        .map(acc :+ _)
        .getOrElse(acc)
    }

    // Fast fail if we didn't end on the length of the string
    if (!splittables.lastOption.map(_._1).contains(in.length)) return Seq.empty

    // Reconstruct the split words
    var result = Seq[String]()
    var end = in.length
    while (end > 0) {
      val start = splittables.find(_._1 == end).get._2
      val (tone, bare) = in.substring(start, end).partition(_.isDigit)
      result = (bare + tone) +: result
      end = start
    }
    result
  }
}
