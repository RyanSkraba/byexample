package com.skraba.byexample.scalatags.duolingo.cn

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

/** A lesson is a collection of vocabulary words and/or phrases.
  *
  * @param title
  *   The name of the lesson.
  * @param vocab
  *   The vocabularies in the lesson.
  */
case class Lesson(title: Option[String], vocab: Vocab*)

object Lesson {

  def apply(title: String, vocab: Vocab*): Lesson = Lesson(Some(title), vocab: _*)

  def apply(vocab: Vocab*): Lesson = Lesson(None, vocab: _*)

  val Health2: Lesson = Lesson(
    "Health 2",
    Vocab("我今天生病了:Wo³ ji¹ntia¹n she³ngbi⁴ng le:I am sick today"),
    Vocab("我感冒了:Wo³ ga³nma⁴o le:I have a cold"),
    Vocab("她下班了:Ta¹ xia³ba¹n le:She got off work")
  )

  val FruitsAndVegetables: Lesson = Lesson(
    "Fruits and Vegetables",
    Vocab("苹果:pi²ngguo³:apple"),
    Vocab("香蕉:xia¹ngjia¹o:banana"),
    Vocab("花椰菜:hua¹ye²ca⁴i:broccoli"),
    Vocab("胡萝卜:hu²luo²bo:carrot"),
    Vocab("白菜:ba²ica⁴i:chinese cabbage"),
    Vocab("黄瓜:hua²nggua¹:cucumber"),
    Vocab("茄子:qie²zi:eggplant"),
    Vocab("大蒜:da⁴sua⁴n:garlic"),
    Vocab("葡萄:pu²tao:grape"),
    Vocab("青椒:qi¹ngjia¹o:green pepper"),
    Vocab("芒果:ma²ngguo³:mango"),
    Vocab("蘑菇:mo²gu:mushroom"),
    Vocab("洋葱:ya²ngco¹ng:onion"),
    Vocab("橙子:che²ngzi:orange"),
    Vocab("菠萝:bo¹luo²:pineapple"),
    Vocab("土豆:tu³do⁴u:potato"),
    Vocab("菠菜:bo¹ca⁴i:spinach"),
    Vocab("草莓:ca³ome²i:strawberry"),
    Vocab("西红柿:xi¹ho²ngshi⁴:tomato"),
    Vocab("西瓜:xi¹gua¹:watermelon")
  )
}
