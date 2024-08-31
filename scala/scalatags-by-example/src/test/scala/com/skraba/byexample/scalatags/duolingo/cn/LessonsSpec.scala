package com.skraba.byexample.scalatags.duolingo.cn

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

class LessonsSpec extends AnyFunSpecLike with Matchers {

  def stringify(v: Vocab, toLower: Boolean): String = {
    val pinyin =
      if (toLower) Cheatsheet.toNumbered(v.pinyin, superscript = true).toLowerCase
      else Cheatsheet.toNumbered(v.pinyin, superscript = true)
    s"""Vocab("${v.cn}:$pinyin:${v.en}")"""
  }

  def stringify(l: Lessons, toLower: Boolean, sortByEn: Boolean): String = {
    val phrases = if (sortByEn) l.phrases.sortBy(_.en) else l.phrases
    s"""Lessons("${l.name}",
       |  ${phrases.sortBy(_.en).map(stringify(_, toLower)).mkString(",\n  ")}
       |)
       |""".stripMargin
  }

  describe("Phrases in a lesson") {
    it("should detect tones") {
      val phrase1 = Lessons.Health2.phrases.head
      phrase1.cn shouldBe "我今天生病了"
      phrase1.pinyin shouldBe "Wǒ jīntiān shěngbìng le"
      phrase1.en shouldBe "I am sick today."
    }

    it("can be stringified for cleanup") {
      stringify(Lessons.FruitsAndVegetables, toLower = true, sortByEn = true) shouldBe
        """Lessons("Fruits and Vegetables",
          |  Vocab("苹果:pi²ngguo³:Apple"),
          |  Vocab("香蕉:xia¹ngjia¹o:Banana"),
          |  Vocab("花椰菜:hua¹ye²ca⁴i:Broccoli"),
          |  Vocab("胡萝卜:hu²luo²bo:Carrot"),
          |  Vocab("白菜:ba²ica⁴i:Chinese cabbage"),
          |  Vocab("黄瓜:hua²nggua¹:Cucumber"),
          |  Vocab("茄子:qie²zi:Eggplant"),
          |  Vocab("大蒜:da⁴sua⁴n:Garlic"),
          |  Vocab("葡萄:pu²tao:Grape"),
          |  Vocab("青椒:qi¹ngjia¹o:Green pepper"),
          |  Vocab("芒果:ma²ngguo³:Mango"),
          |  Vocab("蘑菇:mo²gu:Mushroom"),
          |  Vocab("洋葱:ya²ngco¹ng:Onion"),
          |  Vocab("橙子:che²ngzi:Orange"),
          |  Vocab("菠萝:bo¹luo²:Pineapple"),
          |  Vocab("土豆:tu³do⁴u:Potato"),
          |  Vocab("菠菜:bo¹ca⁴i:Spinach"),
          |  Vocab("草莓:ca³ome²i:Strawberry"),
          |  Vocab("西红柿:xi¹ho²ngshi⁴:Tomato"),
          |  Vocab("西瓜:xi¹gua¹:Watermelon")
          |)
          |""".stripMargin
    }
  }
}
