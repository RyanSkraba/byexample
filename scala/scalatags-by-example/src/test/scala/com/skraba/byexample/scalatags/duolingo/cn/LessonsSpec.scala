package com.skraba.byexample.scalatags.duolingo.cn

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

class LessonsSpec extends AnyFunSpecLike with Matchers {

  def stringify(v: Vocab, toLower: Boolean): String = {
    val pinyin =
      if (toLower) Pinyin.toNumbered(v.pinyin, superscript = true).toLowerCase
      else Pinyin.toNumbered(v.pinyin, superscript = true)
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
      phrase1.en shouldBe "I am sick today"
    }

    it("can be stringified for cleanup") {
      stringify(Lessons.FruitsAndVegetables, toLower = true, sortByEn = true) shouldBe
        """Lessons("Fruits and Vegetables",
          |  Vocab("苹果:ping²guo³:apple"),
          |  Vocab("香蕉:xiang¹jiao¹:banana"),
          |  Vocab("花椰菜:hua¹ye²cai⁴:broccoli"),
          |  Vocab("胡萝卜:hu²luo²bo:carrot"),
          |  Vocab("白菜:bai²cai⁴:chinese cabbage"),
          |  Vocab("黄瓜:huang²gua¹:cucumber"),
          |  Vocab("茄子:qie²zi:eggplant"),
          |  Vocab("大蒜:da⁴suan⁴:garlic"),
          |  Vocab("葡萄:pu²tao:grape"),
          |  Vocab("青椒:qing¹jiao¹:green pepper"),
          |  Vocab("芒果:mang²guo³:mango"),
          |  Vocab("蘑菇:mo²gu:mushroom"),
          |  Vocab("洋葱:yang²cong¹:onion"),
          |  Vocab("橙子:cheng²zi:orange"),
          |  Vocab("菠萝:bo¹luo²:pineapple"),
          |  Vocab("土豆:tu³dou⁴:potato"),
          |  Vocab("菠菜:bo¹cai⁴:spinach"),
          |  Vocab("草莓:cao³mei²:strawberry"),
          |  Vocab("西红柿:xi¹hong²shi⁴:tomato"),
          |  Vocab("西瓜:xi¹gua¹:watermelon")
          |)
          |""".stripMargin
    }
  }
}
