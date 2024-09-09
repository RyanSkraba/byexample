package com.skraba.byexample.scalatags.duolingo.cn

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for the [[Pinyin]] helper object.
  */
class PinyinSpec extends AnyFunSpecLike with Matchers {

  describe("Pinyin helpers") {
    it("should detect tones") {
      Pinyin.tones("de yī líng wǔ èr") shouldBe Seq(0, 1, 2, 3, 4)
    }

    it("should translate numbered pinyin to accented pinyin") {
      Pinyin.toAccented("de yi1 li2ng wu3 e4r") shouldBe "de yī líng wǔ èr"
      Pinyin.toAccented("de yi¹ li²ng wu³ e⁴r") shouldBe "de yī líng wǔ èr"
      Pinyin.toAccented("DE YI1 LI2NG WU3 E4R") shouldBe "DE YĪ LÍNG WǓ ÈR"
      Pinyin.toAccented("DE YI¹ LI²NG WU³ E⁴R") shouldBe "DE YĪ LÍNG WǓ ÈR"
    }

    it("should translate accented pinyin to numbered pinyin") {
      Pinyin.toNumbered("de yī líng wǔ èr") shouldBe "de yi1 li2ng wu3 e4r"
      Pinyin.toNumbered("de yī líng wǔ èr", superscript = true) shouldBe "de yi¹ li²ng wu³ e⁴r"
      Pinyin.toNumbered("DE YĪ LÍNG WǓ ÈR") shouldBe "DE YI1 LI2NG WU3 E4R"
      Pinyin.toNumbered("DE YĪ LÍNG WǓ ÈR", superscript = true) shouldBe "DE YI¹ LI²NG WU³ E⁴R"
    }

    it("should split words into valid pinyin") {
      Pinyin.split("pi²ngguo³") shouldBe Seq("ping2", "guo3")
      Pinyin.split("píngguǒ") shouldBe Seq("ping2", "guo3")
      Pinyin.split("pingguo") shouldBe Seq("ping", "guo")

      // Single words that could also be two syllables
      Pinyin.split("guo") shouldBe Seq("guo")
      Pinyin.split("gu o") shouldBe Seq("gu", " ", "o")
      Pinyin.split("gúó") shouldBe Seq("gu2", "o2")

      // Some tiny words
      Pinyin.split("ae") shouldBe Seq("a", "e")
      Pinyin.split("ea") shouldBe Seq("e", "a")
      Pinyin.split("ao") shouldBe Seq("ao")
      Pinyin.split("oa") shouldBe Seq("o", "a")
      Pinyin.split("ayi") shouldBe Seq("a", "yi")
      Pinyin.split("yia") shouldBe Seq("yi", "a")
      Pinyin.split("a yi") shouldBe Seq("a", " ", "yi")
      Pinyin.split("yi a") shouldBe Seq("yi", " ", "a")

      Pinyin.split("mier") shouldBe Seq("mi", "er")
      Pinyin.split("eryi") shouldBe Seq("er", "yi")

      // Some long words
      Pinyin.split("zhuangchuangshuang") shouldBe Seq("zhuang", "chuang", "shuang")
      Pinyin.split("zhuangashuang") shouldBe Seq("zhuan", "ga", "shuang")
      Pinyin.split("zhuang ashuang") shouldBe Seq("zhuang", " ", "a", "shuang")
    }

    it("should ignore case when splitting words into valid pinyin") {
      Pinyin.split("Pi²ngguo³") shouldBe Seq("Ping2", "guo3")
      Pinyin.split("Píngguǒ") shouldBe Seq("Ping2", "guo3")
      Pinyin.split("Pingguo") shouldBe Seq("Ping", "guo")
    }
  }

  describe("The pinyin map") {
    it("should have the expected size") {
      Pinyin.FinalInitialMap.values should have size 409
      Pinyin.FinalInitialMap.keys.map(_._1).toSet should have size 22
      Pinyin.FinalInitialMap.keys.map(_._2).toSet should have size 35
      Pinyin.Initials should have size 22
      Pinyin.Finals should have size 35
      Pinyin.Valid should have size 410
      Pinyin.LongestValid shouldBe 6
    }
  }
}
