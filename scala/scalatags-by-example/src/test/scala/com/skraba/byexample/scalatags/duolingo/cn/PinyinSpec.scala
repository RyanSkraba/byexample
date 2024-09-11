package com.skraba.byexample.scalatags.duolingo.cn

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for the [[Pinyin]] helper object.
  */
class PinyinSpec extends AnyFunSpecLike with Matchers {

  describe("Pinyin helpers") {
    it("should detect tones") {
      Pinyin.tones("de yī líng wǔ èr") shouldBe Seq(0, 1, 2, 3, 4)
      Pinyin.tones("deyīlíngwǔèr") shouldBe Seq(0, 1, 2, 3, 4)
    }

    ignore("should translate pinyin with tone markers to accented pinyin") {
      Pinyin.toAccented("de yi1 ling2 wu3 er4") shouldBe "de yī líng wǔ èr"
      Pinyin.toAccented("de yi¹ ling² wu³ er⁴") shouldBe "de yī líng wǔ èr"
      Pinyin.toAccented("DE YI1 LING2 WU3 ER4") shouldBe "DE YĪ LÍNG WǓ ÈR"
      Pinyin.toAccented("DE YI¹ LING² WU³ ER⁴") shouldBe "DE YĪ LÍNG WǓ ÈR"
      Pinyin.toAccented("deyi1ling2wu3er4") shouldBe "deyīlíngwǔèr"
      Pinyin.toAccented("deyi¹ling²wu³er⁴") shouldBe "deyīlíngwǔèr"
      Pinyin.toAccented("DEYI1LING2WU3ER4") shouldBe "DEYĪLÍNGWǓÈR"
      Pinyin.toAccented("DEYI¹LING²WU³ER⁴") shouldBe "DEYĪLÍNGWǓÈR"
    }

    it("should translate pinyin with internalized vowel markers to accented pinyin") {
      Pinyin.toAccented("de yi1 li2ng wu3 e4r") shouldBe "de yī líng wǔ èr"
      Pinyin.toAccented("de yi¹ li²ng wu³ e⁴r") shouldBe "de yī líng wǔ èr"
      Pinyin.toAccented("DE YI1 LI2NG WU3 E4R") shouldBe "DE YĪ LÍNG WǓ ÈR"
      Pinyin.toAccented("DE YI¹ LI²NG WU³ E⁴R") shouldBe "DE YĪ LÍNG WǓ ÈR"
      Pinyin.toAccented("deyi1li2ngwu3e4r") shouldBe "deyīlíngwǔèr"
      Pinyin.toAccented("deyi¹li²ngwu³e⁴r") shouldBe "deyīlíngwǔèr"
      Pinyin.toAccented("DEYI1LI2NGWU3E4R") shouldBe "DEYĪLÍNGWǓÈR"
      Pinyin.toAccented("DEYI¹LI²NGWU³E⁴R") shouldBe "DEYĪLÍNGWǓÈR"
    }

    it("should translate accented pinyin to pinyin with internalized vowel markers") {
      Pinyin.toNumbered("de yī líng wǔ èr", internalize = true) shouldBe "de yi1 li2ng wu3 e4r"
      Pinyin.toNumbered("de yī líng wǔ èr", internalize = true, superscript = true) shouldBe "de yi¹ li²ng wu³ e⁴r"
      Pinyin.toNumbered("DE YĪ LÍNG WǓ ÈR", internalize = true) shouldBe "DE YI1 LI2NG WU3 E4R"
      Pinyin.toNumbered("DE YĪ LÍNG WǓ ÈR", internalize = true, superscript = true) shouldBe "DE YI¹ LI²NG WU³ E⁴R"
      Pinyin.toNumbered("deyīlíngwǔèr", internalize = true) shouldBe "deyi1li2ngwu3e4r"
      Pinyin.toNumbered("deyīlíngwǔèr", internalize = true, superscript = true) shouldBe "deyi¹li²ngwu³e⁴r"
      Pinyin.toNumbered("DEYĪLÍNGWǓÈR", internalize = true) shouldBe "DEYI1LI2NGWU3E4R"
      Pinyin.toNumbered("DEYĪLÍNGWǓÈR", internalize = true, superscript = true) shouldBe "DEYI¹LI²NGWU³E⁴R"
    }

    it("should translate accented pinyin to pinyin tone markers") {
      Pinyin.toNumbered("de yī líng wǔ èr") shouldBe "de yi1 ling2 wu3 er4"
      Pinyin.toNumbered("de yī líng wǔ èr", superscript = true) shouldBe "de yi¹ ling² wu³ er⁴"
      Pinyin.toNumbered("DE YĪ LÍNG WǓ ÈR") shouldBe "DE YI1 LING2 WU3 ER4"
      Pinyin.toNumbered("DE YĪ LÍNG WǓ ÈR", superscript = true) shouldBe "DE YI¹ LING² WU³ ER⁴"
      Pinyin.toNumbered("deyīlíngwǔèr") shouldBe "deyi1ling2wu3er4"
      Pinyin.toNumbered("deyīlíngwǔèr", superscript = true) shouldBe "deyi¹ling²wu³er⁴"
      Pinyin.toNumbered("DEYĪLÍNGWǓÈR") shouldBe "DEYI1LING2WU3ER4"
      Pinyin.toNumbered("DEYĪLÍNGWǓÈR", superscript = true) shouldBe "DEYI¹LING²WU³ER⁴"
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
