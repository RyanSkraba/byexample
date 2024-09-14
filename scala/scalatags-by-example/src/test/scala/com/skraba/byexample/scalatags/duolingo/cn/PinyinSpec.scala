package com.skraba.byexample.scalatags.duolingo.cn

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for the [[Pinyin]] helper object.
  */
class PinyinSpec extends AnyFunSpecLike with Matchers {

  describe("Pinyin helpers") {

    it("should detect tones") {
      Pinyin.tones("Běijīng Dàxué de") shouldBe Seq(3, 1, 4, 2, 0)
      Pinyin.tones("běi jīng dà xué de") shouldBe Seq(3, 1, 4, 2, 0)
      Pinyin.tones("běijīngdàxuéde") shouldBe Seq(3, 1, 4, 2, 0)
    }

    it("should correctly mark all single vowels from tones") {
      Pinyin.toAccented("la le li mo lu lü") shouldBe "la le li mo lu lü"
      Pinyin.toAccented("la0 le0 li0 mo0 lu0 lü0") shouldBe "la le li mo lu lü"
      Pinyin.toAccented("la1 le1 li1 mo1 lu1 lü1") shouldBe "lā lē lī mō lū l."
      Pinyin.toAccented("la2 le2 li2 mo2 lu2 lü2") shouldBe "lá lé lí mó lú l."
      Pinyin.toAccented("la3 le3 li3 mo3 lu3 lü3") shouldBe "lǎ lě lǐ mǒ lǔ lǚ"
      Pinyin.toAccented("la4 le4 li4 mo4 lu4 lü4") shouldBe "là lè lì mò lù lǜ"
      Pinyin.toAccented("la5 le5 li5 mo5 lu5 lü5") shouldBe "la le li mo lu lü"
      Pinyin.toAccented("LA LE LI MO LU LÜ") shouldBe "LA LE LI MO LU LÜ"
      Pinyin.toAccented("LA0 LE0 LI0 MO0 LU0 LÜ0") shouldBe "LA LE LI MO LU LÜ"
      Pinyin.toAccented("LA1 LE1 LI1 MO1 LU1 LÜ1") shouldBe "LĀ LĒ LĪ MŌ LŪ L."
      Pinyin.toAccented("LA2 LE2 LI2 MO2 LU2 LÜ2") shouldBe "LÁ LÉ LÍ MÓ LÚ L."
      Pinyin.toAccented("LA3 LE3 LI3 MO3 LU3 LÜ3") shouldBe "LǍ LĚ LǏ MǑ LǓ LǙ"
      Pinyin.toAccented("LA4 LE4 LI4 MO4 LU4 LÜ4") shouldBe "LÀ LÈ LÌ MÒ LÙ LǛ"
      Pinyin.toAccented("LA5 LE5 LI5 MO5 LU5 LÜ5") shouldBe "LA LE LI MO LU LÜ"
    }

    it("should translate pinyin with tone markers to accented pinyin") {
      Pinyin.toAccented("Bei3jing1 Da4xue2 de") shouldBe "Běijīng Dàxué de"
      Pinyin.toAccented("Bei³jing¹ Da⁴xue² de") shouldBe "Běijīng Dàxué de"
      Pinyin.toAccented("bei3jing1 da4xue2 de") shouldBe "běijīng dàxué de"
      Pinyin.toAccented("bei³jing¹ da⁴xue² de") shouldBe "běijīng dàxué de"
      Pinyin.toAccented("BEI3JING1 DA4XUE2 DE") shouldBe "BĚIJĪNG DÀXUÉ DE"
      Pinyin.toAccented("BEI³JING¹ DA⁴XUE² DE") shouldBe "BĚIJĪNG DÀXUÉ DE"
      Pinyin.toAccented("Bei3jing1Da4xue2de") shouldBe "BěijīngDàxuéde"
      Pinyin.toAccented("Bei³jing¹Da⁴xue²de") shouldBe "BěijīngDàxuéde"
      Pinyin.toAccented("bei3jing1da4xue2de") shouldBe "běijīngdàxuéde"
      Pinyin.toAccented("bei³jing¹da⁴xue²de") shouldBe "běijīngdàxuéde"
      Pinyin.toAccented("BEI3JING1DA4XUE2DE") shouldBe "BĚIJĪNGDÀXUÉDE"
      Pinyin.toAccented("BEI³JING¹DA⁴XUE²DE") shouldBe "BĚIJĪNGDÀXUÉDE"
    }

    it("should translate pinyin with internalized vowel markers to accented pinyin") {
      Pinyin.toAccented("Be3iji1ng Da4xue2 de") shouldBe "Běijīng Dàxué de"
      Pinyin.toAccented("Be³iji¹ng Da⁴xue² de") shouldBe "Běijīng Dàxué de"
      Pinyin.toAccented("be3iji1ng da4xue2 de") shouldBe "běijīng dàxué de"
      Pinyin.toAccented("be³iji¹ng da⁴xue² de") shouldBe "běijīng dàxué de"
      Pinyin.toAccented("BE3IJI1NG DA4XUE2 DE") shouldBe "BĚIJĪNG DÀXUÉ DE"
      Pinyin.toAccented("BE³IJI¹NG DA⁴XUE² DE") shouldBe "BĚIJĪNG DÀXUÉ DE"
      Pinyin.toAccented("Be3iji1ngDa4xue2de") shouldBe "BěijīngDàxuéde"
      Pinyin.toAccented("Be³iji¹ngDa⁴xue²de") shouldBe "BěijīngDàxuéde"
      Pinyin.toAccented("be3iji1ngda4xue2de") shouldBe "běijīngdàxuéde"
      Pinyin.toAccented("be³iji¹ngda⁴xue²de") shouldBe "běijīngdàxuéde"
      Pinyin.toAccented("BE3IJI1NGDA4XUE2DE") shouldBe "BĚIJĪNGDÀXUÉDE"
      Pinyin.toAccented("BE³IJI¹NGDA⁴XUE²DE") shouldBe "BĚIJĪNGDÀXUÉDE"
    }

    it("should translate accented pinyin to pinyin with internalized vowel markers") {
      Pinyin.toNumbered("Běijīng Dàxué de", internalize = true) shouldBe "Be3iji1ng Da4xue2 de"
      Pinyin.toNumbered("Běijīng Dàxué de", superscript = true, internalize = true) shouldBe "Be³iji¹ng Da⁴xue² de"
      Pinyin.toNumbered("běijīng dàxué de", internalize = true) shouldBe "be3iji1ng da4xue2 de"
      Pinyin.toNumbered("běijīng dàxué de", superscript = true, internalize = true) shouldBe "be³iji¹ng da⁴xue² de"
      Pinyin.toNumbered("BĚIJĪNG DÀXUÉ DE", internalize = true) shouldBe "BE3IJI1NG DA4XUE2 DE"
      Pinyin.toNumbered("BĚIJĪNG DÀXUÉ DE", superscript = true, internalize = true) shouldBe "BE³IJI¹NG DA⁴XUE² DE"
      Pinyin.toNumbered("BěijīngDàxuéde", internalize = true) shouldBe "Be3iji1ngDa4xue2de"
      Pinyin.toNumbered("BěijīngDàxuéde", superscript = true, internalize = true) shouldBe "Be³iji¹ngDa⁴xue²de"
      Pinyin.toNumbered("běijīngdàxuéde", internalize = true) shouldBe "be3iji1ngda4xue2de"
      Pinyin.toNumbered("běijīngdàxuéde", superscript = true, internalize = true) shouldBe "be³iji¹ngda⁴xue²de"
      Pinyin.toNumbered("BĚIJĪNGDÀXUÉDE", internalize = true) shouldBe "BE3IJI1NGDA4XUE2DE"
      Pinyin.toNumbered("BĚIJĪNGDÀXUÉDE", superscript = true, internalize = true) shouldBe "BE³IJI¹NGDA⁴XUE²DE"
    }

    it("should translate accented pinyin to pinyin tone markers") {
      Pinyin.toNumbered("Běijīng Dàxué de") shouldBe "Bei3jing1 Da4xue2 de"
      Pinyin.toNumbered("Běijīng Dàxué de", superscript = true) shouldBe "Bei³jing¹ Da⁴xue² de"
      Pinyin.toNumbered("běijīng dàxué de") shouldBe "bei3jing1 da4xue2 de"
      Pinyin.toNumbered("běijīng dàxué de", superscript = true) shouldBe "bei³jing¹ da⁴xue² de"
      Pinyin.toNumbered("BĚIJĪNG DÀXUÉ DE") shouldBe "BEI3JING1 DA4XUE2 DE"
      Pinyin.toNumbered("BĚIJĪNG DÀXUÉ DE", superscript = true) shouldBe "BEI³JING¹ DA⁴XUE² DE"
      Pinyin.toNumbered("BěijīngDàxuéde") shouldBe "Bei3jing1Da4xue2de"
      Pinyin.toNumbered("BěijīngDàxuéde", superscript = true) shouldBe "Bei³jing¹Da⁴xue²de"
      Pinyin.toNumbered("běijīngdàxuéde") shouldBe "bei3jing1da4xue2de"
      Pinyin.toNumbered("běijīngdàxuéde", superscript = true) shouldBe "bei³jing¹da⁴xue²de"
      Pinyin.toNumbered("BĚIJĪNGDÀXUÉDE") shouldBe "BEI3JING1DA4XUE2DE"
      Pinyin.toNumbered("BĚIJĪNGDÀXUÉDE", superscript = true) shouldBe "BEI³JING¹DA⁴XUE²DE"
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
