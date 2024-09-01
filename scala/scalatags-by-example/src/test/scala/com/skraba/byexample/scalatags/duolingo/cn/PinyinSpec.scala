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
  }
}
