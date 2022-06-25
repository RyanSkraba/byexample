package com.skraba.byexample.scalatags.duolingo.cn

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

class LessonsSpec extends AnyFunSpecLike with Matchers {

  describe("Phrases in a lesson") {
    it("should detect tones") {

      val phrase1 = Lessons.Health2.phrases.head
      phrase1.cn shouldBe "我今天生病了"
      phrase1.pinyin shouldBe "Wǒ jīntiān shěngbìng le"
      phrase1.en shouldBe "I am sick today."
    }
  }
}
