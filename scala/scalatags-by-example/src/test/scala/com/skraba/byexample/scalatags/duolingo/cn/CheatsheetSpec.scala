package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.File

class CheatsheetSpec extends AnyFunSpecLike with Matchers {

  describe("Creating a cheatsheet") {

    it("should detect tones") {
      Cheatsheet.tones("de yī líng wǔ èr") shouldBe Seq(0, 1, 2, 3, 4)
    }
  }

  describe("A VocabGroup") {

    it("should write a pretty group of words") {
      val vs: Map[String, Vocab] =
        Cheatsheet.All.vocab.map(v => (v.cn, v)).toMap
      val words = Seq()

      val vg =
        VocabGroup(
          "你,好,再,见,再见".split(",").toSeq.map(vs.apply),
          Some("Lesson 1")
        )

      Svg.toFile(
        File("/tmp/duolingo.lesson1.svg"),
        vg.toSvg(Svg.attrTranslate(50, 10)),
        200,
        1000
      )
    }

    it("writes a pretty image") {
      val s = Cheatsheet(vocab = Cheatsheet.All.vocab.filter(_.cn.nonEmpty))
      Svg.toFile(
        File("/tmp/duolingo.all.svg"),
        s.toSvg(Svg.attrTranslate(50, 10)),
        200,
        1000
      )
    }

    it("should only include numbers") {
      // Just get the vocabulary we're interested in.
      val lesson = Cheatsheet.All.vocab
        .filter(v => v.lesson == "Numbers")
        .map(v => (v.cn(0), v))
        .toMap

      val cheat = Cheatsheet(vocab = "零一二三四五六七八九十百元".map(lesson.apply))
        .toSvg()(Svg.attrTranslate(50, 10))

      Svg.toFile(
        File("/tmp/duolingo.numbers.svg"),
        cheat,
        200,
        150
      )
    }
  }

}
