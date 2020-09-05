package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FunSpecLike, Matchers}

import scala.reflect.io.File

@RunWith(classOf[JUnitRunner])
class CheatsheetSpec extends FunSpecLike with Matchers {

  describe("Creating a cheatsheet") {

    it("should detect tones") {
      Cheatsheet.tones("de yī líng wǔ èr") shouldBe Seq(0, 1, 2, 3, 4)
    }

    it("writes a pretty image") {
      val s = Cheatsheet(vocab = Cheatsheet.All.vocab.filter(_.cn.length >= 1))
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
        .filter(v => v.info(2) == "Numbers")
        .map(v => (v.cn(0), v))
        .toMap

      val cheat = Cheatsheet(vocab = "零一二三四五六七八九十百元".map(lesson.apply))
        .toSvg()(Svg.attrTranslate(50, 10))

      Svg.toFile(
        File("/tmp/duolingo.numbers.svg"),
        cheat,
        200,
        150
      );
    }
  }

}
