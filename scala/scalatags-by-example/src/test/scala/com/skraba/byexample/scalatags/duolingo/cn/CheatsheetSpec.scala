package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.duolingo.cn.CheatsheetSpec.assumeCheatsheetNetwork
import org.scalactic.{Prettifier, source}
import org.scalatest.{Assertion, Assertions}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.File

class CheatsheetSpec extends AnyFunSpecLike with Matchers {

  describe("A VocabGroup") {

    it("should write a pretty group of words") {
      assumeCheatsheetNetwork()
      val vs: Map[String, Vocab] = Cheatsheet.All.vocab.map(v => (v.cn, v)).toMap
      val vg = SvgLessonGroup("你,好,再,见,再见".split(",").toSeq.map(vs.apply), Some("Lesson 1"))
      Svg.toFile(File("/tmp/duolingo.lesson1.svg"), vg.toSvg(Svg.attrTranslate(50, 10)), 200, 1000)
    }

    it("writes a pretty image") {
      assumeCheatsheetNetwork()
      val s = Cheatsheet(vocab = Cheatsheet.All.vocab.filter(_.cn.nonEmpty))
      Svg.toFile(File("/tmp/duolingo.all.svg"), s.toSvg(Svg.attrTranslate(50, 10)), 200, 1000)
    }

    it("should only include numbers") {
      // Just get the vocabulary we're interested in.
      assumeCheatsheetNetwork()
      val lesson = Cheatsheet.All.vocab.filter(v => v.lesson == "Numbers").map(v => (v.cn(0), v)).toMap
      val cheat = Cheatsheet(vocab = "零一二三四五六七八九十百元".map(lesson.apply)).toSvg()(Svg.attrTranslate(50, 10))
      Svg.toFile(File("/tmp/duolingo.numbers.svg"), cheat, 200, 150)
    }
  }

}

object CheatsheetSpec {

  def assumeCheatsheetNetwork()(implicit prettifier: Prettifier, pos: source.Position): Assertion = {
    Assertions.assume(CheatsheetAll.nonEmpty, "The network is not available")(prettifier, pos)
  }

  /** Loading the cheatsheet for the Duolingo chinese lessons or None if the network is not available. */
  val CheatsheetAll: Option[Cheatsheet] =
    try { Some(Cheatsheet.All) }
    catch { case e: java.net.UnknownHostException if e.getMessage == "raw.githubusercontent.com" => None }
}
