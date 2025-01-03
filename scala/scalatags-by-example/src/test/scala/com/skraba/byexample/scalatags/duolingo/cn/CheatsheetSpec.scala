package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.duolingo.cn.CheatsheetSpec.assumeCheatsheetNetwork
import org.scalactic.{Prettifier, source}
import org.scalatest.{Assertion, Assertions, BeforeAndAfterAll}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.{Directory, File}

class CheatsheetSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  describe("A VocabGroup") {

    val allVocab: Map[String, Vocab] = Cheatsheet.All.vocab.map(v => (v.cn, v)).toMap

    it("should write a pretty group of words") {
      assumeCheatsheetNetwork()
      val vs: Seq[Vocab] = "你,好,再,见,再见".split(",").toSeq.map(allVocab.apply)
      val slg = SvgLessonGroup(Lesson("Lesson1", vs: _*))
      Svg.toFile(Tmp / File("duolingo.lesson1.svg"), slg.toSvg(Svg.attrTranslate(50, 10)), 200, 1000)
    }

    it("writes a pretty image") {
      assumeCheatsheetNetwork()
      val cs = Cheatsheet(vocab = Cheatsheet.All.vocab.filter(_.cn.nonEmpty))
      Svg.toFile(Tmp / File("duolingo.all.svg"), cs.toSvg(Svg.attrTranslate(50, 10)), 200, 1000)
    }

    it("should only include numbers") {
      // Just get the vocabulary we're interested in.
      assumeCheatsheetNetwork()
      val cheat =
        Cheatsheet(vocab = "零一二三四五六七八九十百元".map(_.toString).map(allVocab.apply)).toSvg()(Svg.attrTranslate(50, 10))
      Svg.toFile(Tmp / File("duolingo.numbers.svg"), cheat, 200, 150)
    }

    it("should print lessons") {
      assumeCheatsheetNetwork()
      Svg.toFile(
        Tmp / File("fruits.vegetables.svg"),
        SvgLessonGroup(Lesson.FruitsAndVegetables).toSvg(Svg.attrTranslate(50, 10)),
        200,
        1000
      )
      Svg.toFile(
        Tmp / File("colours.svg"),
        SvgLessonGroup(Lesson.Colours).toSvg(Svg.attrTranslate(50, 10)),
        200,
        1000
      )
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
