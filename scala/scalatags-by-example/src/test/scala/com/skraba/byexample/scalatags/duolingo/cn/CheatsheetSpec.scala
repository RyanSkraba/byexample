package com.skraba.byexample.scalatags.duolingo.cn

import com.skraba.byexample.scalatags.Svg
import org.scalactic.source.Position
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.{BeforeAndAfterAll, Tag}

import scala.reflect.io.{Directory, File}

class CheatsheetSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  // TODO: TmpDir from docopts4s

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  /** Disabling unit tests that require network to continue. */
  class RequiresCheatsheetItWork() extends ItWord {

    /** Loading the cheatsheet for the Duolingo chinese lessons or None if the network is not available. */
    val CheatsheetOpt: Option[Cheatsheet] =
      try { Some(Cheatsheet.All) }
      catch { case e: java.net.UnknownHostException if e.getMessage == "raw.githubusercontent.com" => None }

    override def apply(specText: String, testTags: Tag*)(testFun: => Any)(implicit pos: Position): Unit = {
      // If we override `it`, we can fall back on the equivalent `they` for the tests that are enabled.
      if (CheatsheetOpt.nonEmpty) they(specText, testTags: _*)(testFun)
      else ignore(specText)((): Unit)
    }
  }

  /** If these are top-level in the class, they get the "right" highlighting in IntelliJ. */
  val itRequiresCheatsheet = new RequiresCheatsheetItWork()

  describe("A VocabGroup") {

    lazy val allVocab: Map[String, Vocab] = Cheatsheet.All.vocab.map(v => (v.cn, v)).toMap

    itRequiresCheatsheet("should write a pretty group of words") {
      val vs: Seq[Vocab] = "你,好,再,见,再见".split(",").toSeq.map(allVocab.apply)
      val slg = SvgLessonGroup(Lesson("Lesson1", vs: _*))
      Svg.toFile(Tmp / File("duolingo.lesson1.svg"), slg.toSvg(Svg.attrTranslate(50, 10)), 200, 1000)
    }

    itRequiresCheatsheet("writes a pretty image") {
      val cs = Cheatsheet(vocab = Cheatsheet.All.vocab.filter(_.cn.nonEmpty))
      Svg.toFile(Tmp / File("duolingo.all.svg"), cs.toSvg(Svg.attrTranslate(50, 10)), 200, 1000)
    }

    itRequiresCheatsheet("should only include numbers") {
      // Just get the vocabulary we're interested in.
      val cheat =
        Cheatsheet(vocab = "零一二三四五六七八九十百元".map(_.toString).map(allVocab.apply)).toSvg()(Svg.attrTranslate(50, 10))
      Svg.toFile(Tmp / File("duolingo.numbers.svg"), cheat, 200, 150)
    }

    itRequiresCheatsheet("should print lessons") {
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
