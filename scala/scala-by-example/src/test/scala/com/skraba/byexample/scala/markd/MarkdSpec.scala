package com.skraba.byexample.scala.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Markd]]
  */
class MarkdSpec extends AnyFunSpecLike with Matchers {

  describe("Parsing markdown contents") {

    it("should just wrap simple contents") {
      val contents = "Hello world"

      val md = Markd.parse("", contents)
      md.title shouldBe ""
      md.text shouldBe "Hello world"
      md.sub shouldBe Seq.empty
      md.linkRefs shouldBe Seq.empty
    }

    it("should separate into level 1 headers") {
      val contents = """
          |English
          |===
          |Hello world
          |French
          |===
          |Bonjour tout le monde""".stripMargin

      val md = Markd.parse("", contents)
      md.title shouldBe ""
      md.text shouldBe ""
      md.sub should have size 2
      md.sub.head._2.title shouldBe "English"
      md.sub.head._2.text shouldBe "Hello world"
      md.sub.head._2.sub shouldBe Seq.empty
      md.sub.head._2.linkRefs shouldBe Seq.empty
      md.sub(1)._2.title shouldBe "French"
      md.sub(1)._2.text shouldBe "Bonjour tout le monde"
      md.sub(1)._2.sub shouldBe Seq.empty
      md.sub(1)._2.linkRefs shouldBe Seq.empty
      md.linkRefs shouldBe Seq.empty

      val cleaned = md.build(createTitle = Markd.SectionH1Title).toString
      cleaned shouldBe
        """English
          |==============================================================================
          |
          |Hello world
          |
          |French
          |==============================================================================
          |
          |Bonjour tout le monde
          |
          |""".stripMargin
      Markd.parse("", cleaned) shouldBe md
    }
  }
}
