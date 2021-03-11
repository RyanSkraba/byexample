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
          |# French
          |Bonjour tout le monde""".stripMargin

      val md = Markd.parse("", contents)
      md.title shouldBe ""
      md.text shouldBe ""
      md.sub should have size 2
      md.sub.head.title shouldBe "English"
      md.sub.head.text shouldBe "Hello world"
      md.sub.head.sub shouldBe Seq.empty
      md.sub.head.linkRefs shouldBe Seq.empty
      md.sub(1).title shouldBe "French"
      md.sub(1).text shouldBe "Bonjour tout le monde"
      md.sub(1).sub shouldBe Seq.empty
      md.sub(1).linkRefs shouldBe Seq.empty
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

    it("should separate into headers and links") {
      val contents = """
          |outside
          |# h1
          |h1txt
          |## header1a
          |h1atxt
          |## header1b
          |h1btxt
          |# header2
          |h2txt
          |## header2a
          |h2atxt
          |## header2b
          |h2btxt
          |""".stripMargin

      val md = Markd.parse("", contents)

      val cleaned = md.build(createTitle = Markd.SectionH1Title).toString
      cleaned shouldBe
        """outside
          |
          |h1
          |==============================================================================
          |
          |h1txt
          |
          |header1a
          |------------------------------------------------------------------------------
          |
          |h1atxt
          |
          |header1b
          |------------------------------------------------------------------------------
          |
          |h1btxt
          |
          |header2
          |==============================================================================
          |
          |h2txt
          |
          |header2a
          |------------------------------------------------------------------------------
          |
          |h2atxt
          |
          |header2b
          |------------------------------------------------------------------------------
          |
          |h2btxt
          |
          |""".stripMargin
      Markd.parse("", cleaned) shouldBe md
    }
  }
}
