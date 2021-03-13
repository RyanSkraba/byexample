package com.skraba.byexample.scala.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Markd]]
  */
class MarkdSpec extends AnyFunSpecLike with Matchers {

  describe("Parsing markdown contents") {

    it("should just wrap simple contents in a Paragraph") {
      val contents = "Hello world"

      val md = Markd.parse(contents)
      md shouldBe Paragraph("Hello world")

      val cleaned = md.build().toString
      cleaned shouldBe
        """Hello world
          |""".stripMargin
      Markd.parse(cleaned) shouldBe md
    }

    it("should separate into level 1 headers") {
      val contents = """
          |English
          |===
          |Hello world
          |# French
          |Bonjour tout le monde""".stripMargin

      val md = Markd.parse(contents)

      val cleaned = md.build().toString
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
          |""".stripMargin
      Markd.parse(cleaned) shouldBe md
    }

    it("should separate into headers and links") {
      val contents = """
          |outside
          |[refout]: https://www.refout.com
          |# header1
          |h1txt
          |## header1a
          |h1atxt
          |[ref1a]: https://www.ref1a.com
          |## header1b
          |h1btxt
          |### header1b1
          |h1b1txt
          |# header2
          |h2txt
          |[ref2]: https://www.ref2.com
          |## header2a
          |h2atxt
          |## header2b
          |h2btxt
          |[ref2b]: https://www.ref2b.com
          |""".stripMargin

      val md = Markd.parse(contents)

      val cleaned = md.build().toString
      cleaned shouldBe
        """outside
          |[refout]: https://www.refout.com
          |
          |header1
          |==============================================================================
          |
          |h1txt
          |
          |header1a
          |------------------------------------------------------------------------------
          |
          |h1atxt
          |[ref1a]: https://www.ref1a.com
          |
          |header1b
          |------------------------------------------------------------------------------
          |
          |h1btxt
          |
          |### header1b1
          |
          |h1b1txt
          |
          |header2
          |==============================================================================
          |
          |h2txt
          |[ref2]: https://www.ref2.com
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
          |[ref2b]: https://www.ref2b.com
          |""".stripMargin
      Markd.parse(cleaned) shouldBe md
    }
  }
}
