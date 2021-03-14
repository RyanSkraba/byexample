package com.skraba.byexample.scala.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Markd]]
  */
class MarkdSpec extends AnyFunSpecLike with Matchers {

  describe("Parsing markdown contents") {

    it("should just wrap simple contents in a Paragraph") {
      val contents = "Hello world"

      val md = Header.parse(contents)
      md shouldBe Header(0, "", Paragraph("Hello world"))

      val cleaned = md.build().toString
      cleaned shouldBe
        """Hello world
          |""".stripMargin
      Header.parse(cleaned) shouldBe md
    }

    it("should separate into level 1 headers") {
      val contents = """
          |English
          |===
          |Hello world
          |# French
          |Bonjour tout le monde""".stripMargin

      val md = Header.parse(contents)
      md.sub should have size 2

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
      Header.parse(cleaned) shouldBe md
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

      val md = Header.parse(contents)

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
      Header.parse(cleaned) shouldBe md

      md.sub should have size 3
      md.sub.head shouldBe Paragraph(
        "outside\n[refout]: https://www.refout.com"
      )
      md.sub(1) shouldBe a[Header]
      md.sub(2) shouldBe a[Header]
      val h1 = md.sub(1).asInstanceOf[Header]
      val h2 = md.sub(2).asInstanceOf[Header]

      h1.sub should have size 3
      h1.sub.head shouldBe Paragraph("h1txt")
      h1.sub(1) shouldBe a[Header]
      h1.sub(2) shouldBe a[Header]
      h1.sub(1) shouldBe Header(
        2,
        "header1a",
        Paragraph("h1atxt\n[ref1a]: https://www.ref1a.com")
      )
      h1.sub(2) shouldBe Header(
        2,
        "header1b",
        Paragraph("h1btxt"),
        Header(3, "header1b1", Paragraph("h1b1txt"))
      )

      h2.sub should have size 3
      h2.sub.head shouldBe Paragraph("h2txt\n[ref2]: https://www.ref2.com")
      h2.sub(1) shouldBe a[Header]
      h2.sub(2) shouldBe a[Header]
      h2.sub(1) shouldBe Header(2, "header2a", Paragraph("h2atxt"))
      h2.sub(2) shouldBe Header(
        2,
        "header2b",
        Paragraph("h2btxt\n[ref2b]: https://www.ref2b.com")
      )
    }
  }
}
