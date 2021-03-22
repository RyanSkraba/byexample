package com.skraba.byexample.scala.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Markd]]
  */
class MarkdSpec extends AnyFunSpecLike with Matchers {

  describe("Parsing markdown into sections") {

    it("should just wrap in a Paragraph if there are none") {
      val md = Header.parse("Hello world")
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

    it("should nicely nest sections ") {
      val contents = """
          |### Three
          |## Two
          |# One
          |## Two
          |### Three
          |""".stripMargin

      val md = Header.parse(contents)
      md.sub should have size 3

      val cleaned = md.build().toString
      cleaned shouldBe
        """### Three
          |
          |Two
          |------------------------------------------------------------------------------
          |
          |One
          |==============================================================================
          |
          |Two
          |------------------------------------------------------------------------------
          |
          |### Three
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
          |[ref1a]: https://www.ref1a.com
          |h1atxt
          |[ref1a_dup]: https://www.ref1a.com
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
          |
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
          |
          |[ref1a]: https://www.ref1a.com
          |[ref1a_dup]: https://www.ref1a.com
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
          |
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
          |
          |[ref2b]: https://www.ref2b.com
          |""".stripMargin
      Header.parse(cleaned) shouldBe md

      md.sub should have size 4
      md.sub.head shouldBe Paragraph("outside")
      md.sub(1) shouldBe LinkRef("refout", "https://www.refout.com")
      md.sub(2) shouldBe a[Header]
      md.sub(3) shouldBe a[Header]
      val h1 = md.sub(2).asInstanceOf[Header]
      val h2 = md.sub(3).asInstanceOf[Header]

      h1.sub should have size 3
      h1.sub.head shouldBe Paragraph("h1txt")
      h1.sub(1) shouldBe a[Header]
      h1.sub(2) shouldBe a[Header]
      h1.sub(1) shouldBe Header(
        2,
        "header1a",
        Paragraph("h1atxt"),
        LinkRef("ref1a", "https://www.ref1a.com"),
        LinkRef("ref1a_dup", "https://www.ref1a.com")
      )
      h1.sub(2) shouldBe Header(
        2,
        "header1b",
        Paragraph("h1btxt"),
        Header(3, "header1b1", Paragraph("h1b1txt"))
      )

      h2.sub should have size 4
      h2.sub.head shouldBe Paragraph("h2txt")
      h2.sub(1) shouldBe LinkRef("ref2", "https://www.ref2.com")
      h2.sub(2) shouldBe Header(2, "header2a", Paragraph("h2atxt"))
      h2.sub(3) shouldBe Header(
        2,
        "header2b",
        Paragraph("h2btxt"),
        LinkRef("ref2b", "https://www.ref2b.com")
      )
    }
  }

  describe("Parsing a paragraph") {

    it("should be empty if there aren't any contents") {
      val contents = "     \t\n\n"

      val md = Header.parse(contents)
      md shouldBe Header(0, "")

      val cleaned = md.build().toString
      cleaned shouldBe ""
      Header.parse(cleaned) shouldBe md
    }

    it("should parse different linkrefs") {
      val contents = """
          |[ref-bare]:
          |[url]: url
          |[url-prews]:.....url-prews
          |[url-postws]:url-postws.....
          |[title]: "title"
          |[title-prews]:....."title-prews"
          |[title-postws]:"title-postws".....
          |[title-empty]:""
          |[title-empty-prews]:.....""
          |[title-empty-postws]:"".....
          |[all]: all "all"
          |[all-prews]:.....all-prews "all-prews"
          |[all-midws]:all-midws....."all-midws"
          |[all-postws]:all-postws."all-postws".....
          |[all-empty-title]:all-empty-title.""
          |""".stripMargin.replaceAllLiterally(".", " ")

      val md = Header.parse(contents)

      val cleaned = md.build().toString
      cleaned shouldBe """[ref-bare]:
          |[url]: url
          |[url-prews]: url-prews
          |[url-postws]: url-postws
          |[title]: "title"
          |[title-prews]: "title-prews"
          |[title-postws]: "title-postws"
          |[title-empty]:
          |[title-empty-prews]:
          |[title-empty-postws]:
          |[all]: all "all"
          |[all-prews]: all-prews "all-prews"
          |[all-midws]: all-midws "all-midws"
          |[all-postws]: all-postws "all-postws"
          |[all-empty-title]: all-empty-title
          |""".stripMargin
      Header.parse(cleaned) shouldBe md

      md.sub should have size 15
      md.sub.head shouldBe LinkRef("ref-bare", None, None)
      md.sub(1) shouldBe LinkRef("url", "url")
      md.sub(2) shouldBe LinkRef("url-prews", "url-prews")
      md.sub(3) shouldBe LinkRef("url-postws", "url-postws")
      md.sub(4) shouldBe LinkRef("title", None, Some("title"))
      md.sub(5) shouldBe LinkRef("title-prews", None, Some("title-prews"))
      md.sub(6) shouldBe LinkRef("title-postws", None, Some("title-postws"))
      md.sub(7) shouldBe LinkRef("title-empty", None, None)
      md.sub(8) shouldBe LinkRef("title-empty-prews", None, None)
      md.sub(9) shouldBe LinkRef("title-empty-postws", None, None)
      md.sub(10) shouldBe LinkRef("all", "all", "all")
      md.sub(11) shouldBe LinkRef("all-prews", "all-prews", "all-prews")
      md.sub(12) shouldBe LinkRef("all-midws", "all-midws", "all-midws")
      md.sub(13) shouldBe LinkRef("all-postws", "all-postws", "all-postws")
      md.sub(14) shouldBe LinkRef("all-empty-title", "all-empty-title")
    }
  }
}
