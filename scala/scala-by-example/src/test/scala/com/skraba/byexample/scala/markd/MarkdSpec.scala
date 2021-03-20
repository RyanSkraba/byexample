package com.skraba.byexample.scala.markd

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Unit tests for [[Markd]]
  */
class MarkdSpec extends AnyFunSpecLike with Matchers {

  describe("Parsing a paragraph") {

    it("should be empty if there aren't any contents") {
      val md = Header.parse("     \t\n\n")
      md shouldBe Header(0, "")

      val cleaned = md.build().toString
      cleaned shouldBe ""
      Header.parse(cleaned) shouldBe md
    }

    it("should parse different linkrefs") {
      val md = Header.parse("""
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
          |""".stripMargin.replaceAllLiterally(".", " "))

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
      val md = Header.parse("""English
          |===
          |Hello world
          |# French
          |Bonjour tout le monde""".stripMargin)
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
      val md = Header.parse("""
          |### Three
          |## Two
          |# One
          |## Two
          |### Three
          |""".stripMargin)
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
      val md = Header.parse("""
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
          |""".stripMargin)

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

  describe("Replacing subelements") {
    val md = Header.parse("""
                            |# One
                            |# Two
                            |# Three
                            |""".stripMargin)

    describe("replacing a match one-to-one with another element") {
      it("should replace all matches") {
        md.replaceInSub() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "THREE")
        )
      }
      it("should replace all matches with filtering") {
        md.replaceInSub(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(Header(1, "TWO"), Header(1, "THREE"))
      }
      it("should replace the first") {
        md.replaceFirstInSub() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
      }
    }

    describe("removing a match") {
      it("should replace all matches") {
        md.replaceInSub() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq.empty
        }.sub shouldBe Seq(Header(1, "One"))
      }
      it("should replace all matches with filtering") {
        md.replaceInSub(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq.empty
        }.sub shouldBe Seq.empty
      }
      it("should replace the first") {
        md.replaceFirstInSub() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq.empty
        }.sub shouldBe Seq(Header(1, "One"), Header(1, "Three"))
      }
    }

    describe("inserting after a match") {
      it("should replace all matches") {
        md.replaceInSub() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three"),
          Header(1, "THREE")
        )
      }
      it("should replace all matches with filtering") {
        md.replaceInSub(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three"),
          Header(1, "THREE")
        )
      }
      it("should replace the first") {
        md.replaceFirstInSub() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
      }
    }

    describe("when a match isn't found") {
      it("should do nothing on all matches") {
        md.replaceInSub() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe md.sub
      }
      it("should remove all when filtering") {
        md.replaceInSub(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq.empty
      }
      it("should do nothing when no first match") {
        md.replaceFirstInSub() {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe md.sub
      }
      it("should help falling back when no first match") {
        md.replaceFirstInSub(ifNotFound = md.sub :+ Header(1, "Four")) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.sub shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
      }
    }

    describe("when matching on None to append") {
      it("should append on all matches") {
        md.replaceInSub() { case (None, _) =>
          Seq(Header(1, "Four"))
        }.sub shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "Four")
        )
      }
      it("should remove all but the element when filtering") {
        md.replaceInSub(filter = true) { case (None, _) =>
          Seq(Header(1, "Four"))
        }.sub shouldBe Seq(Header(1, "Four"))
      }
    }

    describe("when matching on 0 to prepend") {
      it("should prepend on all matches") {
        md.replaceInSub() { case (Some(md), 0) =>
          Seq(Header(1, "Zero"), md)
        }.sub shouldBe Seq(
          Header(1, "Zero"),
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three")
        )
      }
      it("should remove all but the element and the head when filtering") {
        md.replaceInSub(filter = true) { case (Some(md), 0) =>
          Seq(Header(1, "Zero"), md)
        }.sub shouldBe Seq(Header(1, "Zero"), Header(1, "One"))
      }
    }
  }
}
