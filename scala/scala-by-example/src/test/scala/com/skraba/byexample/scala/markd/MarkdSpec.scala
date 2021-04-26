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

    for (
      (tag, content) <- Seq(
        "simple" -> "Hello world",
        "simplePreWhitespace" -> "\n\n\t  Hello world",
        "simplePostWhitespace" -> "Hello world\n\t  \n\n",
        "internalNewline" -> "Hello\nworld",
        "internalNewlinePreWhitespace" -> "\n\n\t  Hello\nworld",
        "internalNewlinePostWhitespace" -> "Hello\nworld\n\t  \n\n",
        "internalWhitespace" -> "Hello     \n     world",
        "internalWhitespacePreWhitespace" -> "\n\n\t  Hello     \n     world",
        "internalWhitespacePostWhitespace" -> "Hello     \n     world\n\t  \n\n"
      )
    ) {
      describe(s"for the $tag paragraph") {
        it("creates a simple paragraph in no section") {
          val md = Header.parse(content)
          md shouldBe Header(0, "", Paragraph(content.trim))

          val cleaned = md.build().toString
          cleaned shouldBe s"${content.trim}\n"
          Header.parse(cleaned) shouldBe md
        }

        it("creates a simple paragraph in a section") {
          val md = Header.parse(s"# Main\n$content")
          md shouldBe Header(0, "", Header(1, "Main", Paragraph(content.trim)))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Main
              |==============================================================================
              |
              |${content.trim}
              |""".stripMargin
          Header.parse(cleaned) shouldBe md
        }
      }
    }

    for (
      (tag, content) <- Seq(
        "simple" -> "Hello\n\nworld",
        "simplePreWhitespace" -> "\n\n\t  Hello\n\nworld",
        "simplePostWhitespace" -> "Hello\n\nworld\n\t  \n\n",
        "internalPreWhitespace" -> "Hello  \t  \n\nworld",
        "internalMidWhitespace" -> "Hello\n  \t  \nworld",
        "internalPostWhitespace" -> "Hello\n\n  \t  world",
        "internalLotsWhitespace" -> "Hello\n\n\n\n\n\nworld"
      )
    ) {
      describe(s"for the $tag paragraphs") {
        it("creates paragraphs in no sections") {
          val md = Header.parse(content)
          md shouldBe Header(0, "", Paragraph("Hello"), Paragraph("world"))

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Hello
               |
               |world
               |""".stripMargin
          Header.parse(cleaned) shouldBe md
        }

        it("creates paragraphs in a section") {
          val md = Header.parse(s"# Main\n$content")
          md shouldBe Header(
            0,
            "",
            Header(1, "Main", Paragraph("Hello"), Paragraph("world"))
          )

          val cleaned = md.build().toString
          cleaned shouldBe
            s"""Main
               |==============================================================================
               |
               |Hello
               |
               |world
               |""".stripMargin
          Header.parse(cleaned) shouldBe md
        }
      }
    }

    describe("with a code block") {
      it("should find a standalone element") {
        val md = Header.parse("""
            |```bash
            |echo Hello world
            |```
        """.stripMargin)
        md shouldBe Header(0, "", Code("bash", "echo Hello world\n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |echo Hello world
            |```
            |""".stripMargin
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore unnecessary whitespace") {
        val md = Header.parse("""
            |```bash.....
            |.....echo Hello world.....
            |```.....
        """.stripMargin.replaceAllLiterally(".", " "))
        md shouldBe Header(0, "", Code("bash", "     echo Hello world     \n"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """```bash
            |.....echo Hello world.....
            |```
            |""".stripMargin.replaceAllLiterally(".", " ")
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore a code block with bad whitespace") {
        val md = Header.parse("""
                                |   ```bash
                                |echo Hello world
                                |```
        """.stripMargin.replaceAllLiterally(".", " "))
        // TODO: What do we expect here?  This is probably not what we want and cleaning breaks.
        // md shouldBe Header(0, "", Paragraph("   ```bash\necho Hello world\n```"))

        val cleaned = md.build().toString
        cleaned shouldBe
          """``bash
            |echo Hello world
            |
            |``
            |""".stripMargin.replaceAllLiterally(".", " ")
        Header.parse(cleaned) shouldBe md
      }
    }

    describe("with a comment block") {
      it("should find a standalone element") {
        val md = Header.parse("<!-- Hello world -->")
        md shouldBe Header(0, "", Comment(" Hello world "))

        val cleaned = md.build().toString
        cleaned shouldBe "<!-- Hello world -->\n"
        Header.parse(cleaned) shouldBe md
      }

      it("should ignore unnecessary whitespace") {
        val md = Header.parse("\n    \t<!-- Hello\n\tworld -->\n    \t")
        md shouldBe Header(0, "", Comment(" Hello\n\tworld "))

        val cleaned = md.build().toString
        cleaned shouldBe "<!-- Hello\n\tworld -->\n"
        Header.parse(cleaned) shouldBe md
      }

      it("should separate it carefully from other elements") {
        val md = Header.parse("Hello\t<!-- Hello\n\tworld -->     world")
        md shouldBe Header(
          0,
          "",
          Paragraph("Hello"),
          Comment(" Hello\n\tworld "),
          Paragraph("world")
        )

        val cleaned = md.build().toString
        cleaned shouldBe
          """Hello
            |
            |<!-- Hello
            |.world -->
            |
            |world
            |""".stripMargin.replaceAllLiterally(".", "\t")
        Header.parse(cleaned) shouldBe md
      }
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

      md.mds should have size 15
      md.mds.head shouldBe LinkRef("ref-bare", None, None)
      md.mds(1) shouldBe LinkRef("url", "url")
      md.mds(2) shouldBe LinkRef("url-prews", "url-prews")
      md.mds(3) shouldBe LinkRef("url-postws", "url-postws")
      md.mds(4) shouldBe LinkRef("title", None, Some("title"))
      md.mds(5) shouldBe LinkRef("title-prews", None, Some("title-prews"))
      md.mds(6) shouldBe LinkRef("title-postws", None, Some("title-postws"))
      md.mds(7) shouldBe LinkRef("title-empty", None, None)
      md.mds(8) shouldBe LinkRef("title-empty-prews", None, None)
      md.mds(9) shouldBe LinkRef("title-empty-postws", None, None)
      md.mds(10) shouldBe LinkRef("all", "all", "all")
      md.mds(11) shouldBe LinkRef("all-prews", "all-prews", "all-prews")
      md.mds(12) shouldBe LinkRef("all-midws", "all-midws", "all-midws")
      md.mds(13) shouldBe LinkRef("all-postws", "all-postws", "all-postws")
      md.mds(14) shouldBe LinkRef("all-empty-title", "all-empty-title")
    }
  }

  it("should ignore non-linkref") {
    val md = Header.parse("""
                            |[url]: url
                            | [space-before]: Leading space?  Not a link ref
                            |""".stripMargin.replaceAllLiterally(".", " "))

    val cleaned = md.build().toString
    cleaned shouldBe """[space-before]: Leading space?  Not a link ref
                       |
                       |[url]: url
                       |""".stripMargin
    // TODO: The round-trip is still broken because of cleaning up whitespace.
    // Header.parse(cleaned) shouldBe md

    md.mds should have size 2
    md.mds.head shouldBe Paragraph(
      "[space-before]: Leading space?  Not a link ref"
    )
    md.mds(1) shouldBe LinkRef("url", "url")
  }

  describe("Parsing markdown into sections") {

    it("should separate into level 1 headers") {
      val md = Header.parse("""English
          |===
          |Hello world
          |# French
          |Bonjour tout le monde""".stripMargin)
      md.mds should have size 2

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
      md.mds should have size 3

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

      md.mds should have size 4
      md.mds.head shouldBe Paragraph("outside")
      md.mds(1) shouldBe LinkRef("refout", "https://www.refout.com")
      md.mds(2) shouldBe a[Header]
      md.mds(3) shouldBe a[Header]
      val h1 = md.mds(2).asInstanceOf[Header]
      val h2 = md.mds(3).asInstanceOf[Header]

      h1.mds should have size 3
      h1.mds.head shouldBe Paragraph("h1txt")
      h1.mds(1) shouldBe a[Header]
      h1.mds(2) shouldBe a[Header]
      h1.mds(1) shouldBe Header(
        2,
        "header1a",
        Paragraph("h1atxt"),
        LinkRef("ref1a", "https://www.ref1a.com"),
        LinkRef("ref1a_dup", "https://www.ref1a.com")
      )
      h1.mds(2) shouldBe Header(
        2,
        "header1b",
        Paragraph("h1btxt"),
        Header(3, "header1b1", Paragraph("h1b1txt"))
      )

      h2.mds should have size 4
      h2.mds.head shouldBe Paragraph("h2txt")
      h2.mds(1) shouldBe LinkRef("ref2", "https://www.ref2.com")
      h2.mds(2) shouldBe Header(2, "header2a", Paragraph("h2atxt"))
      h2.mds(3) shouldBe Header(
        2,
        "header2b",
        Paragraph("h2btxt"),
        LinkRef("ref2b", "https://www.ref2b.com")
      )
    }
  }

  describe("Parsing markdown into tables") {
    it("should clean up a simple table") {
      val md = Header.parse("""Before
          |
          |Id        | Name
          |---    | ---
          |   [1](https://en.wikipedia.org/wiki/1)    |      One
          |2|Two
          |3|Three
          |
          |
          |After
          |""".stripMargin)
      // TODO
      md.mds should have size 3
      val cleaned = md.build().toString
//      cleaned shouldBe
//        """Before
//          |
//          |Id  | Name
//          |--- | ---
//          |1   | One
//          |2   | Two
//          |
//          |After
//          |""".stripMargin
      Header.parse(cleaned) shouldBe md
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
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "THREE")
        )
      }
      it("should replace all matches with filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(Header(1, "TWO"), Header(1, "THREE"))
      }
      it("should replace the first") {
        md.replaceFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
      }
    }

    describe("removing a match") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq.empty
        }.mds shouldBe Seq(Header(1, "One"))
      }
      it("should replace all matches with filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq.empty
        }.mds shouldBe Seq.empty
      }
      it("should replace the first") {
        md.replaceFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq.empty
        }.mds shouldBe Seq(Header(1, "One"), Header(1, "Three"))
      }
    }

    describe("inserting after a match") {
      it("should replace all matches") {
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three"),
          Header(1, "THREE")
        )
      }
      it("should replace all matches with filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three"),
          Header(1, "THREE")
        )
      }
      it("should replace the first") {
        md.replaceFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("T") =>
            Seq(h, h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "TWO"),
          Header(1, "Three")
        )
      }
    }

    describe("when a match isn't found") {
      it("should do nothing on all matches") {
        md.replaceIn() {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe md.mds
      }
      it("should remove all when filtering") {
        md.replaceIn(filter = true) {
          case (Some(h @ Header(title, 1, _)), _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq.empty
      }
      it("should do nothing when no first match") {
        md.replaceFirstIn() {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe md.mds
      }
      it("should help falling back when no first match") {
        md.replaceFirstIn(ifNotFound = md.mds :+ Header(1, "Four")) {
          case h @ Header(title, 1, _) if title.startsWith("F") =>
            Seq(h.copy(title = h.title.toUpperCase))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "FOUR")
        )
      }
    }

    describe("when matching on None to append") {
      it("should append on all matches") {
        md.replaceIn() { case (None, _) =>
          Seq(Header(1, "Four"))
        }.mds shouldBe Seq(
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three"),
          Header(1, "Four")
        )
      }
      it("should remove all but the element when filtering") {
        md.replaceIn(filter = true) { case (None, _) =>
          Seq(Header(1, "Four"))
        }.mds shouldBe Seq(Header(1, "Four"))
      }
    }

    describe("when matching on 0 to prepend") {
      it("should prepend on all matches") {
        md.replaceIn() { case (Some(md), 0) =>
          Seq(Header(1, "Zero"), md)
        }.mds shouldBe Seq(
          Header(1, "Zero"),
          Header(1, "One"),
          Header(1, "Two"),
          Header(1, "Three")
        )
      }
      it("should remove all but the element and the head when filtering") {
        md.replaceIn(filter = true) { case (Some(md), 0) =>
          Seq(Header(1, "Zero"), md)
        }.mds shouldBe Seq(Header(1, "Zero"), Header(1, "One"))
      }
    }
  }
}
