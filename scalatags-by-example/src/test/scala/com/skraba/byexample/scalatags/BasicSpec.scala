package com.skraba.byexample.scalatags

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterEach, FunSpecLike, Matchers}
import scalatags.generic
import scalatags.text.Builder

@RunWith(classOf[JUnitRunner])
class BasicSpec extends FunSpecLike with Matchers with BeforeAndAfterEach {

  describe("Creating HTML documents") {

    // All known tags to scala tag.
    import scalatags.Text.all._

    it("should be awesome") {

      val htmlTag = html(
        head(script("this is a script")),
        body(
          h1("Heading"),
          div(
            p("text text")
          )))

      "<!DOCTYPE html>" + htmlTag.render shouldBe
        """<!DOCTYPE html>
          |<html><head><script>this is a script</script></head>
          |<body><h1>Heading</h1><div><p>text text</p></div></body>
          |</html>""".stripMargin.replaceAll("\n", "")
    }

    it("should add attributes") {
      val h1Tag = h1(cls := "blue", id := "colourful")("Colourful")
      h1Tag.render shouldBe
        """<h1 class="blue" id="colourful">
          |Colourful
          |</h1>""".stripMargin.replaceAll("\n", "")
    }

    it("should collect style attributes") {
      val h1Tag = h1(backgroundColor := "blue", color := "red")("Colourful")
      h1Tag.render shouldBe
        """<h1 style="background-color: blue; color: red;">
          |Colourful
          |</h1>""".stripMargin.replaceAll("\n", "")
    }

    it("should support style enums") {
      val h1Tag = h1(float.left)("Colourful")
      h1Tag.render shouldBe
        """<h1 style="float: left;">
          |Colourful
          |</h1>""".stripMargin.replaceAll("\n", "")
    }

    it("should support tag lists or modifiers") {
      val tags1 = Seq(p("one"), p("two"))
      val tags2 = frag(p("three"), p("four"))
      val attr1 = Seq(id := "one", cls := "two")
      val attr2 = modifier(id := "three", cls := "four")
      h1(tags1).render shouldBe "<h1><p>one</p><p>two</p></h1>"
      h1(tags2).render shouldBe "<h1><p>three</p><p>four</p></h1>"
      h1(attr1).render shouldBe """<h1 id="one" class="two"></h1>"""
      h1(attr2).render shouldBe """<h1 id="three" class="four"></h1>"""
    }

    it("should sanitize text") {
      val xxx = "<hello/>"
      // Sanitized by default.
      val h1Tag = h1(id := xxx)(xxx)
      h1Tag.render shouldBe """<h1 id="&lt;hello/&gt;">&lt;hello/&gt;</h1>"""
      // But can be shown raw if desired.
      val h1RawTag = h1(raw(xxx))
      h1RawTag.render shouldBe """<h1><hello/></h1>"""
    }

    it("should support CSS types") {
      val divTag = div(
        p(color := "red", fontSize := 64.pt)("Hello"),
        p(color := "blue", fontSize := 64.em)("world"),
      )
      divTag.render shouldBe
        """<div>
          |<p style="color: red; font-size: 64pt;">Hello</p>
          |<p style="color: blue; font-size: 64em;">world</p>
          |</div>""".stripMargin.replaceAll("\n", "")
    }
  }

  describe("Creating generic documents") {

    // All known tags to scala tag.
    import scalatags.Text.implicits._
    import scalatags.Text.tags._

    it("should be generic.") {
      // Note that children and attributes are added in order via .apply().
      // The attribute is applied to root, not child, as are the two contents.
      val rootTag = tag("root")(
        tag("child"))(attr("id") := "my-root-id")("One")("Two")
      rootTag.render shouldBe """<root id="my-root-id"><child></child>OneTwo</root>"""
    }

    it("should permit flow control.") {
      val condition = true
      val rootTag = tag("root")(
        tag("in")("one"),
        tag("in")("two"),
        if (condition) "true" else "false",
        "three",
        for (i <- 1 to 3) yield tag("count")(i)
      )
      rootTag.render shouldBe
        """<root>
          |<in>one</in>
          |<in>two</in>
          |true
          |three
          |<count>1</count>
          |<count>2</count>
          |<count>3</count>
          |</root>""".stripMargin.replaceAll("\n", "")
    }

    it("should allow fragments") {
      val letters = frag(tag("a"), tag("b"))
      tag("root")(letters).render shouldBe """<root><a></a><b></b></root>"""
      val moreLetters = frag(tag("z"), letters, tag("c"))
      tag("root")(moreLetters).render shouldBe """<root><z></z><a></a><b></b><c></c></root>"""
    }
  }

  describe("Creating an SVG document") {

    import scalatags.Text.implicits._
    import scalatags.Text.svgAttrs._
    import scalatags.Text.svgTags._

    it("should be awesome") {

      val svg1 = svg(height := 100, width := 100)

      // Use apply to add an element.
      val svg2 = svg1(line(
        x1 := 0, y1 := 0,
        x2 := 100, y2 := 100))
      svg2.render shouldBe
        """<svg height="100" width="100">
          |<line x1="0" y1="0" x2="100" y2="100"></line>
          |</svg>""".stripMargin.replaceAll("\n", "")

      val svg3 = svg2(path(d := "M0 100 L100 0 L100 100 Z"))
      svg3.render shouldBe
        """<svg height="100" width="100">
          |<line x1="0" y1="0" x2="100" y2="100"></line>
          |<path d="M0 100 L100 0 L100 100 Z"></path>
          |</svg>""".stripMargin.replaceAll("\n", "")
    }

    it("should be styleable") {

      val svg1 = svg()
      val svgStyled = svg1(scalatags.Text.svgTags.tag("style")(
        raw(
          """<![CDATA[
            |  #my-rect { fill: blue; }
            |]]>""".stripMargin.replaceAll("\n\\s*", " "))))

      val svgWithStyledRect = svgStyled(rect(id := "my-rect", x := 0, y := 0, width := 10, height := 10))
      svgWithStyledRect.render shouldBe
        """<svg>
          |<style>
          |<![CDATA[ #my-rect { fill: blue; } ]]>
          |</style>
          |<rect id="my-rect" x="0" y="0" width="10" height="10"></rect>
          |</svg>""".stripMargin.replaceAll("\n", "")
    }
  }
}
