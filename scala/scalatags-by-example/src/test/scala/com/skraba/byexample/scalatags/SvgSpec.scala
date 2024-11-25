package com.skraba.byexample.scalatags

import com.skraba.byexample.scalatags.Svg.Attrs
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import scalatags.Text.implicits._
import scalatags.Text.svgTags.{g, svg}

import scala.reflect.io._

/** Unit tests for the helpers in the [[Svg]].
  */
class SvgSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterAll {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  describe("Writing to a file") {

    it("should write an svg tag unwrapped") {
      val svgContents = svg(Attrs.width := 100)
      val dst = Tmp / File("svg_toFile_unwrapped.svg")
      Svg.toFile(dst, svgContents, 99, 99);
      dst.jfile should exist
      val dstContents = dst.safeSlurp()
      dstContents.value shouldBe ("""<?xml version="1.0"?><svg width="100"></svg>""")
    }

    it("should wrap a group tag") {
      val svgContents = g(Attrs.width := 100)
      val dst = Tmp / File("svg_toFile_wrapped.svg")
      Svg.toFile(dst, svgContents, 99, 99);
      val dstContents = dst.safeSlurp()
      dstContents.value shouldBe
        """<?xml version="1.0"?>
          |<svg width="99" height="99" viewBox="0 0 99 99" xmlns="https://www.w3.org/2000/svg">
          |<g width="100"></g></svg>""".stripMargin.replaceAll("\n", "")
    }
  }

  describe("Writing inkscape layers") {
    it("should add the appropriate tags") {
      Svg
        .inkscapeLayer(1, "Background")(g(Attrs.id := "SvgSpec"))
        .render shouldBe
        """<g id="layer1" inkscape:groupmode="layer" inkscape:label="Background">
          |<g id="SvgSpec">
          |</g></g>""".stripMargin.replaceAll("\n", "")
    }
  }

  describe("Creating text tags") {

    it("should have reasonable defaults") {
      val default = Svg.Text()
      val tag = default.middle(0, 0, 10, 10)("Hello")
      tag.render shouldBe
        """<text fill="#000000"
          | font-family="San Serif"
          | font-weight="normal"
          | font-size="8.0"
          | x="5.0"
          | y="5.8"
          | text-anchor="middle"
          | dominant-baseline="middle">
          |Hello
          |</text>""".stripMargin
          .replaceAll("\n", "")
    }

    it("should be reasonably customizable") {
      val customized = Svg.Text(
        fill = "FFFFFF",
        family = "Serif",
        weight = "bold",
        size = 6,
        adjust = 0.11
      )
      val tag = customized.middle(0, 0, 10, 10)("Hello")
      tag.render shouldBe
        """<text fill="#FFFFFF"
          | font-family="Serif"
          | font-weight="bold"
          | font-size="6.0"
          | x="5.0"
          | y="5.66"
          | text-anchor="middle"
          | dominant-baseline="middle">
          |Hello
          |</text>""".stripMargin
          .replaceAll("\n", "")
    }

    it("should be alignable") {
      Svg.Text().left(10, 10)("LEFT").render shouldBe
        """<text fill="#000000"
          | font-family="San Serif"
          | font-weight="normal"
          | font-size="8.0"
          | x="10.0"
          | y="10.0"
          | text-anchor="start">
          |LEFT
          |</text>""".stripMargin
          .replaceAll("\n", "")
      Svg.Text().left(0, 0, 100, 100)("LEFT_IN_BOX").render shouldBe
        """<text fill="#000000"
          | font-family="San Serif"
          | font-weight="normal"
          | font-size="8.0"
          | x="0.0" y="50.8" text-anchor="start" dominant-baseline="middle">
          |LEFT_IN_BOX
          |</text>""".stripMargin
          .replaceAll("\n", "")
      Svg.Text().center(10, 10)("CENTER").render shouldBe
        """<text fill="#000000"
          | font-family="San Serif"
          | font-weight="normal"
          | font-size="8.0"
          | x="10.0"
          | y="10.0"
          | text-anchor="middle">
          |CENTER
          |</text>""".stripMargin
          .replaceAll("\n", "")
      Svg.Text().right(10, 10)("RIGHT").render shouldBe
        """<text fill="#000000"
          | font-family="San Serif"
          | font-weight="normal"
          | font-size="8.0"
          | x="10.0"
          | y="10.0"
          | text-anchor="end">
          |RIGHT
          |</text>""".stripMargin
          .replaceAll("\n", "")
      Svg.Text().right(0, 0, 100, 100)("RIGHT_IN_BOX").render shouldBe
        """<text fill="#000000"
          | font-family="San Serif"
          | font-weight="normal"
          | font-size="8.0"
          | x="0.0" y="50.8" text-anchor="end" dominant-baseline="middle">
          |RIGHT_IN_BOX
          |</text>""".stripMargin
          .replaceAll("\n", "")
    }
  }
}
