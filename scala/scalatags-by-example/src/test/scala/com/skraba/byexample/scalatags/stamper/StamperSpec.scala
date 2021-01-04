package com.skraba.byexample.scalatags.stamper

import com.skraba.byexample.scalatags.Svg
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import scalatags.Text.implicits._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._

import scala.reflect.io.File

class StamperSpec extends AnyFunSpecLike with Matchers {

  describe("Using a Stamper") {

    it("draws one element when moved") {
      val origin = Stamper(tag = rect)
      val east = origin.e()

      east.x shouldBe 1.0
      east.y shouldBe 0.0

      east.history should have size 1
      east.history.head shouldBe rect(x := 0.0, y := 0.0)

      g(
        east.history: _*
      ).render shouldBe """<g><rect x="0.0" y="0.0"></rect></g>"""
    }

    it("draws two elements when moved") {
      val origin = Stamper(tag = rect)
      val east = origin.e().e()

      east.x shouldBe 2.0
      east.y shouldBe 0.0

      east.history should have size 2
      east.history.head shouldBe rect(x := 1.0, y := 0.0)

      g(
        east.history: _*
      ).render shouldBe """<g><rect x="1.0" y="0.0"></rect><rect x="0.0" y="0.0"></rect></g>"""
    }

    it("draws no elements when the pen is up") {
      val origin = Stamper(tag = rect, penDown = false)
      val east = origin.e().e().e().e().e()

      east.x shouldBe 5.0
      east.y shouldBe 0.0

      east.history should have size (0)
    }

    it("retains a checkpoint") {
      val stamp =
        Stamper(tag = rect).se().save("1").se().e().se().se().save("2").se()

      stamp.checkpoints("1") shouldBe (1.0, 1.0)
      stamp.checkpoints("2") shouldBe (5.0, 4.0)
      stamp.history should have size 6
      stamp.history.head shouldBe rect(x := 5.0, y := 4.0)
    }

    it("should stamp in all redirections") {
      val origin = Stamper(tag = rect)
      origin.n().stamp().history.head shouldBe rect(x := 0.0, y := -1.0)
      origin.s().stamp().history.head shouldBe rect(x := 0.0, y := 1.0)
      origin.e().stamp().history.head shouldBe rect(x := 1.0, y := 0.0)
      origin.w().stamp().history.head shouldBe rect(x := -1.0, y := 0.0)
      origin.ne().stamp().history.head shouldBe rect(x := 1.0, y := -1.0)
      origin.nw().stamp().history.head shouldBe rect(x := -1.0, y := -1.0)
      origin.se().stamp().history.head shouldBe rect(x := 1.0, y := 1.0)
      origin.sw().stamp().history.head shouldBe rect(x := -1.0, y := 1.0)
    }

    it("should stamp in all redirections with a custom distance and position") {
      val origin = Stamper(x = 1, y = 2, stepX = 3, stepY = 4, tag = rect)
      origin.n().stamp().history.head shouldBe rect(x := 1.0, y := -2.0)
      origin.s().stamp().history.head shouldBe rect(x := 1.0, y := 6.0)
      origin.e().stamp().history.head shouldBe rect(x := 4.0, y := 2.0)
      origin.w().stamp().history.head shouldBe rect(x := -2.0, y := 2.0)
      origin.ne().stamp().history.head shouldBe rect(x := 4.0, y := -2.0)
      origin.nw().stamp().history.head shouldBe rect(x := -2.0, y := -2.0)
      origin.se().stamp().history.head shouldBe rect(x := 4.0, y := 6.0)
      origin.sw().stamp().history.head shouldBe rect(x := -2.0, y := 6.0)
    }

    it("can clone itself into 4") {
      val origin =
        Stamper(x = 1, y = 2, stepX = 3, stepY = 4, tag = rect).stamp()
      origin.history should have size 1

      val cloned = origin.clone4("x")
      cloned.stepX shouldBe 6.0
      cloned.stepY shouldBe 8.0

      // A southeast clone uses the history internally twice.
      g(cloned.history: _*).render shouldBe
        """<g>
          |<g id="x_2">
          |<g id="x"><rect x="1.0" y="2.0"></rect>
          |</g>
          |<use xlink:href="#x" transform="translate(0.0,4.0)"></use>
          |</g>
          |<use xlink:href="#x_2" transform="translate(3.0,0.0)"></use>
          |</g>""".stripMargin.replaceAll("\n", "")
    }
  }

  describe("Using a Stamper with examples") {

    val base = rect(width := 1, height := 1)

    it("should create a couple of paths") {
      val lightGray = base(fill := "#EEE")
      val darkGray = base(fill := "#888")
      val pink = base(fill := "#F88")
      val door = base(fill := "#E8E8E8")

      // Draw a path in a circle.
      val circle = Stamper(tag = darkGray)
        .w()
        .w()
        .w()
        .w()
        .sw()
        .w()
        .sw()
        .sw()
        .s()
        .sw()
        .s()
        .s()
        .s()
        .s()
        .save("eastWall") // A remember a point in the path.
        .se()
        .s()
        .se()
        .se()
        .e()
        .se()
        .e()
        .e()
        .e()
        .e()
        .ne()
        .e()
        .ne()
        .ne()
        .n()
        .ne()
        .n()
        .n()
        .n()
        .n()
        .nw()
        .n()
        .nw()
        .nw()
        .w()
        .stamp()

      // Get the position at the checkpoint.
      val eastWall = circle.checkpoints.getOrElse("eastWall", (0.0, 0.0))

      // And create a new Stamper starting at the point.
      val extra =
        Stamper(x = eastWall._1 - 1, y = eastWall._2, tag = lightGray)
          .w()
          .sw()
          .s(door)
          .s(lightGray)
          .se()
          .sw(pink)
          .s(door)
          .s(pink)
          .se()
          .e(door)
          .e(pink)
          .ne(lightGray)
          .se()
          .e(door)
          .e(lightGray)
          .se(pink)
          .se()
          .e(door)
          .e(pink)
          .ne()
          .ne(lightGray)
          .e(door)
          .e(lightGray)
          .ne()
          .n()
          .n()
          .n()
          .stamp()

      // Draw the SVG diagram.
      val plan =
        g(circle.history: _*)(extra.history: _*)(transform := "translate(12,0)")

      Svg.toFile(File("/tmp/plan.svg"), plan)
    }

    it("should draw circles on a grid") {

      val lightGray = base(fill := "#E8E8E8")
      val darkGray = base(fill := "#888")
      val pink = base(fill := "#FDD")

      // A 128 x 128 checkered background
      val background = g(
        Stamper(tag = lightGray)
          .se()
          .stamp()
          .copy(stepX = 2, stepY = 2)
          .clone4()
          .clone4()
          .clone4()
          .clone4()
          .clone4()
          .clone4()
          .history: _*
      )

      // A pink dot in the middle of each 16x16 square
      val foreground = g(
        Stamper(x = 7, y = 7, stepX = 16, stepY = 16, tag = pink)
          .stamp()
          .clone4()
          .clone4()
          .clone4()
          .history: _*
      )

      // Draw a path in a circle.
      val layer1 = Stamper(5, 5, tag = darkGray)
        .draw(""" XXX
            |XXXXX
            |XXXXX
            |XXXXX
            | XXX""".stripMargin)

      val layer2 = Stamper(3, 3, tag = darkGray)
        .draw("""  XXXXX
            | XXXXXXX
            |XXX   XXX
            |XX     XX
            |XX     XX
            |XX     XX
            |XXX   XXX
            | XXXXXXX
            |  XXXXX""".stripMargin)

      val layer3 = Stamper(2, 2, tag = darkGray)
        .draw("""   XXXXX
            |  X     X
            | X       X
            |X         X
            |X         X
            |X         X
            |X         X
            |X         X
            | X       X
            |  X     X
            |   XXXXX""".stripMargin)

      val layer4 = Stamper(1, 1, tag = darkGray)
        .draw("""    XXXXX
            |   X     X
            |  X       X
            | X         X
            |X           X
            |X           X
            |X           X
            |X           X
            |X           X
            | X         X
            |  X       X
            |   X     X
            |    XXXXX""".stripMargin)

      val layer5 = Stamper(1, 1, tag = darkGray)
        .draw("""   XXXXXXX
            |  X       X
            | X         X
            |X           X
            |X           X
            |X           X
            |X           X
            |X           X
            |X           X
            |X           X
            | X         X
            |  X       X
            |   XXXXXXX""".stripMargin)

      val layer6 = Stamper(tag = darkGray)
        .draw("""      XXX
            |   XXX   XXX
            |  X         X
            | X           X
            | X           X
            | X           X
            |X             X
            |X             X
            |X             X
            | X           X
            | X           X
            | X           X
            |  X         X
            |   XXX   XXX
            |      XXX""".stripMargin)

      val layer7 = Stamper(tag = darkGray)
        .draw("""     XXXXX
            |   XX     XX
            |  X         X
            | X           X
            | X           X
            |X             X
            |X             X
            |X             X
            |X             X
            |X             X
            | X           X
            | X           X
            |  X         X
            |   XX     XX
            |     XXXXX""".stripMargin)

      // Draw the SVG diagram.
      val plan =
        g(
          g(layer1.history: _*)(transform := "translate(0,0)"),
          g(layer2.history: _*)(transform := "translate(16,0)"),
          g(layer3.history: _*)(transform := "translate(32,0)"),
          g(layer4.history: _*)(transform := "translate(48,0)"),
          g(layer5.history: _*)(transform := "translate(64,0)"),
          g(layer6.history: _*)(transform := "translate(80,0)"),
          g(layer7.history: _*)(transform := "translate(96,0)"),
          g(layer7.history: _*)(transform := "translate(112,0)"),
          g(layer7.history: _*)(transform := "translate(0,16)"),
          g(layer6.history: _*)(transform := "translate(16,16)"),
          g(layer5.history: _*)(transform := "translate(32,16)"),
          g(layer4.history: _*)(transform := "translate(48,16)"),
          g(layer3.history: _*)(transform := "translate(64,16)"),
          g(layer2.history: _*)(transform := "translate(80,16)"),
          g(layer1.history: _*)(transform := "translate(96,16)")
        )

      Svg.toFile(
        File("/tmp/circles.svg"),
        scalatags.Text.svgTags.frag(
          Svg.inkscapeLayer(1, "Background")(background),
          Svg.inkscapeLayer(2, "Circles")(plan),
          Svg.inkscapeLayer(3, "Foreground")(foreground)
        ),
        128,
        128
      )
    }
  }
}
