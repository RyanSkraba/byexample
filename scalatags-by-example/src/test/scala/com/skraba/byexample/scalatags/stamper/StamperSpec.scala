package com.skraba.byexample.scalatags.stamper

import com.skraba.byexample.scalatags.Svg
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSpecLike, Matchers}
import scalatags.Text.implicits._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._

import scala.reflect.io.File

@RunWith(classOf[JUnitRunner])
class StamperSpec extends FunSpecLike with Matchers with BeforeAndAfterAll {

  describe("Using a Stamper") {

    val base = rect(width := 1, height := 1)

    it("draws two elements when moved") {
      val origin = Stamper(stamp = rect)
      val east = origin.e()

      east.x shouldBe 1.0
      east.y shouldBe 0.0

      east.asTags should have size 2
      east.asTags.head shouldBe rect(x := 1.0, y := 0.0)

      g(east.asTags: _*).render shouldBe """<g><rect x="1.0" y="0.0"></rect><rect x="0.0" y="0.0"></rect></g>"""
    }

    it("retains a checkpoint") {

      val stamp = Stamper(stamp = rect).se().save("1").se().e().se().se().save("2").se()

      stamp.checkpoints("1") shouldBe(1.0, 1.0)
      stamp.checkpoints("2") shouldBe(5.0, 4.0)
      stamp.asTags.head shouldBe rect(x := 6.0, y := 5.0)
    }

    it("should stamp in all redirections") {
      val origin = Stamper(stamp = rect)
      origin.n().asTags.head shouldBe rect(x := 0.0, y := -1.0)
      origin.s().asTags.head shouldBe rect(x := 0.0, y := 1.0)
      origin.e().asTags.head shouldBe rect(x := 1.0, y := 0.0)
      origin.w().asTags.head shouldBe rect(x := -1.0, y := 0.0)
      origin.ne().asTags.head shouldBe rect(x := 1.0, y := -1.0)
      origin.nw().asTags.head shouldBe rect(x := -1.0, y := -1.0)
      origin.se().asTags.head shouldBe rect(x := 1.0, y := 1.0)
      origin.sw().asTags.head shouldBe rect(x := -1.0, y := 1.0)
    }

    it("should stamp in all redirections with a custom distance and position") {
      val origin = Stamper(x = 1, y = 2, dx = 3, dy = 4, stamp = rect)
      origin.n().asTags.head shouldBe rect(x := 1.0, y := -2.0)
      origin.s().asTags.head shouldBe rect(x := 1.0, y := 6.0)
      origin.e().asTags.head shouldBe rect(x := 4.0, y := 2.0)
      origin.w().asTags.head shouldBe rect(x := -2.0, y := 2.0)
      origin.ne().asTags.head shouldBe rect(x := 4.0, y := -2.0)
      origin.nw().asTags.head shouldBe rect(x := -2.0, y := -2.0)
      origin.se().asTags.head shouldBe rect(x := 4.0, y := 6.0)
      origin.sw().asTags.head shouldBe rect(x := -2.0, y := 6.0)
    }

    it("should create a couple of paths") {
      val lightGray = base(fill := "#EEE")
      val darkGray = base(fill := "#888")
      val pink = base(fill := "#F88")
      val door = base(fill := "#FFF")

      val circle = Stamper(stamp = darkGray)
        .w().w().w().w().sw().w().sw().sw().s()
        .sw().s().s().s().s().save("eastWall").se().s().se().se().e()
        .se().e().e().e().e().ne().e().ne().ne().n()
        .ne().n().n().n().n().nw().n().nw().nw().w()

      val eastWall = circle.checkpoints.getOrElse("eastWall", (0.0, 0.0))

      val extra = Stamper(x = eastWall._1 - 1, y = eastWall._2, stamp = lightGray).w().sw()
        .s(door).s(lightGray).se()
        .sw(pink).s(door).s(pink).se().e(door).e(pink)
        .ne(lightGray).se().e(door).e(lightGray)
        .se(pink).se().e(door).e(pink).ne()
        .ne(lightGray).e(door).e(lightGray).ne().n().n().n()

      val plan = g(circle.asTags: _*)(extra.asTags: _*)(transform := "translate(12,0)")

      Svg.toFile(File("/tmp/plan.svg"), plan, 100, 100);
    }
  }
}
