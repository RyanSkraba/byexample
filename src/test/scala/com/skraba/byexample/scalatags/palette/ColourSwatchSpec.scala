package com.skraba.byexample.scalatags.palette

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSpecLike, Matchers}

@RunWith(classOf[JUnitRunner])
class ColourSwatchSpec extends FunSpecLike with Matchers with BeforeAndAfterAll {

  val white: ColourSwatch = ColourSwatch("Red", "FFFFFF", "000000")
  val red: ColourSwatch = ColourSwatch("Red", "FF0000", white.hex)

  describe("Lightening a colour swatch") {
    red.lightened(0.0) shouldBe "FF0000"
    red.lightened(0.2) shouldBe "FF3333"
    red.lightened(0.5) shouldBe "FF8080"
    red.lightened(1.0) shouldBe "FFFFFF"

    ColourSwatch("Red", "FF6D70", white.hex).lightened(0.2) shouldBe "FF8A8D"
  }
}