package com.skraba.byexample.scalatags.palette

import com.skraba.byexample.scalatags.Svg
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSpecLike, Matchers}

import scala.reflect.io.File

@RunWith(classOf[JUnitRunner])
class PaletteSpec extends FunSpecLike with Matchers with BeforeAndAfterAll {

  describe("Creating an SVG document") {

    val indigoGray: ColourSwatch = ColourSwatch("Indigo Gray", "323E48", "F8F3EF")
    val seashellWhite: ColourSwatch = ColourSwatch("Seashell White", "F8F3EF", indigoGray.hex)
    val coral: ColourSwatch = ColourSwatch("Coral", "FF6D70", indigoGray.hex)
    val paleCyan: ColourSwatch = ColourSwatch("Pale Cyan", "91D1ED", indigoGray.hex)
    val russianViolet: ColourSwatch = ColourSwatch("Russian Violet", "2C1F56", seashellWhite.hex)
    val deepBlue: ColourSwatch = ColourSwatch("Deep Blue", "19426C", seashellWhite.hex)

    val colours = Seq(coral, indigoGray, seashellWhite, paleCyan, russianViolet, deepBlue)

    val dusk = ColourGradient("Dusk", coral.hex, russianViolet.hex, seashellWhite.hex)
    val twilight = ColourGradient("Twilight", deepBlue.hex, russianViolet.hex, seashellWhite.hex)
    val dawn = ColourGradient("Dawn", paleCyan.hex, deepBlue.hex, seashellWhite.hex)
    val bluebird = ColourGradient("Bluebird", seashellWhite.hex, paleCyan.hex, seashellWhite.hex)
    val cottonCandy = ColourGradient("Cotton Candy", paleCyan.hex, "FFA7A9", seashellWhite.hex)
    val indigoNight = ColourGradient("Indigo Night", "3C4857", "1D2024", seashellWhite.hex)

    val gradients = Seq(dusk, twilight, dawn, bluebird, cottonCandy, indigoNight)

    it("should draw a palette group") {
      Svg.toFile(File("/tmp/palette.svg"), Palette(colours, gradients).toSvg(
        titleFont = "gelasio",
        shadeFont = "Source Sans Pro"
      ))
    }
  }
}
