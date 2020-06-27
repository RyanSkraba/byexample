package com.skraba.byexample.scalatags.palette

import com.skraba.byexample.scalatags.Svg
import com.skraba.byexample.scalatags.palette.PaletteSpec.Rainbow
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{BeforeAndAfterAll, FunSpecLike, Matchers}

import scala.reflect.io.File

@RunWith(classOf[JUnitRunner])
class PaletteSpec extends FunSpecLike with Matchers with BeforeAndAfterAll {

  describe("Creating an SVG document") {

    val indigoGray: ColourSwatch =
      ColourSwatch("Indigo Gray", "323E48", "F8F3EF")
    val seashellWhite: ColourSwatch =
      ColourSwatch("Seashell White", "F8F3EF", indigoGray.hex)
    val coral: ColourSwatch = ColourSwatch("Coral", "FF6D70", indigoGray.hex)
    val paleCyan: ColourSwatch =
      ColourSwatch("Pale Cyan", "91D1ED", indigoGray.hex)
    val russianViolet: ColourSwatch =
      ColourSwatch("Russian Violet", "2C1F56", seashellWhite.hex)
    val deepBlue: ColourSwatch =
      ColourSwatch("Deep Blue", "19426C", seashellWhite.hex)

    val colours =
      Seq(coral, indigoGray, seashellWhite, paleCyan, russianViolet, deepBlue)

    val dusk =
      ColourGradient("Dusk", coral.hex, russianViolet.hex, seashellWhite.hex)
    val twilight = ColourGradient(
      "Twilight",
      deepBlue.hex,
      russianViolet.hex,
      seashellWhite.hex
    )
    val dawn =
      ColourGradient("Dawn", paleCyan.hex, deepBlue.hex, seashellWhite.hex)
    val bluebird = ColourGradient(
      "Bluebird",
      seashellWhite.hex,
      paleCyan.hex,
      seashellWhite.hex
    )
    val cottonCandy =
      ColourGradient("Cotton Candy", paleCyan.hex, "FFA7A9", seashellWhite.hex)
    val indigoNight =
      ColourGradient("Indigo Night", "3C4857", "1D2024", seashellWhite.hex)

    val gradients =
      Seq(dusk, twilight, dawn, bluebird, cottonCandy, indigoNight)

    it("should draw a palette group") {
      Svg.toFile(
        File("/tmp/palette.svg"),
        Palette(colours, gradients)
          .toSvg(titleFont = "gelasio", shadeFont = "Source Sans Pro")
      )
    }

    it("should draw a pretty rainbow palette group") {

      Svg.toFile(
        File("/tmp/rainbow.svg"),
        Palette(
          colours = Seq(
            Rainbow.violet,
            Rainbow.blue,
            Rainbow.green,
            Rainbow.yellow,
            Rainbow.orange,
            Rainbow.red
          ),
          gradients = Seq(
            ColourGradient(
              "Vibe",
              Rainbow.violet.hex,
              Rainbow.blue.hex,
              "FFFFFF"
            ),
            ColourGradient(
              "Bilge",
              Rainbow.blue.hex,
              Rainbow.green.hex,
              "FFFFFF"
            ),
            ColourGradient(
              "Gooey",
              Rainbow.green.hex,
              Rainbow.yellow.hex,
              "FFFFFF"
            ),
            ColourGradient(
              "Yoink",
              Rainbow.yellow.hex,
              Rainbow.orange.hex,
              "FFFFFF"
            ),
            ColourGradient(
              "Oscule",
              Rainbow.orange.hex,
              Rainbow.red.hex,
              "FFFFFF"
            ),
            ColourGradient(
              "Ruddy",
              Rainbow.red.hex,
              Rainbow.violet.hex,
              "FFFFFF"
            )
          )
        ).toSvg(titleFont = "Alegreya SC", shadeFont = "Alegreya Sans SC")
      )
    }
  }
}

object PaletteSpec {

  object Rainbow {
    val violet: ColourSwatch = ColourSwatch("Violet", "6a245c", "FFFFFF")
    val blue: ColourSwatch = ColourSwatch("Blue", "3543a6", "FFFFFF")
    val green: ColourSwatch = ColourSwatch("Green", "498c13", "FFFFFF")
    val yellow: ColourSwatch = ColourSwatch("Yellow", "fec938", "FFFFFF")
    val orange: ColourSwatch = ColourSwatch("Orange", "ef6d0c", "FFFFFF")
    val red: ColourSwatch = ColourSwatch("Red", "c8361e", "FFFFFF")
  }

  object RainbowDark {
    val violet: ColourSwatch = ColourSwatch("Violet", "4c2451", "FFFFFF")
    val blue: ColourSwatch = ColourSwatch("Blue", "242551", "FFFFFF")
    val green: ColourSwatch = ColourSwatch("Green", "406244", "FFFFFF")
    val yellow: ColourSwatch = ColourSwatch("Yellow", "dab71f", "FFFFFF")
    val orange: ColourSwatch = ColourSwatch("Orange", "d8832b", "FFFFFF")
    val red: ColourSwatch = ColourSwatch("Red", "c94d4d", "FFFFFF")
  }

}
