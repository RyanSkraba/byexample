package com.skraba.byexample.scalatags.palette

import com.skraba.byexample.scalatags.ScalatagsGo
import com.skraba.byexample.scalatags.ScalatagsGo.InternalDocoptException

import scala.jdk.CollectionConverters._

/** Command-line driver for generating a Palette SVG
  */
object PaletteTask {

  val Doc: String =
    """Generate a pretty palette with hex codes and colours.
    |
    |Usage:
    |  ScalatagsGo palette [--swatch=SWATCHSPEC]... [--gradient=GRADSPEC]...
    |
    |Options:
    |  -h --help            Show this screen.
    |  --version            Show version.
    |  --swatch=SWATCHSPEC  Add a colour swatch to the palette.
    |  --gradient=GRADSPEC  Add a gradient bubble to the palette.
    |
    |Swatches and gradients follow the patterns like this:
    |
    |--swatch=FF0000:FireHydrant
    |  A red swatch starting at pure red named FireHydrant, with default black text.
    |
    |--swatch=FF6666/880000:Pinky
    |  Slightly lighter pinks using red text
    |
    |--gradient=66FF66-DDFFDD/008800:EmeraldCity
    |  A gradient of shades of pure green.
    |
    |""".stripMargin.trim

  val Cmd = "palette"

  val Description = "Generate a pretty palette with hex codes and colours."

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val optSwatches: Seq[String] =
      opts
        .get("--swatch")
        .asInstanceOf[java.lang.Iterable[String]]
        .asScala
        .toSeq
    val optGradients: Seq[String] =
      opts
        .get("--gradient")
        .asInstanceOf[java.lang.Iterable[String]]
        .asScala
        .toSeq

    // The swatch and gradient specifications as regex
    val swatchSpec = """([0-9A-F]{6}):(.*)""".r
    val swatchSpecText = """([0-9A-F]{6})/([0-9A-F]{6}):(.*)""".r
    val gradientSpec = """([0-9A-F]{6})-([0-9A-F]{6}):(.*)""".r
    val gradientSpecText =
      """([0-9A-F]{6})-([0-9A-F]{6})/([0-9A-F]{6}):(.*)""".r

    val palette = Palette(
      colours = optSwatches map {
        case swatchSpec(hex, name) => ColourSwatch(name, hex, "FFFFFF")
        case swatchSpecText(hex, textHex, name) =>
          ColourSwatch(name, hex, textHex)
        case unknownSpec =>
          throw new InternalDocoptException(s"Unknown swatch: $unknownSpec")
      },
      gradients = optGradients map {
        case gradientSpec(hex1, hex2, name) =>
          ColourGradient(name, hex1, hex2, "FFFFFF")
        case gradientSpecText(hex1, hex2, textHex, name) =>
          ColourGradient(name, hex1, hex2, textHex)
        case unknownSpec =>
          throw new InternalDocoptException(s"Unknown gradient: $unknownSpec")
      }
    )

    println(palette.toSvg().render)

  }

  val Task: ScalatagsGo.Task = ScalatagsGo.Task(Doc, Cmd, Description, go)
}
