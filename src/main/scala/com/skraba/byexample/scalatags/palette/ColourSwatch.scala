package com.skraba.byexample.scalatags.palette

import scalatags.Text.implicits._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._

/**
 * Provides a swatch for a colour in the palette, including lighter shades in a series of steps.
 *
 * The swatch is organised vertically with the title and RGB (in hex) at the top, and the shades at the bottom.
 *
 * @param name    The name of the colour
 * @param hex     The colour to use (i.e. "FF0000")
 * @param textHex The colour of the text naming the gradient.
 */
case class ColourSwatch(name: String,
                        hex: String,
                        textHex: String) {

  /** @return a hex value that is f percent lighter than the actual colour. */
  def lightened(f: Double): String = {
    hex
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .map(255 - _)
      .map(_ * (1 - f) + 0.5)
      .map(255 - _)
      .map(_.toInt)
      .map(colour => f"$colour%02X")
      .mkString
  }

  /** @return a hex value that is f percent darker than the actual colour. */
  def darkened(f: Double): String = {
    hex
      .sliding(2, 2)
      .map(Integer.parseInt(_, 16))
      .map(_ * (1 - f) + 0.5)
      .map(_.toInt)
      .map(colour => f"$colour%02X")
      .mkString
  }

  /**
   * @param dx         The width of the swatch.
   * @param dy         The height of the swatch.
   * @param textSize   The font size of the title.
   * @param textAdjust A helpful hint to push the text into a centered position.
   * @param shades     The number of shades to apply on the swatch.
   * @param shadeStep  The proportion to lighten per shade.
   * @param shadeDy    The height of each shade colour.
   * @param titleFont  The font to use for the colour name.
   * @param shadeFont  The font to use for RGB values.
   * @return the swatch for the given colour. */
  def toSvg(dx: Double = 50,
            dy: Double = 160,
            textSize: Double = 5,
            textAdjust: Double = 2,
            shades: Int = 4,
            shadeStep: Double = 0.2,
            shadeDy: Double = 16,
            titleFont: String = "Sans serif",
            shadeFont: String = "Sans serif"): Tag = {

    val textTag: Tag = text(x := dx / 2, fill := s"#$textHex",
      textAnchor := "middle", fontSize := textSize)

    /** Preconfigured text tag for the name of this colour. */
    val titleTag: Tag = textTag(fontFamily := titleFont, fontWeight := "bold")

    /** Preconfigured text tag for the RGB values. */
    val hexTag: Tag = textTag(fontFamily := shadeFont)

    /** @return a swatch for a shade. */
    def shade(rgb: String): Tag = g(
      rect(width := dx, height := shadeDy, fill := rgb),
      hexTag(y := shadeDy / 2 + textAdjust)(rgb.toLowerCase())
    )

    g(
      rect(y := 0, width := dx, height := dy, fill := s"#$hex"),
      titleTag(y := shadeDy / 2 + textAdjust)(name),
      hexTag(y := shadeDy / 2 + textAdjust * 2 + textSize)(s"#${hex.toLowerCase}")
    )(
      (1 to shades)
        .map {
          n =>
            shade("#" + lightened(shadeStep * n))(
              transform := s"translate(0, ${dy - shadeDy * (shades - n + 1)})")
        }: _*)
  }
}
