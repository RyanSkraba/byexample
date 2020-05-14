package com.skraba.byexample.scalatags.palette

import scalatags.Text.implicits._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._

/**
 * Creates a palette with all of the given colours and the gradients.
 *
 * @param colours   The colour swatches to include in the palette.
 * @param gradients The colour gradients to include in the palette.
 * @param dx        The width of the swatch.
 * @param dy        The height of the swatch.
 */
case class Palette(colours: Seq[ColourSwatch],
                   gradients: Seq[ColourGradient],
                   dx: Double = 297,
                   dy: Double = 210) {

  /**
   * @param swatchDy   The height of the swatch.
   * @param textSize   The font size of the title.
   * @param textAdjust A helpful hint to push the text into a centered position.
   * @param shades     The number of shades to apply on the swatch.
   * @param shadeStep  The proportion to lighten per shade.
   * @param shadeDy    The height of each shade colour.
   * @param titleFont  The font to use for the colour name.
   * @param shadeFont  The font to use for RGB values.
   * @return the swatch for the given colour. */
  def toSvg(swatchDy: Double = 160,
            textSize: Double = 5,
            textAdjust: Double = 2,
            shades: Int = 4,
            shadeStep: Double = 0.2,
            shadeDy: Double = 16,
            titleFont: String = "Sans serif",
            shadeFont: String = "Sans serif"): Tag = {

    val swatchWidth = dx / colours.size
    svg(width := dx, height := dy, viewBox := s"0 0 $dx $dy")(
      Seq(
        defs(gradients.map(_.toSvgDefs()): _*)
      ) ++ (
        for ((c, i) <- colours.zipWithIndex)
          yield c.toSvg(
            dx = swatchWidth,
            dy = swatchDy,
            textSize = textSize,
            textAdjust = textAdjust,
            shades = shades,
            shadeStep = shadeStep,
            shadeDy = shadeDy,
            titleFont = titleFont,
            shadeFont = shadeFont)(transform := s"translate(${swatchWidth * i}, 0)")
        ) ++ (
        for ((g, i) <- gradients.zipWithIndex)
          yield g.toSvg(
            size = swatchWidth,
            scale = 0.8,
            titleFont = "gelasio")(transform := s"translate(${swatchWidth * i}, $swatchDy)")
        )
        : _*)
  }

}