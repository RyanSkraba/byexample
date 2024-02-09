package com.skraba.byexample.scalatags.palette

import scalatags.Text.implicits._
import scalatags.Text.svgAttrs._
import scalatags.Text.svgTags._

/** Provides a swatch for a linear gradient in a circle.
  *
  * @param name
  *   The name of the gradient.
  * @param hex1
  *   The first colour to use (i.e. "FF0000")
  * @param hex2
  *   The second colour to use (i.e. "0000FF").
  * @param textHex
  *   The colour of the text naming the gradient.
  */
case class ColourGradient(
    name: String,
    hex1: String,
    hex2: String,
    textHex: String
) {

  /** @return
    *   the SVG gradient definitions that should be put in the {{<defs>}} tag.
    */
  def toSvgDefs(size: Double = 50): Tag =
    linearGradient(
      id := name,
      x1 := -size / 2,
      y1 := -size / 2,
      x2 := size / 2,
      y2 := -size / 2
    )(
      stop(offset := 0.05, stopColor := s"#$hex1"),
      stop(offset := 0.95, stopColor := s"#$hex2")
    )

  /** @param size
    *   The size of the swatch to be applied.
    * @param scale
    *   The circle will be scaled by this amount but remain centered in the area.
    * @param textSize
    *   The font size of the title (unaffected by the scale).
    * @param textAdjust
    *   A helpful hint to push the text into a centered position.
    * @param titleFont
    *   The font to use for the gradient name.
    * @return
    *   a group containing the gradient.
    */
  def toSvg(
      size: Double = 50,
      scale: Double = 1,
      textSize: Double = 5,
      textAdjust: Double = 1,
      titleFont: String = "Sans serif"
  ): Tag = {
    val textTag: Tag = text(
      x := size / 2,
      y := size / 2 + textSize / 2 - textAdjust,
      fill := s"#$textHex",
      textAnchor := "middle",
      fontFamily := titleFont,
      fontWeight := "bold",
      fontSize := textSize
    )
    g(
      circle(
        cx := 0,
        0,
        r := size / 2,
        fill := s"url(#$name)",
        transform := s"translate(${size / 2} ${size / 2}) rotate(45) scale($scale)"
      ),
      textTag(name)
    )
  }
}
