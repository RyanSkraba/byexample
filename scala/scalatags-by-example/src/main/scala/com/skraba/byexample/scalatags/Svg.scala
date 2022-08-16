package com.skraba.byexample.scalatags

import java.io.BufferedWriter

import scalatags.Text.TypedTag
import scalatags.Text.implicits._
import scalatags.Text.svgTags.{g, svg, text}
import scalatags.Text.tags.attr
import scalatags.generic
import scalatags.text.Builder

import scala.reflect.io.File

object Svg {

  /** Useful pointer to qualify attribute names, since they can conflict with
    * common variables.
    */
  val Attrs: scalatags.Text.svgAttrs.type = scalatags.Text.svgAttrs

  val XmlNs = attr("xmlns") := "http://www.w3.org/2000/svg"

  /** Wraps the contents in an SVG tag. If the content is already an svg tag, it
    * isn't wrapped.
    *
    * @param svgContents
    *   The tag or fragment to write to the file.
    * @param viewBoxDx
    *   The width of the document and viewbox.
    * @param viewBoxDy
    *   The height of the document and viewbox.
    */
  def wrapSvg(
      svgContents: Modifier,
      viewBoxDx: Int = 100,
      viewBoxDy: Int = 100
  ): TypedTag[String] = {
    svgContents match {
      case svgTag @ TypedTag("svg", _, _) =>
        svgTag
      case _ =>
        svg(
          Attrs.width := viewBoxDx,
          Attrs.height := viewBoxDy,
          Attrs.viewBox := s"0 0 $viewBoxDx $viewBoxDy",
          XmlNs
        )(svgContents)
    }
  }

  /** Wraps the contents in an SVG tag and writes them to a file. If the content
    * is already an svg tag, it isn't wrapped.
    *
    * @param f
    *   The file to write or overwrite.
    * @param svgContents
    *   The tag or fragment to write to the file.
    * @param viewBoxDx
    *   The width of the document and viewbox.
    * @param viewBoxDy
    *   The height of the document and viewbox.
    */
  def toFile(
      f: File,
      svgContents: Modifier,
      viewBoxDx: Int = 100,
      viewBoxDy: Int = 100,
      license: Option[String] = None
  ): Unit = {
    val pw: BufferedWriter = f.bufferedWriter()
    pw.write("""<?xml version="1.0"?>""")
    license.foreach(l => pw.write(s"\n<!--\n\n$l\n\n-->\n"))
    pw.write(wrapSvg(svgContents, viewBoxDx, viewBoxDy).render)
    pw.close()
  }

  /** Returns a group tag that can be used as an inkscape layer if at the top of
    * the document.
    *
    * @param n
    *   The unique number to use for the layer. This should (probably) be
    *   sequential.
    * @param label
    *   The visible name to use for the layer.
    * @return
    *   The group tag to fill with contents.
    */
  def inkscapeLayer(n: Int, label: String): Tag =
    g(
      Attrs.id := s"layer$n",
      attr("inkscape:groupmode") := "layer",
      attr("inkscape:label") := label
    )

  /** Returns an attribute that can be added to translate an element.
    *
    * @param dx
    *   The number of pixels right to translate.
    * @param label
    *   The number of pixels down to translate.
    * @return
    *   The attribute to add to a tag.
    */
  def attrTranslate(dx: Double = 0, dy: Double = 0): generic.Modifier[Builder] =
    Attrs.transform := s"translate($dx,$dy)"

  /** A text config stores information about how to create small text tags.
    * @param fill
    *   The text colour.
    * @param family
    *   The font-family attribute
    * @param weight
    *   the font-weight attribute
    * @param size
    *   The font-size attribute
    * @param adjust
    *   A percentage to adjust text *downwards* based on the size.
    */
  case class Text(
      fill: String = "000000",
      family: String = "San Serif",
      weight: String = "normal",
      size: Double = 8,
      adjust: Double = 0.1
  ) {

    /** The base tag with the attributes applied from the case class. */
    lazy val base: Tag = text(
      Attrs.fill := s"#${fill.stripMargin('#')}",
      Attrs.fontFamily := family,
      Attrs.fontWeight := weight,
      Attrs.fontSize := size
    )

    /** A tag centered horizontally and vertically on the given rectangle. */
    def middle(x: Double, y: Double, dx: Double, dy: Double): Tag = {
      base(
        Attrs.x := x + dx / 2,
        Attrs.y := y + dy / 2 + size * adjust,
        Attrs.textAnchor := "middle",
        Attrs.dominantBaseline := "middle"
      )
    }

    /** A tag centered horizontally at the given point. */
    def center(x: Double, y: Double): Tag = {
      base(
        Attrs.x := x,
        Attrs.y := y,
        Attrs.textAnchor := "middle"
      )
    }

    /** A tag left-aligned at the given point. */
    def left(x: Double, y: Double): Tag = {
      base(
        Attrs.x := x,
        Attrs.y := y,
        Attrs.textAnchor := "start"
      )
    }

    /** A tag vertically and right-aligned on the given rectangle. */
    def left(x: Double, y: Double, dx: Double, dy: Double): Tag = {
      base(
        Attrs.x := 0,
        Attrs.y := y + dy / 2 + size * adjust,
        Attrs.textAnchor := "start",
        Attrs.dominantBaseline := "middle"
      )
    }

    /** A tag right-aligned at the given point. */
    def right(x: Double, y: Double): Tag = {
      base(
        Attrs.x := x,
        Attrs.y := y,
        Attrs.textAnchor := "end"
      )
    }

    /** A tag vertically and right-aligned on the given rectangle. */
    def right(x: Double, y: Double, dx: Double, dy: Double): Tag = {
      base(
        Attrs.x := x,
        Attrs.y := y + dy / 2 + size * adjust,
        Attrs.textAnchor := "end",
        Attrs.dominantBaseline := "middle"
      )
    }
  }
}
