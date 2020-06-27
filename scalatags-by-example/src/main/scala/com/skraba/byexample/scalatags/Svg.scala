package com.skraba.byexample.scalatags

import java.io.BufferedWriter

import scalatags.Text.TypedTag
import scalatags.Text.implicits._
import scalatags.Text.svgAttrs.{height, viewBox, width}
import scalatags.Text.svgTags.svg

import scala.reflect.io.File

object Svg {

  /** Wraps the contents in an SVG tag and writes them to a file. */
  def toFile(
      f: File,
      svgContents: Modifier,
      dx: Int = 100,
      dy: Int = 100
  ): Unit = {
    val wrapped = svgContents match {
      case svgTag @ TypedTag("svg", _, _) =>
        svgTag
      case _ =>
        svg(width := dx, height := dy, viewBox := s"0 0 $dx $dy")(svgContents)
    }
    val pw: BufferedWriter = f.bufferedWriter()
    pw.write("""<?xml version="1.0"?>""")
    pw.write(wrapped.render)
    pw.close()
  }
}
