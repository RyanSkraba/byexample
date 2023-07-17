package com.skraba.byexample.scala.ammonite.git

class ColourCfg {
  val Black: String = ""
  val Red: String = ""
  val Green: String = ""
  val Yellow: String = ""
  val Blue: String = ""
  val Magenta: String = ""
  val Cyan: String = ""
  val White: String = ""

  val BlackB: String = ""
  val RedB: String = ""
  val GreenB: String = ""
  val YellowB: String = ""
  val BlueB: String = ""
  val MagentaB: String = ""
  val CyanB: String = ""
  val WhiteB: String = ""

  val Bold: String = ""
  val Reset: String = ""

  def style(
      in: Any,
      colour: String = White,
      reset: Boolean = true,
      bold: Boolean = false
  ): String = s"${if (bold) Bold else ""}$colour$in${if (reset) Reset else ""}"

  def black(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Black, reset, bold)
  def red(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Red, reset, bold)
  def green(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Green, reset, bold)
  def yellow(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Yellow, reset, bold)
  def blue(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Blue, reset, bold)
  def magenta(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Magenta, reset, bold)
  def cyan(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, Cyan, reset, bold)
  def white(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, White, reset, bold)

  def bold(in: Any, reset: Boolean = true): String = style(in, "", reset, true)

  def ok(in: Any): String = green(in)
  def warn(in: Any): String = yellow(in)
  def error(in: Any): String = red(in)
  def left(in: Any): String = cyan(in)
  def right(in: Any): String = magenta(in)
  def kv(key: Any, value: Any) = magenta(key) + " : " + value
}
object AnsiColourCfg extends ColourCfg {

  import scala.io.AnsiColor
  override val Black: String = AnsiColor.BLACK
  override val Red: String = AnsiColor.RED
  override val Green: String = AnsiColor.GREEN
  override val Yellow: String = AnsiColor.YELLOW
  override val Blue: String = AnsiColor.BLUE
  override val Magenta: String = AnsiColor.MAGENTA
  override val Cyan: String = AnsiColor.CYAN
  override val White: String = AnsiColor.WHITE

  override val BlackB: String = AnsiColor.BLACK_B
  override val RedB: String = AnsiColor.RED_B
  override val GreenB: String = AnsiColor.GREEN_B
  override val YellowB: String = AnsiColor.YELLOW_B
  override val BlueB: String = AnsiColor.BLUE_B
  override val MagentaB: String = AnsiColor.MAGENTA_B
  override val CyanB: String = AnsiColor.CYAN_B
  override val WhiteB: String = AnsiColor.WHITE_B

  override val Bold: String = AnsiColor.BOLD
  override val Reset: String = AnsiColor.RESET

}
