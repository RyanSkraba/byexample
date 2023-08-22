package com.skraba.byexample.scala.ammonite

import mainargs._

import scala.io.AnsiColor

/** A configurable, colourful mechanism to print to standard out.
  * @param verbose
  * @param plain
  */
@main
case class ColourCfg(
    @arg(doc = "Verbose for extra output")
    verbose: Flag = Flag(false),
    @arg(doc = "Don't use ansi colour codes")
    plain: Flag = Flag(false)
) {

  val Black: String = ifAnsi(AnsiColor.BLACK)
  val Red: String = ifAnsi(AnsiColor.RED)
  val Green: String = ifAnsi(AnsiColor.GREEN)
  val Yellow: String = ifAnsi(AnsiColor.YELLOW)
  val Blue: String = ifAnsi(AnsiColor.BLUE)
  val Magenta: String = ifAnsi(AnsiColor.MAGENTA)
  val Cyan: String = ifAnsi(AnsiColor.CYAN)
  val White: String = ifAnsi(AnsiColor.WHITE)

  val BlackBg: String = ifAnsi(AnsiColor.BLACK_B)
  val RedBg: String = ifAnsi(AnsiColor.RED_B)
  val GreenBg: String = ifAnsi(AnsiColor.GREEN_B)
  val YellowBg: String = ifAnsi(AnsiColor.YELLOW_B)
  val BlueBg: String = ifAnsi(AnsiColor.BLUE_B)
  val MagentaBg: String = ifAnsi(AnsiColor.MAGENTA_B)
  val CyanBg: String = ifAnsi(AnsiColor.CYAN_B)
  val WhiteBg: String = ifAnsi(AnsiColor.WHITE_B)

  val Bold: String = ifAnsi(AnsiColor.BOLD)
  val Reset: String = ifAnsi(AnsiColor.RESET)

  private[this] def ifAnsi(ansi: String, notAnsi: String = ""): String =
    if (this.plain.value) notAnsi else ansi;

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

  def blackBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, BlackBg, reset, bold)
  def redBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, RedBg, reset, bold)
  def greenBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, GreenBg, reset, bold)
  def yellowBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, YellowBg, reset, bold)
  def blueBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, BlueBg, reset, bold)
  def magentaBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, MagentaBg, reset, bold)
  def cyanBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, CyanBg, reset, bold)
  def whiteBg(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    style(in, WhiteBg, reset, bold)

  def bold(in: Any, reset: Boolean = true): String =
    style(in, "", reset, bold = true)

  def ok(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    green(in, reset, bold)
  def warn(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    yellow(in, reset, bold)
  def error(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    red(in, reset, bold)
  def left(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    cyan(in, reset, bold)
  def right(in: Any, reset: Boolean = true, bold: Boolean = false): String =
    magenta(in, reset, bold)
  def kv(key: Any, value: Any, reset: Boolean = true, bold: Boolean = false): String =
    magenta(key, reset, bold) + " : " + value

  /** Print a standardized help for a script with subcommands
    * @param cli
    *   The script name
    * @param description
    *   A oneliner description of the script
    * @param subcommands
    *   Pairs of subcommands and their oneliner description
    * @return
    *   The string to print for that script.
    */
  def helpHeader(
      cli: String,
      description: String,
      subcommands: Tuple2[String, String]*
  ): String = {

    val tmpl = s" $Cyan%1$$${subcommands.map(_._1.length).max}s$Reset : %2$$s"

    s"""$Bold$Green$cli$Reset - $description
       |
       |${subcommands.map(p => tmpl.format(p._1, p._2)).mkString("\n")}
       |
       |${Bold}Usage:$Reset
       |""".stripMargin
  }

  /** Print a usage line for a script.
    * @param cli
    *   The script name
    * @param subcommand
    *   The subcommand to run
    * @param args
    *   The arguments to demonstrate the script
    * @return
    *   The string to print for that script line
    */
  def helpUse(cli: String, subcommand: String, args: String*): String =
    s"""$Green$cli $Cyan$subcommand$Reset """ + args
      .map {
        case arg if arg.startsWith("[") => arg
        case arg                        => bold(arg)
      }
      .mkString(" ")

  def withVerbose: ColourCfg = this.copy(verbose = new Flag(true))
  def vPrint(in: => String): Unit = if (verbose.value) Console.print(in)
  def vPrintln(in: => String): Unit = if (verbose.value) Console.println(in)
}

object ColourCfg {
  def apply(ansi: Boolean, verbose: Boolean): ColourCfg =
    ColourCfg(Flag(ansi), Flag(verbose))
  implicit def mainargsParser: ParserForClass[ColourCfg] =
    ParserForClass[ColourCfg]
}