package com.skraba.byexample.scala.ammonite

import com.skraba.docoptcli.AnsiConsole
import mainargs._

import scala.io.AnsiColor

/** A configurable, colourful mechanism for interacting with the user via the terminal.
  *
  * @param verbose
  *   True if the script should print extra verbose information. By default, be concise.
  * @param plain
  *   True if the script should avoid ANSI colour codes. By default, be colourful.
  * @param yes
  *   True if the script should assume the user would reply yes to prompts. By default, ask whether to proceed.
  */
@main
case class ConsoleCfg(
    @arg(short = 'v', doc = "Verbose for extra output")
    verbose: Flag = Flag(false),
    @arg(short = 'p', doc = "Don't use ansi colour codes")
    plain: Flag = Flag(false),
    @arg(short = 'y', doc = "Don't prompt for user confirmation, assume yes")
    yes: Flag = Flag(false)
) extends AnsiConsole {

  /** Makes a copy of this configuration, turning the verbose config on or off. */
  def withVerbose(verbose: Boolean = true): ConsoleCfg = this.copy(verbose = Flag(verbose))

  protected override val _verbose: Boolean = verbose.value

  protected override val _plain: Boolean = plain.value

  protected override val _yes: Boolean = yes.value

  protected override val _print: Option[Any => Any] = None
}

object ConsoleCfg {
  def apply(verbose: Boolean, plain: Boolean, yes: Boolean): ConsoleCfg =
    ConsoleCfg(verbose = Flag(verbose), plain = Flag(plain), yes = Flag(yes))
  implicit def mainargsParser: ParserForClass[ConsoleCfg] =
    ParserForClass[ConsoleCfg]
}
