package com.skraba.byexample.scala.ammonite

import com.skraba.docoptcli.AnsiConsole

/** A [[FileMaker]] is used to generate a file and provide output to the console, checking whether it exists before
  * running the code to create it.
  * @param dstPath
  *   The file to create, if present. If this is None, this class doesn't do anything.
  * @param tag
  *   Some text to print that indicates what type of file is created
  * @param overwrite
  *   Overwrite the file, even if it does exist.
  * @param out
  *   Configuration for printing to the console
  * @param style
  *   Some code for styling a string for output.
  */
case class FileMaker(
    dstPath: Option[os.Path] = None,
    tag: String = "none",
    overwrite: Boolean = false,
    out: AnsiConsole = AnsiConsole(),
    style: String => String = identity,
    thunk: os.Path => Any = _ => ()
) {

  def black(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.black(output), thunk = thunk)
  def red(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.red(output), thunk = thunk)
  def green(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.green(output), thunk = thunk)
  def yellow(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.yellow(output), thunk = thunk)
  def blue(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.blue(output), thunk = thunk)
  def magenta(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.magenta(output), thunk = thunk)
  def cyan(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.cyan(output), thunk = thunk)
  def white(dstPath: Option[os.Path], tag: String)(thunk: os.Path => Any): FileMaker =
    copy(dstPath = dstPath, tag = tag, style = output => out.white(output), thunk = thunk)

  def black(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = black(Some(dstPath), tag)(thunk)
  def red(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = red(Some(dstPath), tag)(thunk)
  def green(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = green(Some(dstPath), tag)(thunk)
  def yellow(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = yellow(Some(dstPath), tag)(thunk)
  def blue(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = blue(Some(dstPath), tag)(thunk)
  def magenta(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = magenta(Some(dstPath), tag)(thunk)
  def cyan(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = cyan(Some(dstPath), tag)(thunk)
  def white(dstPath: os.Path, tag: String)(thunk: os.Path => Any): FileMaker = white(Some(dstPath), tag)(thunk)

  def isEmpty: Boolean = dstPath.isEmpty

  /** Initializes the destination file using the [[thunk]]. */
  lazy val dst: Option[os.Path] = dstPath.map(path => {
    if (!os.exists(path) || overwrite) {
      out.vPrint(style(s"($tag"))
      os.makeDir.all(path / os.up)
      if (os.exists(path)) out.vPrint(style(s"*"))
      thunk(path)
      out.vPrint(style(s")"))
    }
    path
  })

}
