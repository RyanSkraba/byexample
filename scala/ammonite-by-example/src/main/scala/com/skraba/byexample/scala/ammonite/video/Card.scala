package com.skraba.byexample.scala.ammonite.video

import com.skraba.byexample.scala.ammonite.FileMaker
import com.skraba.docoptcli.AnsiConsole

/** A card is an image that should be applied to the beginning or the end of a video.
  * @param genSvg
  *   A generator to create the SVG file for this card
  * @param filename
  *   The filename to use for the different files, which will be appended with the appropriate extension.
  * @param maker
  *   A helper configured to create the files lazily
  * @param dirSvg
  *   The SVG output directory
  * @param dirPng
  *   The PNG output directory
  * @param dirMp4
  *   The MP4 output directory
  * @param duration
  *   The length of the output video (in seconds)
  */
case class Card(
    genSvg: AnsiConsole => String,
    filename: String,
    maker: FileMaker = FileMaker(),
    dirSvg: os.Path,
    dirPng: os.Path,
    dirMp4: os.Path,
    duration: Int = 5
) {

  /** The title card in SVG format. */
  val svg: FileMaker = maker.blue(dstPath = dirSvg / (filename + ".svg"), tag = "svg") {
    os.write.over(_, genSvg(maker.out))
  }

  /** The title card in PNG format. */
  val png: FileMaker = maker
    .green(dstPath = dirPng / (filename + ".png"), tag = "png") {
      Ffmpeg().osProc(Card.Inkscape, "-w", "1920", "-h", "1080", svg.dst.get, "--export-filename", _)
    }

  private lazy val mp4Cache = collection.mutable.Map[Int, FileMaker]()

  /** The title card in MP4 format. */
  def mp4(ff: Ffmpeg, frameRate: Int = 25): FileMaker = mp4Cache.getOrElseUpdate(
    frameRate, {
      val mp4Path = dirMp4 / (filename + (if (frameRate == 25) "" else s".${frameRate}fps") + ".mp4")
      maker.magenta(mp4Path, "mp4") { ff.mp4(_).pngToMp4(png.dst.get, frameRate = frameRate, duration = duration) }
    }
  )
}

object Card {

  /** The command to call to run the Inkscape CLI. */
  val Inkscape: String = "inkscape"

  /** Given a template string, searches and replaces some lines in the template.
    *
    * The tags in the template look like `[[TAG4.3]]` where the first digit is the total number of lines that were found
    * and the second digit is to be replaced by the n'th line.
    *
    * For example, if there are three incoming lines `[[TAG3.1]]`, `[[TAG3.2]]`, `[[TAG3.3]]` will be replaced.
    *
    * If there is one incoming line, `[[TAG]]`, `[[TAG1]]`, and `[[TAG1.1]]` are all replaced.
    *
    * Any of the unreplaced tags are automatically deleted from the template.
    *
    * @param tmpl
    *   The incoming string to be replaced.
    * @param tag
    *   The tag to use when looking up in the template.
    * @param lines
    *   The lines to replace in the template.
    * @return
    *   The result of all the replacements.
    */
  def multiReplaceInTemplate(tmpl: String, tag: String, lines: Seq[String]): String = {
    val tagRe = s"\\[\\[($tag(\\d*)(\\.\\d*)?)]]".r

    val max = tagRe.findAllMatchIn(tmpl).map(_.group(2)).filterNot(_.isEmpty).map(_.toInt).max

    val replaced = lines.size match {
      case 1 =>
        // If there's only one line incoming, then replace [[TAG]], [[TAG1]] or [[TAG1.1]]
        tmpl
          .replaceAll(s"\\[\\[$tag]]", lines.head)
          .replaceAll(s"\\[\\[${tag}1]]", lines.head)
          .replaceAll(s"\\[\\[${tag}1.1]]", lines.head)
      case size if size <= max =>
        // Otherwise replace [[TAGM.N]] (e.g.  [[TAG2.1]] and [[TAG2.2]] for two lines)
        lines.zipWithIndex.foldLeft(tmpl) { case (in, (line, lineNum)) =>
          in.replaceAll(s"\\[\\[$tag$size.${lineNum + 1}]]", line)
        }
      case _ => throw new IllegalStateException(s"Weird $tag: " + lines)
    }

    tagRe.replaceAllIn(replaced, "")
  }

}
