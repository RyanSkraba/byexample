package com.skraba.byexample.scalatags.countdown

import com.skraba.byexample.scalatags.ScalatagsGo

import scala.reflect.io.{Directory, File}
import scala.util.matching.Regex

/** Command-line driver for generating a countdown video frame by frame based on an SVG template, and which can be
  * turned to images and assembled into a video.
  */
object CountdownTask {

  val Doc: String =
    """Generate a pretty palette with hex codes and colours.
    |
    |Usage:
    |  ScalatagsGo countdown TEMPLATE [options]
    |
    |Options:
    |  -h --help      Show this screen.
    |  --version      Show version.
    |  TEMPLATE       The template file to use
    |  --dstDir=DIR   The destination directory, relative to the template
    |  --frameRate=N  The number of frames per second to use [default: 30]
    |
    |""".stripMargin.trim

  val Cmd = "countdown"

  val Description = "Generate a countdown timer from a template."

  /** Represents the countdown video.
    *
    * @param src
    *   The SVG template that will be adjusted for each frame.
    * @param dstDir
    *   The directory to use for each output frame.
    * @param frameRate
    *   The number of frames per second for the video
    * @param warmup
    *   The number of seconds in the video before the countdown starts (READY, SET...)
    * @param timer
    *   The number of seconds that the countdown should last (GO!)
    * @param cooldown
    *   The number of seconds after the countdown (Please stop now)
    */
  case class Video(
      src: File,
      dstDir: Directory,
      frameRate: Int = 30,
      warmup: Double = 2,
      timer: Double = 5 * 60,
      cooldown: Double = 2
  ) {

    /** The total number of frames that the video will include */
    val framesTotal: Int = frameRate * (warmup + timer + cooldown).toInt

    /** The width and height of the template. */
    val (dx, dy) = (1920, 1080)

    // val inXml: Elem = XML.loadString(inString)

    /** The contents of the template file */
    val inString: String = src.slurp()

    def write(): Unit = {
      for (f <- 0 to framesTotal) {
        val dst: File =
          (dstDir / (src.stripExtension + f".${f / frameRate}%05d.${f % frameRate}%05d" + "." + src.extension)).toFile
        dst.writeAll(Frame(this, f).contents())
      }
    }
  }

  /* Pretty failed experiment trying to rigorously parse XML :/
  case class XmlFrame(v: Video, num: Int) {
    val dst: File = (v.dstDir / (v.src.stripExtension + f".$num%05d" + "." + v.src.extension)).toFile

    def find(in: Node, key: String, pre: String = "inkscape"): Option[String] =
      in.attributes.collectFirst { case p: PrefixedAttribute if p.pre == pre && p.key == key => p }.map(_.toString)

    def replaceLayer(in: Node, layerName: String)(thunk: Node => Option[Node]): Node = in match {
      case e @ Elem(_, "g", _, _, _, children) if find(e, "label").contains(layerName) =>
        e match {
          case elem: Elem => elem.copy(child = children.flatMap(thunk))
          case _          => in
        }
      case e: Elem => e.copy(child = e.child.map(replaceLayer(_, layerName)(thunk)))
      case _       => in
    }

    def replaceTime(in: Node): Option[Node] = in match {
      case Text(txt) =>
        Some(Text(txt.replace("$:$$", "5:00")))
      case e: Elem => Some(e.copy(child = e.child.flatMap(replaceTime)))
      case other   => Some(other)
    }

    lazy val outXml: Node = replaceLayer(v.inXml, "Countdown")(replaceTime)

    def write(): Unit = {
      val outWriter = new StringWriter()
      XML.write(outWriter, outXml, "UTF-8", xmlDecl = true, doctype = null)
      outWriter.close()
      val outString = outWriter.toString
      dst.writeAll(outString)
    }
  }
   */

  /** Represents a single frame in the video.
    * @param v
    *   The video being written
    * @param frameNum
    *   This current frame number
    */
  case class Frame(v: Video, frameNum: Int) {

    /** A relative time in seconds to the start of the actual timer (the warmup will be negative). */
    val time: Double = frameNum.toDouble / v.frameRate - v.warmup

    /** The time bounded by the expected timer duration (not negative or greater than the timer). */
    val timeBounded: Double = v.timer min time max 0

    /** The bounded time represented as a floating point number between 0 and 1. */
    val fraction: Double = timeBounded / v.timer

    /** The time of the task represented as M:SS */
    val timeRemaining: String = f"${(v.timer - timeBounded).toInt / 60}%01d:${(v.timer - timeBounded).toInt % 60}%02d"

    /** The center and size of the progress pie. */
    val (cx, cy, r) = (v.dx / 2, v.dy / 2, 890 / 2)

    private val PathRegex: Regex = raw"""(?s).*(<path.*?id="progress".*?/>)""".r
    private val DAttributeRegex: Regex = raw"""\bd="(.*?)"""".r

    /** @return
      *   An SVG path representing the progress of the timer.
      */
    def progressPath(): String = {
      val degrees = math.Pi * fraction
      // The starting and ending point of the progress pie.  These lie along a circle.
      val (drx, dry) = (r * math.cos(degrees), r * math.sin(degrees))
      val (p1x, p1y) = (cx - drx, cy - dry)
      val (p2x, p2y) = (cx - drx, cy + dry)
      // The path is a straight line from the center to the first point, then an arc to the farthest right point,
      // then another arc continuing to the second point, then a straight line back to the center.
      // Separating this into two arcs lets us guarantee that the arc size is always smaller than half the
      // circumference, which simplifies the path.
      s"M $cx $cy L $p1x, $p1y A $r, $r 0 0 1 ${cx + r} $cy A $r, $r 0 0 1 $p2x, $p2y L $cx $cy z"
    }

    def contents(): String = {
      // Replace the time in the string
      val timeOutString = v.inString.replace("$:$$", timeRemaining)

      // Find the path representing the progress of the talk
      val m1 = PathRegex.findFirstMatchIn(timeOutString).get

      // And use the match to replace the path.
      val out =
        "" + m1.before(1) + DAttributeRegex.replaceFirstIn(m1.group(1), "d=\"" + progressPath() + '"') + m1.after(1)
      out
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val template: File = File(opts.get("TEMPLATE").asInstanceOf[String])
    val dstDir: Directory = {
      Option(opts.get("--dstDir").asInstanceOf[String])
        .map(Directory.apply(_))
        .getOrElse((template.parent / "target").toDirectory)
    }
    val frameRate = opts.get("--frameRate").asInstanceOf[String].toInt

    dstDir.createDirectory()
    Video(src = template, dstDir = dstDir, frameRate = frameRate).write()
  }

  val Task: ScalatagsGo.Task = ScalatagsGo.Task(Doc, Cmd, Description, go)
}
