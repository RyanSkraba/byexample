package com.skraba.byexample.scalatags.countdown

import com.skraba.byexample.scalatags.ScalatagsGo

import java.nio.file.{Files, StandardCopyOption}
import scala.reflect.io.{Directory, File}
import scala.sys.process.stringSeqToProcess
import scala.util.matching.Regex

/** Command-line driver for generating a countdown video frame by frame based on an SVG template, and which can be
  * turned to images and assembled into a video.
  *
  * If the --dstVideo option is enabled, this attempts to make system calls to construct the video:
  *
  * {{{
  * # Use inkscape to convert an svg file to a png file:
  * inkscape -w 1920 -h 1080 "$svg" --export-filename "${filename%.svg}.png"
  * # Use ffmpeg to convert a series of png files to a video:
  * ffmpeg -framerate 30 -pattern_type glob -i '/tmp/countdown/timer*.png' -c:a copy -shortest -c:v libx264 \
  *     -pix_fmt yuv420p /tmp/timer.mp4
  * }}}
  *
  * If you wanted to do a quick test run to see the animation in low resolution and relatively fast timing:
  *
  * {{{
  * byexample_go_scalatags countdown --dstDir /tmp/countdown/ --dstVideo /tmp/countdown/timer.mp4 \
  *       --frameRate 10 --duration 30 --mild 25 --strong 20 timer.svg
  * }}}
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
    |  --warmup=N     The seconds to generate before the countdown [default: 2]
    |  --duration=N   The number of seconds for the countdown [default: 300]
    |  --mild=N       The number of seconds at the end of the countdown that
    |                 indicate a mild warning [default: 60]
    |  --strong=N     The number of seconds at the end of the countdown that
    |                 indicate a strong warning [default: 20]
    |  --cooldown=N   The seconds to generate after the countdown [default: 30]
    |  --dstVideo=F   If present, attempts to use system calls to inkscape and
    |                 ffmpeg to generate the video
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
    * @param duration
    *   The number of seconds that the countdown should last (GO!)
    * @param warningMild
    *   The number of seconds relative to the end of the duration that should be considered a mild time warning.
    * @param warningStrong
    *   The number of seconds relative to the end of the duration that should be considered a strong time warning.
    * @param cooldown
    *   The number of seconds after the countdown (Please stop now)
    */
  case class Video(
      src: File,
      dstDir: Directory,
      frameRate: Int = 30,
      warmup: Double = 2,
      duration: Double = 5 * 60,
      warningMild: Double = 60,
      warningStrong: Double = 20,
      cooldown: Double = 10
  ) {

    /** The total number of frames that the video will include */
    val framesTotal: Int = frameRate * (warmup + duration + cooldown).toInt

    /** The width and height of the template. */
    val (dx, dy) = (1920, 1080)

    // val inXml: Elem = XML.loadString(inString)

    /** The contents of the template file */
    val inString: String = src.slurp()

    private lazy val execCache = collection.mutable.Map[String, File]()

    private def execInkscape(f: Int, src: String, dst: String): Unit = {
      Seq("inkscape", "-w", "1920", "-h", "1080", src, "--export-filename", dst).!!
      if ((f + 1) % frameRate == 0) {
        if (f / frameRate % 10 == 0) print(f / frameRate)
        else print("x")
      } else print(".")
    }

    private def execFfmpeg(dst: File): Unit = {
      println(".")
      Seq(
        "ffmpeg",
        "-framerate",
        frameRate.toString,
        "-pattern_type",
        "glob",
        "-i",
        dstDir.toString + "/*.png",
        "-c:a",
        "copy",
        "-shortest",
        "-c:v",
        "libx264",
        "-pix_fmt",
        "yuv420p",
        dst.toString
      ).!!
    }

    def write(dstVideo: Option[File]): Unit = {
      for (f <- 0 to framesTotal) {
        val fileBase = src.stripExtension + f".${f / frameRate}%05d.${f % frameRate}%05d"
        val dstSvg: File = (dstDir / (fileBase + "." + src.extension)).toFile

        // Always write the file contents
        val contents = Frame(this, f).contents()
        dstSvg.writeAll(contents)

        // If the destination video is being written, do the png conversion
        if (dstVideo.nonEmpty) {
          val dst = dstDir / File(fileBase + ".png")
          // See if this frame already has an identical file
          val cached = execCache.getOrElseUpdate(
            contents, {
              // If it doesn't, then use Inkscape to create it.
              execInkscape(f, dstSvg.toString, dst.toString)
              dst
            }
          )
          if (cached != dst) {
            // If the frame does exist already, then just make a copy.
            print("o")
            Files.copy(cached.jfile.toPath, dst.jfile.toPath, StandardCopyOption.REPLACE_EXISTING)
          }
        }
      }
      dstVideo.foreach(execFfmpeg)
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
    val timeBounded: Double = v.duration min time max 0

    /** The bounded time represented as a floating point number between 0 and 1. */
    val fraction: Double = timeBounded / v.duration

    /** The time of the task represented as M:SS */
    val timeRemaining: String =
      f"${(v.duration - timeBounded).toInt / 60}%01d:${(v.duration - timeBounded).toInt % 60}%02d"

    /** The center and size of the progress pie. */
    val (cx, cy, r) = (v.dx / 2, v.dy / 2, 890 / 2)

    private val CountdownLayerRegex: Regex = raw"""(?s).*(<g.*?\bid="Countdown".*?>)""".r
    private val CountdownMildLayerRegex: Regex = raw"""(?s).*(<g.*?\bid="CountdownMild".*?>)""".r
    private val CountdownStrongLayerRegex: Regex = raw"""(?s).*(<g.*?\bid="CountdownStrong".*?>)""".r

    private val PathRegex: Regex = raw"""(?s).*(<path.*?\bid="progress".*?/>)""".r

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

    private def modifyLayer(in: String, re: Regex, opacity: Double): String = {
      val m = re.findFirstMatchIn(in).get
      val modified = {
        if (opacity == 0) m.group(1).replaceAll(raw"\bdisplay:\w+\b", "display:none")
        else if (opacity == 1) m.group(1).replaceAll(raw"\bdisplay:\w+\b", "display:inline")
        else m.group(1).replaceAll(raw"\bdisplay:\w+\b", s"display:inline;opacity:$opacity")
      }
      m.before(1) + modified + m.after(1)
    }

    private def flash(time: Double, cycle: Double, start: Double, end: Double): Double = {
      val cycleTime = time % cycle
      val mid = start / 2 + end / 2
      (if (cycleTime >= start && cycleTime <= mid) 1 - (cycleTime - start) / (end - start) * 2
       else if (cycleTime >= mid && cycleTime <= end) (cycleTime - mid) / (end - start) * 2
       else 1d) min 1d max 0d
    }

    private def fadeOut(time: Double, fade: Double): Double = {
      (1 - time / fade) min 1d max 0d
    }

    def contents(): String = {
      // Replace the time in the string
      val withTime = v.inString.replace("$:$$", timeRemaining)

      // Find the path representing the progress of the talk and use the match to replace the path
      val withProgress = {
        val m = PathRegex.findFirstMatchIn(withTime).get
        "" + m.before(1) + m.group(1).replaceAll(raw"""(?s)\bd=".*?"""", s"""d="${progressPath()}"""") + m.after(1)
      }

      // Style the text according to the warnings
      if (time >= v.duration - v.warningStrong - 1) {
        val timeWarned = time - v.duration + v.warningStrong + 1

        // A strong warning turns red
        val withoutCountdown = modifyLayer(withProgress, CountdownLayerRegex, 0)
        val withoutCountdownMild = modifyLayer(withoutCountdown, CountdownMildLayerRegex, fadeOut(timeWarned, 1))

        // And flashes faster during the last half
        val opacity =
          if (time >= v.duration - v.warningStrong / 2) flash(timeWarned - 0.5, 1, 0.4, 0.8)
          else 1
        modifyLayer(withoutCountdownMild, CountdownStrongLayerRegex, opacity)

      } else if (time >= v.duration - v.warningMild - 1) {
        // Mild warning just changes the colour
        val timeWarned = time - v.duration + v.warningMild + 1
        val withoutCountdown = modifyLayer(withProgress, CountdownLayerRegex, fadeOut(timeWarned, 2))
        modifyLayer(withoutCountdown, CountdownMildLayerRegex, 1)
      } else withProgress
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
    val warmup = opts.get("--warmup").asInstanceOf[String].toDouble
    val duration = opts.get("--duration").asInstanceOf[String].toDouble
    val mild = opts.get("--mild").asInstanceOf[String].toDouble
    val strong = opts.get("--strong").asInstanceOf[String].toDouble
    val cooldown = opts.get("--cooldown").asInstanceOf[String].toDouble
    val dstVideo = Option(opts.get("--dstVideo").asInstanceOf[String]).map(File.apply(_))

    dstDir.createDirectory()

    Video(
      src = template,
      dstDir = dstDir,
      frameRate = frameRate,
      warmup = warmup,
      duration = duration,
      warningMild = mild,
      warningStrong = strong,
      cooldown = cooldown
    ).write(dstVideo)
  }

  val Task: ScalatagsGo.Task = ScalatagsGo.Task(Doc, Cmd, Description, go)
}
