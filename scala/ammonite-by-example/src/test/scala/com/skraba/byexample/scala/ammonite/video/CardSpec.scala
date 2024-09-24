package com.skraba.byexample.scala.ammonite.video

import com.skraba.byexample.scala.ammonite.OsPathScalaRelectIOConverters._
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.reflect.io.Directory

/** Test the [[Card]] class. */
class CardSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** A local temporary directory for test file storage. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Delete temporary resources after the script. */
  override protected def afterAll(): Unit =
    try { Tmp.deleteRecursively() }
    catch { case ex: Exception => ex.printStackTrace() }

  describe("Create a basic svg file using the card") {
    it("should create the files as needed") {
      val flag = Card(
        genSvg = _ => CardSpec.SvgFlag,
        filename = "canada_flag",
        dirSvg = Tmp / "svg",
        dirPng = Tmp / "png",
        dirMp4 = Tmp / "mp4"
      )

      // Files are created lazily as they are accessed in the card
      (Tmp / "svg").jfile shouldNot exist
      (Tmp / "png").jfile shouldNot exist
      (Tmp / "mp4").jfile shouldNot exist

      // The SVG file
      flag.svg.dst
      (Tmp / "svg").jfile should exist
      (Tmp / "png").jfile shouldNot exist
      (Tmp / "mp4").jfile shouldNot exist
      (Tmp / "svg" / "canada_flag.svg").jfile should exist

      // The SVG file
      flag.png.dst
      (Tmp / "svg").jfile should exist
      (Tmp / "png").jfile should exist
      (Tmp / "mp4").jfile shouldNot exist
      (Tmp / "svg" / "canada_flag.svg").jfile should exist
      (Tmp / "png" / "canada_flag.png").jfile should exist

      val ff = Ffmpeg(cmdLog = Ffmpeg.cmdLogToFile(Tmp / "out.log"))
      flag.mp4(ff, 25).dst
      Ffmpeg.log(ff).last shouldBe s"ffmpeg -loop 1 -t 5 -i $Tmp/png/canada_flag.png -f lavfi " +
        "-i anullsrc=channel_layout=stereo:sample_rate=96000 -c:v libx264 -pix_fmt yuv420p " +
        "-vf \"scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2\" " +
        s"-c:a aac -b:a 128k -shortest -y $Tmp/mp4/canada_flag.mp4"
      (Tmp / "svg").jfile should exist
      (Tmp / "png").jfile should exist
      (Tmp / "mp4").jfile should exist
      (Tmp / "svg" / "canada_flag.svg").jfile should exist
      (Tmp / "png" / "canada_flag.png").jfile should exist
      (Tmp / "mp4" / "canada_flag.mp4").jfile should exist

      flag.mp4(ff, 30).dst
      Ffmpeg.log(ff).last shouldBe s"ffmpeg -framerate 30 -loop 1 -t 5 -i $Tmp/png/canada_flag.png -f lavfi " +
        "-i anullsrc=channel_layout=stereo:sample_rate=96000 -c:v libx264 -pix_fmt yuv420p " +
        "-vf \"scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2\" " +
        s"-c:a aac -b:a 128k -shortest -y $Tmp/mp4/canada_flag.30fps.mp4"
      (Tmp / "svg").jfile should exist
      (Tmp / "png").jfile should exist
      (Tmp / "mp4").jfile should exist
      (Tmp / "svg" / "canada_flag.svg").jfile should exist
      (Tmp / "png" / "canada_flag.png").jfile should exist
      (Tmp / "mp4" / "canada_flag.mp4").jfile should exist
      (Tmp / "mp4" / "canada_flag.30fps.mp4").jfile should exist
    }
  }
}

object CardSpec {

  /** SVG containing the canada flag. */
  val SvgFlag: String = """<svg xmlns="http://www.w3.org/2000/svg" width="1200" height="600" viewBox="0 0 9600 4800">
    | <!-- https://upload.wikimedia.org/wikipedia/commons/c/cf/Flag_of_Canada.svg -->
    |	<title>Flag of Canada</title>
    |	<path fill="#f00" d="m0 0h2400l99 99h4602l99-99h2400v4800h-2400l-99-99h-4602l-99 99H0z"/>
    |	<path fill="#fff"
    |     d="m2400 0h4800v4800h-4800zm2490 4430-45-863a95 95 0 0 1 111-98l859 151-116-320a65 65 0 0
    |        1 20-73l941-762-212-99a65 65 0 0 1-34-79l186-572-542 115a65 65 0 0 1-73-38l-105-247-423
    |        454a65 65 0 0 1-111-57l204-1052-327 189a65 65 0 0 1-91-27l-332-652-332 652a65 65 0 0
    |        1-91 27l-327-189 204 1052a65 65 0 0 1-111 57l-423-454-105 247a65 65 0 0 1-73 38l-542-115
    |        186 572a65 65 0 0 1-34 79l-212 99 941 762a65 65 0 0 1 20 73l-116 320 859-151a95 95 0 0
    |        1 111 98l-45 863z"/>
    |</svg>""".stripMargin

  /** @param t
    *   A time value where the rectangle starts in the upper left at zero and moves to the lower left at 1 second and
    *   back at 2 second
    * @return
    *   An svg with a red rectangle moving from the upper right to lower left as time passes from 0 to 1.
    */
  def svg(t: Double, dx: Double = 1920, dy: Double = 1080, pixelDx: Double = 50): String = {
    val it = t.toLong
    val dt = if (it % 2 == 0) t - it else 1 + it - t
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="$dx" height="$dy" viewBox="0 0 $dx $dy">
      |	<rect fill="#fee" width="$dx" height="$dy"/>
      |	<rect fill="#f00" x="${dt * (dx - pixelDx)}" y="${dt * (dy - pixelDx)}" width="10" height="10"/>
      |</svg>""".stripMargin
  }
}
