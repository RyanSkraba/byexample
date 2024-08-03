package com.skraba.byexample.scala.ammonite.video

import com.skraba.byexample.scala.ammonite.OsPathScalaRelectIOConverters._
import org.scalactic.source.Position
import org.scalatest.{BeforeAndAfterAll, Tag}
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.nio.file.StandardCopyOption
import scala.collection.mutable
import scala.reflect.io.{Directory, File}
import scala.util.Properties

/** Test the [[Ffmpeg]] class. */
class FfmpegSpec extends AnyFunSpecLike with BeforeAndAfterAll with Matchers {

  /** Create a temporary directory to use for all tests. This will be automatically cleaned up. */
  val Tmp: Directory = Directory.makeTemp(getClass.getSimpleName)

  /** Create a temporary directory that will be shared and not deleted between runs. Be careful, this can have
    * unintended side effects.
    */
  val CachedTmp: Directory = (Directory(Properties.tmpDir) / getClass.getSimpleName).createDirectory()

  /** If this is not None, retain the last [[Tmp]] here. */
  val SaveLastTmp: Option[Directory] = Some(CachedTmp / "last").map(_.toDirectory)

  /** And delete it after the tests. */
  override protected def afterAll(): Unit =
    try {
      // Optionally save the last temp directory
      SaveLastTmp.map(last => {
        last.deleteRecursively()
        java.nio.file.Files.move(
          Tmp.jfile.toPath,
          last.jfile.toPath,
          StandardCopyOption.ATOMIC_MOVE,
          StandardCopyOption.REPLACE_EXISTING
        )
      })
      // Delete the temporary directory
      Tmp.deleteRecursively()
    } catch {
      case ex: Exception =>
        ex.printStackTrace()
    }

  /** Generated sequence of frames. */
  val frames: Seq[File] = {
    val template: Card =
      Card(_ => "", "", dirSvg = CachedTmp / "src", dirPng = CachedTmp / "src", dirMp4 = CachedTmp / "src")
    for (i <- 0 to 99)
      yield template.copy(genSvg = _ => CardSpec.svg(i / 50d), filename = f"input$i%03d").png.dst.get
  }

  /** Generated sequence of frames. */
  val frames2: Seq[File] = {
    for (i <- 0 to 99) yield {
      val dst = (CachedTmp / "src" / f"input$i%03d.png").toFile
      os.makeDir.all(dst.parent)

      if (!os.exists(dst)) {
        val image = new java.awt.image.BufferedImage(1920, 1080, java.awt.image.BufferedImage.TYPE_INT_RGB)
        val g = image.createGraphics()
        g.setColor(java.awt.Color.white)
        g.fillRect(0, 0, 1920, 1080)
        g.setColor(java.awt.Color.red)
        g.fillRect(((i * (1920.0 - 100)) / 99).toInt, ((i * (1080.0 - 100)) / 99).toInt, 100, 100)
        g.dispose()
        javax.imageio.ImageIO.write(image, "png", dst.jfile)
      }

      dst
    }
  }

  /** This is a technique for disabling all of the unit tests in this spec by rewriting the `it` word that is used to
    * run the tests. If `ffmpeg` or `ffprobe` are not present, the word is replaced entirely by ignored calls.
    */
  protected class FfmpegItWord extends ItWord {

    /** Determine if the tests should be run or not. */
    val enabled: Boolean = {
      import scala.sys.process._
      try { "ffmpeg -version".! == 0 && "ffprobe -version".! == 0 }
      catch { case _: Exception => false }
    }
    override def apply(specText: String, testTags: Tag*)(testFun: => Any)(implicit pos: Position): Unit = {
      // Since we override `it`, we fall back on the equivalent `they` if the tests are enabled.
      if (enabled) they(specText)(testFun)
      else ignore(specText)()
    }
  }
  override val it = new FfmpegItWord

  describe("Create a basic movie from still images") {

    it("should create a video from a single frame") {
      val ff = Ffmpeg(mp4 = Tmp / "still.mp4", cmdLog = Ffmpeg.cmdLogToMemory())
      ff.pngToMp4(frames.head)
      Ffmpeg.log(ff).last shouldBe s"ffmpeg -loop 1 -t 5 -i ${frames.head} " +
        "-f lavfi -i anullsrc=channel_layout=stereo:sample_rate=96000 -c:v libx264 -pix_fmt yuv420p " +
        """-vf "scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2" """ +
        s"-c:a aac -b:a 128k -shortest -y $Tmp/still.mp4"
      ff.duration shouldBe 5.0
      ff.frameRate shouldBe 25.0
    }

    it("should create a 5fps video from a single frame") {
      val ff = Ffmpeg(mp4 = Tmp / "still.mp4", cmdLog = Ffmpeg.cmdLogToMemory())
      ff.pngToMp4(frames.head, frameRate = 5)
      Ffmpeg.log(ff).last shouldBe s"ffmpeg -framerate 5 -loop 1 -t 5 -i ${frames.head} " +
        "-f lavfi -i anullsrc=channel_layout=stereo:sample_rate=96000 -c:v libx264 -pix_fmt yuv420p " +
        """-vf "scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2" """ +
        s"-c:a aac -b:a 128k -shortest -y $Tmp/still.mp4"
      ff.duration shouldBe 5.0
      ff.frameRate shouldBe 5.0
    }

    describe("should create a video from multiple frames") {

      /** Basic silent animation reused in tests in this section. */
      lazy val basic: Ffmpeg = {
        val ff = Ffmpeg(mp4 = Tmp / "animated.mp4", cmdLog = Ffmpeg.cmdLogToMemory())
        ff.pngsToMp4((CachedTmp / "src" / "input").toString + "*.png", 10)
        Ffmpeg
          .log(ff)
          .last shouldBe s"ffmpeg -framerate 25 -stream_loop 9 -pattern_type glob -i $CachedTmp/src/input*.png " +
          "-f lavfi -i anullsrc=channel_layout=stereo:sample_rate=96000 -c:v libx264 -pix_fmt yuv420p " +
          """-vf "scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2" """ +
          s"-c:a aac -b:a 128k -shortest -y $Tmp/animated.mp4"
        ff
      }

      /** Basic annotated animation reused in tests in this section. */
      lazy val annotated: Ffmpeg = {
        basic.annotate(Tmp / "annotated.mp4")
        Ffmpeg.log(basic).last shouldBe s"ffmpeg -i $Tmp/animated.mp4 " +
          """-vf "drawtext=text='%{pts\:hms} [%{n}]':fontsize=24:fontcolor=white:x=10:y=10:boxborderw=3:box=1:boxcolor=black@0.5" """ +
          s"-c:a copy -y $Tmp/annotated.mp4"
        basic.mp4(Tmp / "annotated.mp4").copy(cmdLog = Ffmpeg.cmdLogToMemory())
      }

      /** Basic annotated animation with sound reused in tests in this section. */
      lazy val sound: Ffmpeg = {
        annotated.replaceGeneratedAudio(
          Tmp / "sound.mp4",
          dt = 2,
          aevalsrc = Ffmpeg.aevalsrcSine(hiFrequency = 880, dt = 2)
        )
        Ffmpeg.log(annotated).last shouldBe s"ffmpeg -i $Tmp/annotated.mp4 -f lavfi " +
          """-i "aevalsrc=sin(2*PI*t*(550)-330*cos(2*PI*t/2)):s=96000:d=2" """ +
          "-filter_complex [1:a]aloop=loop=-1:size=192000[aout] " +
          "-c:v copy -c:a aac -ac 2 -ar 96000 -map 0:v:0 -map [aout] " +
          s"-shortest -y $Tmp/sound.mp4"
        basic.mp4(Tmp / "sound.mp4").copy(cmdLog = Ffmpeg.cmdLogToMemory())
      }

      /** Frame information about the first 1000 frames of the video with sound */
      lazy val soundFrames = {
        // Get a single frame from the annotated video
        val frames = sound.frameChunkInfo("0", "+#1000")
        frames.value.size shouldBe 1000
        Ffmpeg.log(sound).last shouldBe s"ffprobe -v quiet -print_format json -show_frames -select_streams v:0 " +
          s"-read_intervals 0%+#1000 $Tmp/sound.mp4"
        frames
      }

      it("with basic silence") {
        basic.duration shouldBe 40.0
        basic.rFrameRate shouldBe (25, 1)
        basic.rTimeBase shouldBe (1, 12800)
        basic.audio0Stream.value should contain("codec_type" -> ujson.Str("audio"))
        basic.video0Stream.value should contain("codec_type" -> ujson.Str("video"))
      }

      it("with annotations") {
        annotated.duration shouldBe 40.0
        annotated.rFrameRate shouldBe (25, 1)
        annotated.rTimeBase shouldBe (1, 12800)
        annotated.audio0Stream.value should contain("codec_type" -> ujson.Str("audio"))
        annotated.video0Stream.value should contain("codec_type" -> ujson.Str("video"))
      }

      describe("when fetching the first 1000 frames") {

        it("should check the first frame for properties") {
          // Get a single frame from the annotated video
          val first: ujson.Obj = soundFrames(0).obj

          // Check out all the zero values
          val (first0, firstNon0) = first.value.keys.partition(first(_) == ujson.Num(0))
          first0.toSeq.sorted shouldBe Seq(
            "best_effort_timestamp",
            "coded_picture_number",
            "crop_bottom",
            "crop_left",
            "crop_right",
            "crop_top",
            "display_picture_number",
            "interlaced_frame",
            "pkt_dts",
            "pts",
            "repeat_pict",
            "stream_index",
            "top_field_first"
          )

          // And the non-zero values
          firstNon0.toSeq.sorted shouldBe Seq(
            "best_effort_timestamp_time",
            "chroma_location",
            "duration",
            "duration_time",
            "height",
            "key_frame",
            "media_type",
            "pict_type",
            "pix_fmt",
            "pkt_dts_time",
            "pkt_duration",
            "pkt_duration_time",
            "pkt_pos",
            "pkt_size",
            "pts_time",
            "sample_aspect_ratio",
            "side_data_list",
            "width"
          )

          // Other important frame values.
          first("best_effort_timestamp_time") shouldBe ujson.Str("0.000000")
          first("chroma_location") shouldBe ujson.Str("left")
          first("duration") shouldBe ujson.Num(512)
          first("duration_time") shouldBe ujson.Str("0.040000")
          first("height") shouldBe ujson.Num(1080)
          first("key_frame") shouldBe ujson.Num(1)
          first("media_type") shouldBe ujson.Str("video")
          first("pict_type") shouldBe ujson.Str("I")
          first("pix_fmt") shouldBe ujson.Str("yuv420p")
          first("pkt_duration") shouldBe ujson.Num(512)
          first("pkt_duration_time") shouldBe ujson.Str("0.040000")
          first("pkt_pos") shouldBe ujson.Str("48")
          // The packet size will probably depend on the contents of the frame
          // first("pkt_size") shouldBe ujson.Str("2623")
          first("pts_time") shouldBe ujson.Str("0.000000")
          first("sample_aspect_ratio") shouldBe ujson.Str("1:1")
          first("width") shouldBe ujson.Num(1920)
        }
      }

      it("when fetching a single frame") {
        val frames = sound.frameChunkInfo("0", "+#1")
        frames.value.size shouldBe 1
        Ffmpeg.log(sound).last shouldBe s"ffprobe -v quiet -print_format json -show_frames -select_streams v:0 " +
          s"-read_intervals 0%+#1 $Tmp/sound.mp4"
        val first: ujson.Obj = frames(0).obj
        val soundFirst: ujson.Obj = soundFrames(0).obj

        first.value.keySet.diff(soundFirst.value.keySet) shouldBe empty
        // For some reason, the dts isn't in the list if the number of frames is less than two
        soundFirst.value.keySet.diff(first.value.keySet) shouldBe Set("pkt_dts", "pkt_dts_time")

        for (key <- first.value.keySet.intersect(soundFirst.value.keySet))
          first(key) shouldBe soundFirst(key)
      }

      describe("with sound") {
        it("should have audio information") {
          sound.duration shouldBe 40.0
          // Some information about the generated audio
          val info = sound.audio0Stream
          info("codec_name") shouldBe ujson.Str("aac")
          info("codec_type") shouldBe ujson.Str("audio")
          info("codec_tag_string") shouldBe ujson.Str("mp4a")
          info("sample_fmt") shouldBe ujson.Str("fltp")
          info("sample_rate") shouldBe ujson.Str("96000")
          info("channels") shouldBe ujson.Num(2)
          info("channel_layout") shouldBe ujson.Str("stereo")
          info("time_base") shouldBe ujson.Str("1/96000")
          info("nb_frames") shouldBe ujson.Str("3751")
        }
      }
    }
  }
}
