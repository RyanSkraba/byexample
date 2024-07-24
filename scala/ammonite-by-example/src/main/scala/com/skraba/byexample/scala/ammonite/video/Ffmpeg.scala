package com.skraba.byexample.scala.ammonite.video

import com.skraba.docoptcli.AnsiConsole

/** Encapsulates file operations using the ffmpeg tool for video operations on a file. This was tested with;
  *
  * {{{
  * # ffmpeg -version
  * ffmpeg version 6.1.1 Copyright (c) 2000-2023 the FFmpeg developers
  * built with gcc 14 (GCC)
  * configuration: --prefix=/usr --bindir=/usr/bin --datadir=/usr/share/ffmpeg --docdir=/usr/share/doc/ffmpeg --incdir=/usr/include/ffmpeg --libdir=/usr/lib64 --mandir=/usr/share/man --arch=x86_64 --optflags='-O2 -flto=auto -ffat-lto-objects -fexceptions -g -grecord-gcc-switches -pipe -Wall -Wno-complain-wrong-lang -Werror=format-security -Wp,-U_FORTIFY_SOURCE,-D_FORTIFY_SOURCE=3 -Wp,-D_GLIBCXX_ASSERTIONS -specs=/usr/lib/rpm/redhat/redhat-hardened-cc1 -fstack-protector-strong -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1 -m64 -march=x86-64 -mtune=generic -fasynchronous-unwind-tables -fstack-clash-protection -fcf-protection -fno-omit-frame-pointer -mno-omit-leaf-frame-pointer' --extra-ldflags='-Wl,-z,relro -Wl,--as-needed -Wl,-z,pack-relative-relocs -Wl,-z,now -specs=/usr/lib/rpm/redhat/redhat-hardened-ld-errors -specs=/usr/lib/rpm/redhat/redhat-hardened-ld -specs=/usr/lib/rpm/redhat/redhat-annobin-cc1 -Wl,--build-id=sha1 ' --extra-cflags=' -I/usr/include/rav1e' --enable-libopencore-amrnb --enable-libopencore-amrwb --enable-libvo-amrwbenc --enable-version3 --enable-bzlib --enable-chromaprint --disable-crystalhd --enable-fontconfig --enable-frei0r --enable-gcrypt --enable-gnutls --enable-ladspa --enable-lcms2 --enable-libaom --enable-libdav1d --enable-libass --enable-libbluray --enable-libbs2b --enable-libcodec2 --enable-libcdio --enable-libdrm --enable-libjack --enable-libjxl --enable-libfreetype --enable-libfribidi --enable-libgsm --enable-libharfbuzz --enable-libilbc --enable-libmp3lame --enable-libmysofa --enable-nvenc --enable-openal --enable-opencl --enable-opengl --enable-libopenh264 --enable-libopenjpeg --enable-libopenmpt --enable-libopus --enable-libpulse --enable-libplacebo --enable-librsvg --enable-librav1e --enable-librubberband --enable-libsmbclient --enable-version3 --enable-libsnappy --enable-libsoxr --enable-libspeex --enable-libsrt --enable-libssh --enable-libsvtav1 --enable-libtesseract --enable-libtheora --enable-libtwolame --enable-libvorbis --enable-libv4l2 --enable-libvidstab --enable-libvmaf --enable-version3 --enable-vapoursynth --enable-libvpx --enable-vulkan --enable-libshaderc --enable-libwebp --enable-libx264 --enable-libx265 --enable-libxvid --enable-libxml2 --enable-libzimg --enable-libzmq --enable-libzvbi --enable-lv2 --enable-avfilter --enable-libmodplug --enable-postproc --enable-pthreads --disable-static --enable-shared --enable-gpl --disable-debug --disable-stripping --shlibdir=/usr/lib64 --enable-lto --enable-libvpl --enable-runtime-cpudetect
  * libavutil      58. 29.100 / 58. 29.100
  * libavcodec     60. 31.102 / 60. 31.102
  * libavformat    60. 16.100 / 60. 16.100
  * libavdevice    60.  3.100 / 60.  3.100
  * libavfilter     9. 12.100 /  9. 12.100
  * libswscale      7.  5.100 /  7.  5.100
  * libswresample   4. 12.100 /  4. 12.100
  * libpostproc    57.  3.100 / 57.  3.100
  * }}}
  *
  * @param mp4
  *   An MP4 file being created or investigated. Depending on the command, this might be the input file (such as
  *   [[trim]]), or the output file (such as [[concat]]).
  * @param cmdLog
  *   A callback that can be used to log Ffmpeg or other system commands that were executed.
  * @param cmdResult
  *   If this instance was generated by an os.Proc command, the results of that command (or None if not applicable)
  * @param cmdQuietStderr
  *   ffmeg normally sends a lot of output to STDERR. Setting this to true calms the output significantly.
  * @param out
  *   A console helper to assist with providing output.
  * @see
  *   https://ottverse.com/ffprobe-comprehensive-tutorial-with-examples/ '''Good ffprobe tutorial'''
  * @see
  *   https://github.com/Hong-Bo/hands-on-ffmpeg
  */

case class Ffmpeg(
    mp4: os.Path = os.pwd / "video.mp4",
    cmdLog: String => Any = _ => (),
    cmdResult: Option[os.CommandResult] = None,
    cmdQuietStderr: Boolean = true,
    out: AnsiConsole = AnsiConsole()
) {

  import Ffmpeg._

  /** Probe the video file for data in JSON format.
    * {{{
    *  ffprobe -v quiet -print_format json -show_format -show_streams input.mp4
    * }}}
    */
  lazy val streamInfo: ujson.Obj = ujson
    .read(
      osProc(
        "ffprobe",
        Seq("-v", "quiet"),
        Seq("-print_format", "json"),
        Seq("-show_format", "-show_streams"),
        mp4
      ).out.text
    )
    .obj

  /** Information about the first audio stream in the mp4 file. */
  lazy val audio0Stream: ujson.Obj = streamInfo("streams").arr.find(_.obj("codec_type").str == "audio").get.obj

  /** Information about the first video stream in the mp4 file. */
  lazy val video0Stream: ujson.Obj = streamInfo("streams").arr.find(_.obj("codec_type").str == "video").get.obj

  /** The frame rate as a rational number, like 25/1 for 25 frames per second. */
  lazy val rFrameRate: (Int, Int) = {
    val fraction = video0Stream("r_frame_rate").str.split("/")
    (fraction.head.toInt, fraction(1).toInt)
  }

  /** The frame rate as a rational number, like 1/12800 for a timestamp of 12800 units per second. */
  lazy val rTimeBase: (Int, Int) = {
    val fraction = video0Stream("time_base").str.split("/")
    (fraction.head.toInt, fraction(1).toInt)
  }

  /** Gets a chunk of frame information from the stream.
    * {{{
    * ffprobe -v quiet -print_format json -show_frames  -select_streams v:0 -read_intervals "%+#1" input.mp4
    * }}}
    *
    * Also:
    * {{{
    * ffprobe -loglevel error -select_streams v:0 -show_entries packet=pts_time,flags -of csv=print_section=0 input.mp4
    * }}}
    *
    * The `-read_intervals` flat takes the following formats:
    *   - `0:10%0:20,1:10%1:20` : Two ten second intervals
    *     - `%100` : The first 100 seconds of the stream
    *     - `0:10%+0:20` : From 10s to 30s
    *
    * @param start
    *   The starting time in seconds that should be investigated. This can be a total number of seconds like "125", or
    *   in minutes and seconds like "2:05", or in packets like "#125". If prefixed with a '+', this is relative to the
    *   last time specified.
    * @param numFrames
    *   The number of frames to fetch from that time.
    * @return
    *   The frame information
    * @see
    *   https://www.ffmpeg.org/ffprobe.html#Main-options '''-read_options format'''
    */
  def frameChunkInfo(start: String, end: String, keyFramesOnly: Boolean = false): ujson.Arr = ujson
    .read(
      osProc(
        "ffprobe",
        Seq("-v", "quiet"),
        if (keyFramesOnly) Seq("-skip_frame", "nokey") else Seq(),
        Seq("-print_format", "json"),
        Seq("-show_frames", "-select_streams", "v:0"),
        Seq("-read_intervals", f"$start%%$end"),
        mp4
      ).out.text
    )
    .obj("frames")
    .arr

  /** The frame rate as a double. For more precision, the [[rFrameRate]] can be used, which as a rational number can
    * avoid issues with floating point precision.
    *
    * Also:
    * {{{
    * ffprobe -v error -select_streams v:0 -show_entries stream=r_frame_rate -of csv=p=0 input.mp4
    * }}}
    *   - `-v error` to limit the verbosity to error messages only
    *   - `-select_streams v:0` selects the first video stream
    *   - `-show_entries stream=r_frame_rate` picks the frame_rate entry
    *   - `-of csv=p=0` sets the simplified output format
    *
    * The output will look like 25/1 or 30/1 (where 25/1 is usually the default for libx264).
    */
  lazy val frameRate: Double = rFrameRate._1.toDouble / rFrameRate._2

  /** Also:
    * {{{
    * ffprobe -v error -show_entries format=duration -of csv=p=0 input.mp4
    * }}}
    *
    * The output be a double
    *
    * @return
    *   the framerate of the MP4 file.
    */
  lazy val duration: Double = video0Stream("duration").str.toDouble

  /** @param newMp4
    *   A different mp4 file to consider.
    * @return
    *   A copy of this class containing the given file.
    */

  def mp4(newMp4: os.Path): Ffmpeg = copy(mp4 = newMp4)

  /** A helper to run os.proc, useful for printing and redirecting output
    * @param args
    *   arguments to run as a system command
    * @return
    *   the command results
    */
  def osProc(args: os.Shellable*): os.CommandResult = {
    cmdLog(args.flatMap(_.value).map(arg => if (arg.contains(" ")) s"\"$arg\"" else arg).mkString(" "))
    os.proc(args: _*).call(os.pwd, stderr = if (cmdQuietStderr) os.Pipe else os.Inherit)
  }

  /** Given a single image, creates the destination [[mp4]] of duration N
    *
    * {{{
    * ffmpeg -framerate 25 -loop 1 -t 5 -i input.png \
    *     -f lavfi -i anullsrc=channel_layout=stereo:sample_rate=96000 \
    *     -c:v libx264 -pix_fmt yuv420p \
    *     -vf scale=1920:1080:force_original_aspect_ratio=decrease,pad=1920:1080:(ow-iw)/2:(oh-ih)/2 \
    *     -c:a aac -b:a 128k -shortest -y output.mp4
    * }}}
    *
    * @param srcPng
    *   The input image file to read
    * @param duration
    *   Length of the video in seconds
    * @param audioRate
    *   Audio sampling rate (in silence).
    * @param dx
    *   Width of the video
    * @param dy
    *   Height of the video
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def pngToMp4(
      srcPng: os.Path,
      duration: Int = 5,
      audioRate: Int = 96000,
      dx: Int = 1920,
      dy: Int = 1080,
      frameRate: Int = 25
  ): Ffmpeg = {
    // The arguments that may override the default frame rate
    val frameRateArg: Seq[String] = if (frameRate == 25) Seq("-framerate", frameRate.toString) else Seq.empty[String]

    val cmd = osProc(
      "ffmpeg",
      frameRateArg,
      // Loop the input image indefinitely for a certain number of seconds
      Seq("-loop", "1", "-t", duration, "-i", srcPng).map(_.toString),
      // Include a silent audio stream
      Seq("-f", "lavfi", "-i", s"anullsrc=channel_layout=stereo:sample_rate=$audioRate"),
      // The video codec and pixel format
      Seq("-c:v", "libx264", "-pix_fmt", "yuv420p"),
      // Scale, center and fit to this size
      Seq("-vf", s"scale=$dx:$dy:force_original_aspect_ratio=decrease,pad=$dx:$dy:(ow-iw)/2:(oh-ih)/2"),
      // The audio codec and parameters
      Seq("-c:a", "aac", "-b:a", "128k"),
      // Make the video as long as the shortest input stream (in this case the looped image)
      "-shortest",
      // Overwrite and output
      Seq("-y", mp4).map(_.toString)
    )
    copy(cmdResult = Some(cmd))
  }

  /** Given a sequence of PNG files where each is meant to serve as a frame, creates the destination [[mp4]].
    *
    * {{{
    * ffmpeg -framerate 30 -pattern_type glob -i 'input*.png' -c:a copy -shortest -c:v libx264 \
    *        -pix_fmt yuv420p output.mp4
    * }}}
    *
    * @param inPngGlob
    *   A glob specifying the sequence of input images to use as frames for the video
    * @param loop
    *   The number of times to loop the clip
    * @param audioRate
    *   Audio sampling rate (in silence).
    * @param dx
    *   Width of the video
    * @param dy
    *   Height of the video
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def pngsToMp4(
      inPngGlob: String,
      loop: Int = 1,
      audioRate: Int = 96000,
      dx: Int = 1920,
      dy: Int = 1080,
      frameRate: Int = 25
  ): Ffmpeg = {
    // The arguments that may override the default frame rate
    val frameRateArg: Seq[String] = if (frameRate == 25) Seq("-framerate", frameRate.toString) else Seq.empty[String]

    val cmd = osProc(
      "ffmpeg",
      frameRateArg,
      if (loop > 1) Seq("-stream_loop", (loop - 1).toString) else Seq.empty[String],
      // Use a pattern glob to select the incoming frames
      Seq("-pattern_type", "glob", "-i", inPngGlob),
      // Include a silent audio stream
      Seq("-f", "lavfi", "-i", s"anullsrc=channel_layout=stereo:sample_rate=$audioRate"),
      // The video codec and pixel format
      Seq("-c:v", "libx264", "-pix_fmt", "yuv420p"),
      // Scale, center and fit to this size
      Seq("-vf", s"scale=$dx:$dy:force_original_aspect_ratio=decrease,pad=$dx:$dy:(ow-iw)/2:(oh-ih)/2"),
      // The audio codec and parameters
      Seq("-c:a", "aac", "-b:a", "128k"),
      // Make the video as long as the shortest input stream (in this case the frames)
      "-shortest",
      // Overwrite and output
      Seq("-y", mp4).map(_.toString)
    )
    copy(cmdResult = Some(cmd))
  }

  /** Annotate the source [[mp4]] video with time and frame information in the upper left corner.
    *
    * {{{
    * ffmpeg -i input.mp4 \
    *    -vf "drawtext=text='%{pts\:hms} %{n}':fontsize=24:fontcolor=white:x=10:y=10:box=1:boxcolor=black@0.5"
    *    -c:a copy output.mp4
    * }}}
    * @param dstMp4
    *   The mp4 file to create.
    * @param fontFile
    *   Optionally, the file to use in the font
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def annotate(
      dstMp4: os.Path,
      fontFile: Option[os.Path] = None
  ): Ffmpeg = {
    val cmd = osProc(
      "ffmpeg",
      Seq("-i", mp4).map(_.toString),
      // Use a video filter to draw text over the frames
      "-vf",
      Seq(
        // Optionally provide a font file
        fontFile.toSeq.map(f => s"fontfile=$f"),
        Seq(
          "text='%{pts\\:hms} [%{n}]'",
          "fontsize=24",
          "fontcolor=white",
          "x=10",
          "y=10",
          "boxborderw=3",
          "box=1",
          "boxcolor=black@0.5"
        )
      ).flatten.mkString("drawtext=", ":", ""),
      // Copy the audio
      Seq("-c:a", "copy"),
      // Overwrite and output
      Seq("-y", dstMp4).map(_.toString)
    )
    mp4(dstMp4).copy(cmdResult = Some(cmd))
  }

  /** Generate an audio track over top of the source [[mp4]] video.
    *
    * {{{
    * # For a sweep:
    * ffmpeg -i input.mp4 \
    *   -i "sin(2*PI*t*(220+t*220)/5):s=96000:d=5" \
    *   -filter_complex "aloop=loop=-1:size=441000" \
    *   -c:v copy -map 0:v:0 -map 1:a:0 -shortest output.mp4
    *
    * }}}
    *
    * @param dstMp4
    *   The mp4 file to create with the new audio.
    * @param dt
    *   The length of the sound clip to generate (it should be repeated)
    * @param audioRate
    *   Audio sampling rate to generate
    * @param aevalsrc
    *   Optionally, an `ffmpeg` aevalsrc expression, such as [[aevalsrcSweep]]
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def replaceGeneratedAudio(
      dstMp4: os.Path,
      dt: Int = 5,
      audioRate: Int = 96000,
      aevalsrc: String = ""
  ): Ffmpeg = {
    val cmd = osProc(
      "ffmpeg",
      Seq("-i", mp4).map(_.toString),
      // The generated sound
      Seq("-f", "lavfi", "-i", s"aevalsrc=${if (aevalsrc.nonEmpty) aevalsrc else aevalsrcSweep()}:s=$audioRate:d=$dt"),
      // Repeat the sound and map it to an output
      Seq("-filter_complex", s"[1:a]aloop=loop=-1:size=${audioRate * dt}[aout]"),
      // Copy the video and map the audio to the output
      Seq("-c:v", "copy", "-c:a", "aac", "-ac", 2, "-ar", audioRate).map(_.toString),
      Seq("-map", "0:v:0", "-map", "[aout]"),
      "-shortest",
      // Overwrite and output
      Seq("-y", dstMp4).map(_.toString)
    )
    mp4(dstMp4).copy(cmdResult = Some(cmd))
  }

  /** Given MP4s with the same encoding, concatenate them into a single destination [[mp4]] video.
    * @param concatTxt
    *   The text file to create for the concatenation
    * @param srcMp4s
    *   A list of input mp4s to be concatenated
    * @param reencode
    *   Whether to use fast copying or slow reencoding behaviour.
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def concat(
      concatTxt: os.Path,
      srcMp4s: Seq[os.Path],
      reencode: Boolean = false
  ): Ffmpeg = {
    os.write.over(
      concatTxt,
      srcMp4s.map(_.toString.replaceAll(" ", "\\\\ ")).map(mp4 => s"file file:$mp4\n").mkString
    )
    val cmd = osProc(
      "ffmpeg",
      "-f",
      "concat",
      "-safe",
      0,
      "-i",
      concatTxt,
      if (reencode) Seq.empty[String] else Seq("-c", "copy"),
      "-y",
      mp4
    )
    copy(cmdResult = Some(cmd))
  }

  /** Trims the source [[mp4]] to the specified times (if present)
    * @param dstMp4
    *   The destination file to create with the contents of the trimmed file.
    * @param start
    *   If present, the start time of the source.
    * @param end
    *   If present, the end time of the source.
    * @param reencode
    *   Whether to use fast copying or slow reencoding behaviour.
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def trim(
      dstMp4: os.Path,
      start: Option[String] = None,
      end: Option[String] = None,
      reencode: Boolean
  ): Ffmpeg = {
    val cmd = osProc(
      "ffmpeg",
      // The start time, either in seconds or HH:MM:SS
      start.toSeq.filterNot(_.isEmpty).flatMap(Seq("-ss", _)),
      // The end time, either in seconds or HH:MM:SS
      end.toSeq.filterNot(_.isEmpty).flatMap(Seq("-to", _)),
      Seq("-i", mp4).map(_.toString),
      if (reencode) Seq.empty[String] else Seq("-c", "copy"),
      // Overwrite and output
      Seq("-y", dstMp4).map(_.toString)
    )
    mp4(dstMp4).copy(cmdResult = Some(cmd))
  }

  /** Fades the source [[mp4]] for a certain number of start and end seconds.
    * @param dstMp4
    *   The destination file to create with the new audio.
    * @param start
    *   If non-zero, the number of seconds to apply a fade in.
    * @param end
    *   If non-zero, the number of seconds to apply a fade out.
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def fade(
      dstMp4: os.Path,
      start: Double = 0,
      end: Double = 0
  ): Ffmpeg = {
    val fadeFilter: String = Seq(
      if (start == 0) None else Some(s"afade=t=in:st=0:d=$start"),
      if (end == 0) None else Some(s"afade=t=out:st=${duration - end}:d=$end")
    ).flatten.mkString(",")
    val cmd = osProc(
      "ffmpeg",
      Seq("-i", mp4).map(_.toString),
      Seq("-af", fadeFilter),
      Seq("-c:v", "copy"),
      // Overwrite and output
      Seq("-y", dstMp4).map(_.toString)
    )
    mp4(dstMp4).copy(cmdResult = Some(cmd))
  }

  /** Normalizes the audio of the source [[mp4]].
    * @param dstMp4
    *   The destination file to create with the normalized audio
    * @return
    *   An instance containing the destination MP4 and the command results.
    */
  def normalizeMp4(dstMp4: os.Path): os.CommandResult = osProc(
    "ffmpeg",
    Seq("-i", mp4).map(_.toString),
    Seq("-filter:a", "loudnorm=I=-15 [f] ; [f] afftdn=nr=97 [g]; [g] highpass=f=100"),
    // Audio codec
    Seq("-c:a", "aac"),
    // Overwrite and output
    Seq("-y", dstMp4).map(_.toString)
  )
}

object Ffmpeg {

  /** @param loFrequency
    *   The low frequency in hertz
    * @param hiFrequency
    *   The high frequency in hertz
    * @param dt
    *   The length of the sound clip to generate
    * @return
    *   An `ffmpeg` aevalsrc expression that sweeps from the low frequency to the high frequency in the time required.
    */
  def aevalsrcSweep(loFrequency: Int = 220, hiFrequency: Int = 440, dt: Int = 5): String =
    s"sin(2*PI*t*($loFrequency+t*${hiFrequency - loFrequency})/$dt)"

  /** @param loFrequency
    *   The low frequency in hertz
    * @param hiFrequency
    *   The high frequency in hertz
    * @param dt
    *   The length of the sound clip to generate
    * @return
    *   An `ffmpeg` aevalsrc expression that generates a wave from the low frequency to the high frequency in the time
    *   required.
    */
  def aevalsrcSine(loFrequency: Int = 220, hiFrequency: Int = 440, dt: Int = 5): String =
    s"sin(2*PI*t*(${(loFrequency + hiFrequency) / 2})-${(hiFrequency - loFrequency) / 2}*cos(2*PI*t/$dt))"
}