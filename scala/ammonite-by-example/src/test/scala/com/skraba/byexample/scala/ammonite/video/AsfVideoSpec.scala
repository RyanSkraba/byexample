package com.skraba.byexample.scala.ammonite.video

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the asf_video.sc script. */
class AsfVideoSpec extends AmmoniteScriptSpecBase("/asf_video.sc") {

  /** A file with a basic scenario. */
  describe(s"Running $ScriptName help") {

    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(s"$BOLD${GREEN}asf_video.sc$RESET - Generate files for ASF video presentations")
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith("asf_video.sc - Generate files for ASF video presentations")
    }
  }
}
