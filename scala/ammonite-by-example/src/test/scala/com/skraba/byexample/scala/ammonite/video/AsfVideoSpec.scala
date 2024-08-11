package com.skraba.byexample.scala.ammonite.video

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the asf_video.sc script. */
class AsfVideoSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File =
    AmmoniteScriptSpecBase.find("/asf_video.sc")

  /** A file with a basic scenario. */
  describe("Running the asf_video.sc help") {

    /** Helper to run getting_things_done.sc help successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def help(args: String*): String = {
      withScript2("help")(args: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout
      }
    }

    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(
        s"$BOLD${GREEN}asf_video.sc$RESET - Generate files for ASF video presentations"
      )
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith("asf_video.sc - Generate files for ASF video presentations")
    }
  }
}
