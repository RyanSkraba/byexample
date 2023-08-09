package com.skraba.byexample.scala.ammonite.gtd

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the getting_things_done.sc script. */
class GtdScriptSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File =
    AmmoniteScriptSpecBase.find("/getting_things_done.sc")

  describe("Running the getting_things_done.sc help") {

    /** Helper to run getting_things_done.sc help successfully with some initial
      * checks
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
        s"$BOLD${GREEN}getting_things_done.sc$RESET - Let's get things done!"
      )
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "getting_things_done.sc - Let's get things done!"
      )
    }
  }
}
