package com.skraba.byexample.scala.ammonite

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the file_renamer.sc script. */
class AsfValidatorSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File =
    AmmoniteScriptSpecBase.find("/asf_validator.sc")

  describe("Running the asf_validator.sc help") {

    /** Helper to run git_checker.sc help successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def help(args: String*): String = {
      val arguments: Seq[String] = Seq("help") ++ args
      withScript(arguments: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout
      }
    }

    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(
        s"$BOLD${GREEN}asf_validator.sc$RESET - Validating a release for the ASF"
      )
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "asf_validator.sc - Validating a release for the ASF"
      )
    }
  }
}
