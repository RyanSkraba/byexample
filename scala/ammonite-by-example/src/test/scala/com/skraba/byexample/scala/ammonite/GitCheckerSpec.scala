package com.skraba.byexample.scala.ammonite

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the git_checker.sc script. */
class GitCheckerSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File = AmmoniteScriptSpecBase.find("/git_checker.sc")

  describe("Running the git_checker.sc help") {

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
        s"$BOLD${GREEN}git_checker.sc$RESET - Do some analysis on git repositories"
      )
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "git_checker.sc - Do some analysis on git repositories"
      )
    }
  }
}
