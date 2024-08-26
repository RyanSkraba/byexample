package com.skraba.byexample.scala.ammonite

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the git_checker.sc script. */
class GitCheckerScriptSpec extends AmmoniteScriptSpecBase("/git_checker.sc") {

  describe(s"Running $ScriptName help") {
    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(s"$BOLD${GREEN}git_checker.sc$RESET - Do some analysis on git repositories")
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "git_checker.sc - Do some analysis on git repositories"
      )
    }
  }
}
