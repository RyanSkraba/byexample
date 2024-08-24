package com.skraba.byexample.scala.ammonite.gtd

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase

import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the getting_things_done.sc script. */
class GtdScriptSpec extends AmmoniteScriptSpecBase("/getting_things_done.sc") {

  /** A file with a basic scenario. */
  val Basic: File = (Tmp / "basic_gtd.md").createFile()
  Basic.writeAll(
    """# Weekly Status
      !## 2023-01-02
      !
      !|To Do|Notes|
      !|-----|-----|
      !| A | One |
      !""".stripMargin('!')
  )

  describe(s"Running $ScriptName help") {
    it("should print a useful message") {
      // with helpers
      val ansiHelp = help()
      ansiHelp should startWith(s"$BOLD${GREEN}getting_things_done.sc$RESET - Let's get things done!")
      help("--verbose") shouldBe ansiHelp
      help("--plain") should startWith(
        "getting_things_done.sc - Let's get things done!"
      )
    }
  }

  describe(s"Running $ScriptName clean") {

    /** Helper to run getting_things_done.sc help successfully with some initial checks
      *
      * @param args
      *   Additional arguments to the script
      * @return
      *   stdout
      */
    def clean(args: String*): String = {
      sys.props("GTD_TAG") = "BASIC"
      sys.props("BASIC_STATUS_REPO") = Tmp.toString
      sys.props("BASIC_STATUS_FILE") = Basic.toString
      withScript2("clean")(args: _*) { case (result, stdout, stderr) =>
        stderr shouldBe empty
        result shouldBe true
        stdout
      }
    }

    it("should clean the basic scenario") {
      clean()
      Basic.slurp() shouldBe
        """Weekly Status
          !==============================================================================
          !
          !2023-01-02
          !------------------------------------------------------------------------------
          !
          !| To Do | Notes |
          !|-------|-------|
          !| A     | One   |
          !""".stripMargin('!')
    }
  }
}
