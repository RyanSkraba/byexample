package com.skraba.byexample.scala.ammonite.gtd

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase
import com.skraba.byexample.scala.ammonite.gtd.GettingThingsDone.nextWeekStart

import java.time.Instant
import scala.Console._
import scala.io.AnsiColor.{BOLD, RESET}
import scala.reflect.io.File

/** Test the getting_things_done.sc script. */
class GtdScriptSpec extends AmmoniteScriptSpecBase("/getting_things_done.sc") {

  val today: String = GettingThingsDone.Pattern.format(Instant.now).replaceAll("-", "")

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
      help("--plain") should startWith("getting_things_done.sc - Let's get things done!")
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

  describe(s"Running $ScriptName link") {
    it("should create a doc with a link") {
      // with helpers
      val output = (Tmp / "link.md").toFile
      output.writeAll("""# Weekly Status
          |## 2023-01-01
          |""".stripMargin)
      sys.props("GTD_TAG") = "BASIC"
      sys.props("BASIC_STATUS_REPO") = Tmp.toString
      sys.props("BASIC_STATUS_FILE") = output.toString
      val stdout = withTaskSuccess()("link")("http://example.com", "Example link", "--plain")
      // TODO: check STDOUT

      output.slurp().replaceAll(today, "<TODAY>") shouldBe
        """Weekly Status
          |==============================================================================
          |
          |2023-01-01
          |------------------------------------------------------------------------------
          |
          || To Do  | Notes 🟢🔵🔶🟥⤴️🕒         |
          ||--------|----------------------------|
          || 🔶TODO | [Example link][<TODAY>-1] |
          |
          |[<TODAY>-1]: http://example.com "Example link"
          |""".stripMargin

      withTaskSuccess()("link")("http://example.com/second", "Another", "--plain")
      output.slurp().replaceAll(today, "<TODAY>") shouldBe
        """Weekly Status
          |==============================================================================
          |
          |2023-01-01
          |------------------------------------------------------------------------------
          |
          || To Do  | Notes 🟢🔵🔶🟥⤴️🕒         |
          ||--------|----------------------------|
          || 🔶TODO | [Example link][<TODAY>-1] |
          || 🔶TODO | [Another][<TODAY>-2]      |
          |
          |[<TODAY>-1]: http://example.com "Example link"
          |[<TODAY>-2]: http://example.com/second "Another"
          |""".stripMargin
    }
  }

}
