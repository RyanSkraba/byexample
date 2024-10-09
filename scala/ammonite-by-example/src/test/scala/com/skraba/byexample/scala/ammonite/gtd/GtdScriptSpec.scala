package com.skraba.byexample.scala.ammonite.gtd

import com.skraba.byexample.scala.ammonite.AmmoniteScriptSpecBase
import java.time.Instant
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

  /** The default linkRef used for links generated today. */
  val TodayLink: String = GettingThingsDone.Pattern.format(Instant.now).replaceAll("-", "")

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

    val Output = (Tmp / "link.md").toFile

    def link(args: String*): (String, String) = {
      sys.props("GTD_TAG") = "BASIC"
      sys.props("BASIC_STATUS_REPO") = Tmp.toString
      sys.props("BASIC_STATUS_FILE") = Output.toString
      val stdout = withTaskSuccess()("link")(args: _*)
      (stdout, Output.slurp().replaceAll(TodayLink, "<TODAY>"))
    }

    it("should create a doc with a link") {
      // Create a basic file with a known top date
      val output = (Tmp / "link.md").toFile
      output.writeAll("""# Weekly Status
          |## 2023-01-01
          |""".stripMargin)

      val (stdout, gtd) = link("http://example.com", "Example link", "--plain")
      stdout shouldBe """Commit:
                        |  git -C <TMP> add link.md &&
                        |      git -C <TMP> difftool --staged
                        |  git -C <TMP> add link.md &&
                        |      git -C <TMP> commit -m "feat(status): Add 'Example link' to the weekly status"
                        |
                        |""".stripMargin

      gtd shouldBe
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

      link("http://example.org/second", "Second")
      val (_, gtd2) = link("http://example.net/third", "Third")
      gtd2 shouldBe
        """Weekly Status
          |==============================================================================
          |
          |2023-01-01
          |------------------------------------------------------------------------------
          |
          || To Do  | Notes 🟢🔵🔶🟥⤴️🕒         |
          ||--------|----------------------------|
          || 🔶TODO | [Example link][<TODAY>-1] |
          || 🔶TODO | [Second][<TODAY>-2]       |
          || 🔶TODO | [Third][<TODAY>-3]        |
          |
          |[<TODAY>-1]: http://example.com "Example link"
          |[<TODAY>-2]: http://example.org/second "Second"
          |[<TODAY>-3]: http://example.net/third "Third"
          |""".stripMargin

      // Rewrite the file but throw away one of the link references
      Output.writeAll(Output.lines().filter(!_.contains("example.org")).mkString("\n"))
      val (_, gtd3) = link("http://example.fr/fourth", "Fourth")
      // The new link reference is inserted in the right place
      gtd3 shouldBe
        """Weekly Status
          |==============================================================================
          |
          |2023-01-01
          |------------------------------------------------------------------------------
          |
          || To Do  | Notes 🟢🔵🔶🟥⤴️🕒         |
          ||--------|----------------------------|
          || 🔶TODO | [Example link][<TODAY>-1] |
          || 🔶TODO | [Second][<TODAY>-2]       |
          || 🔶TODO | [Third][<TODAY>-3]        |
          || 🔶TODO | [Fourth][<TODAY>-2]       |
          |
          |[<TODAY>-1]: http://example.com "Example link"
          |[<TODAY>-2]: http://example.fr/fourth "Fourth"
          |[<TODAY>-3]: http://example.net/third "Third"
          |""".stripMargin
    }
  }

}
