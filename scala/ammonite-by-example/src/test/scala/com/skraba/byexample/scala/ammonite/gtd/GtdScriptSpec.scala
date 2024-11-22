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
      Output.writeAll("""# Weekly Status
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
      link("http://example.fr/custom", "Custom", "19000101")
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
          || 🔶TODO | [Custom][19000101-1]       |
          || 🔶TODO | [Fourth][<TODAY>-2]       |
          |
          |[19000101-1]: http://example.fr/custom "Custom"
          |[<TODAY>-1]: http://example.com "Example link"
          |[<TODAY>-2]: http://example.fr/fourth "Fourth"
          |[<TODAY>-3]: http://example.net/third "Third"
          |""".stripMargin
    }
  }

  describe(s"Running $ScriptName pr") {

    val Output = (Tmp / "pr.md").toFile

    def pr(args: String*): (String, String) = {
      sys.props("GTD_TAG") = "BASIC"
      sys.props("BASIC_STATUS_REPO") = Tmp.toString
      sys.props("BASIC_STATUS_FILE") = Output.toString
      val stdout = withTaskSuccess()("pr")(args: _*)
      (stdout, Output.slurp().replaceAll(TodayLink, "<TODAY>"))
    }

    it("should create a doc with PRs") {
      // Create a basic file with a known top date
      val output = Output.toFile
      output.writeAll("""# Weekly Status
                        |## 2023-01-01
                        |""".stripMargin)

      val (stdout, _) = pr("avro", "1234", "4321", "Describe the PR", "STATUS", "--plain")
      stdout shouldBe """Commit:
                        |  git -C <TMP> add pr.md &&
                        |      git -C <TMP> difftool --staged
                        |  git -C <TMP> add pr.md &&
                        |      git -C <TMP> commit -m "feat(status): PR AVRO-4321 Describe the PR"
                        |
                        |""".stripMargin

      pr("beam", "123", "321", "Beam fixes", "TODO")
      pr("beam", "0", "321", "Beam fixes 1", "TODO")
      pr("beam", "", "321", "Beam fixes 2", "TODO")
      pr("beam", "123", "0", "Beam fixes 3", "TODO")
      pr("beam", "123", "", "Beam fixes 4", "TODO")
      pr("parquet", "234", "432", "Parquet things", "TODO")
      pr("flink", "345", "543", "Flink fixes", "DONE")
      pr("flink", "8", "1111", "More flink fixes", "MERGED")
      pr("flink", "11118", "9", "Even more flink fixes", "OPENED")
      val (_, gtd) = pr("flink-web", "98", "89", "Another", "FIXED")
      gtd shouldBe
        """Weekly Status
          |==============================================================================
          |
          |2023-01-01
          |------------------------------------------------------------------------------
          |
          || To Do     | Notes 🟢🔵🔶🟥⤴️🕒                                                |
          ||-----------|-------------------------------------------------------------------|
          || 🔶Avro    | **[AVRO-4321]**:[apache/avro#1234] Describe the PR `STATUS`       |
          || 🔶Beam    | **[BEAM-321]**:[apache/beam#123] Beam fixes `TODO`                |
          || 🔶Beam    | **[BEAM-321]** Beam fixes 1 `TODO`                                |
          || 🔶Beam    | **[BEAM-321]** Beam fixes 2 `TODO`                                |
          || 🔶Beam    | [apache/beam#123] Beam fixes 3 `TODO`                             |
          || 🔶Beam    | [apache/beam#123] Beam fixes 4 `TODO`                             |
          || 🔶Parquet | **[PARQUET-432]**:[apache/parquet-mr#234] Parquet things `TODO`   |
          || 🟢Flink   | **[FLINK-543]**:[apache/flink#345] Flink fixes `DONE`             |
          || 🟢Flink   | **[FLINK-1111]**:[apache/flink#8] More flink fixes `MERGED`       |
          || 🔶Flink   | **[FLINK-9]**:[apache/flink#11118] Even more flink fixes `OPENED` |
          || 🟢Flink   | **[FLINK-89]**:[apache/flink-web#98] Another `FIXED`              |
          |
          |References
          |==============================================================================
          |
          |[AVRO-4321]: https://issues.apache.org/jira/browse/AVRO-4321 "Describe the PR"
          |[apache/avro#1234]: https://github.com/apache/avro/pull/1234 "Describe the PR"
          |[BEAM-321]: https://issues.apache.org/jira/browse/BEAM-321 "Beam fixes"
          |[apache/beam#123]: https://github.com/apache/beam/pull/123 "Beam fixes"
          |[FLINK-9]: https://issues.apache.org/jira/browse/FLINK-9 "Even more flink fixes"
          |[FLINK-89]: https://issues.apache.org/jira/browse/FLINK-89 "Another"
          |[FLINK-543]: https://issues.apache.org/jira/browse/FLINK-543 "Flink fixes"
          |[FLINK-1111]: https://issues.apache.org/jira/browse/FLINK-1111 "More flink fixes"
          |[apache/flink#8]: https://github.com/apache/flink/pull/8 "More flink fixes"
          |[apache/flink#345]: https://github.com/apache/flink/pull/345 "Flink fixes"
          |[apache/flink#11118]: https://github.com/apache/flink/pull/11118 "Even more flink fixes"
          |[apache/flink-web#98]: https://github.com/apache/flink-web/pull/98 "Another"
          |[PARQUET-432]: https://issues.apache.org/jira/browse/PARQUET-432 "Parquet things"
          |[apache/parquet-mr#234]: https://github.com/apache/parquet-mr/pull/234 "Parquet things"
          |""".stripMargin
    }
  }
}
