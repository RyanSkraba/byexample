package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}
import com.tinfoiled.docopt4s.FsPath._

import java.nio.file.Path

/** Unit tests for [[BeautifyTask]] */
class BeautifyTaskSpec extends MultiTaskMainSpec(MarkdGo, Some(BeautifyTask)) with TmpDir {

  describe(s"Standard $MainName $TaskCmd command line help, versions and exceptions") {
    itShouldHandleVersionNoArgsAndHelpFlags()
    itShouldThrowOnUnknownOptKey()
    itShouldThrowOnIncompleteArgs()
  }

  describe(s"${Main.Name} $TaskCmd beautify basic scenario") {

    /** Generate a scenario with some files to beautify. */
    def scenario(tag: String): Path = {
      val scenario = (Tmp / tag).createDirectory()
      // A very simple file
      (scenario / "basic.md").writeAll(
        s"""# Header
           |Some text
           |""".stripMargin
      )
      // The same file as it would be beautified
      (scenario / "basic_expected.md").writeAll(
        s"""Header
           |==============================================================================
           |
           |Some text
           |""".stripMargin
      )
      scenario
    }

    it("should beautify a single file") {
      val basic = scenario("basic")

      (basic / "basic.md").slurp() shouldNot be((basic / "basic_expected.md").slurp())

      withGoMatching(TaskCmd, basic, "--dryRun") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe s"Modifying $basic/basic.md\n"
      }

      (basic / "basic.md").slurp() shouldNot be((basic / "basic_expected.md").slurp())

      withGoMatching(TaskCmd, basic / "basic.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe ""
      }

      (basic / "basic.md").slurp() shouldBe (basic / "basic_expected.md").slurp()
    }
  }
}
