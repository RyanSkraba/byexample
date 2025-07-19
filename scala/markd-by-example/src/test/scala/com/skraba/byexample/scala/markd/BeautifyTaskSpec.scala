package com.skraba.byexample.scala.markd

import com.tinfoiled.docopt4s.testkit.{MultiTaskMainSpec, TmpDir}

import scala.reflect.io.{Directory, File}

/** Unit tests for [[BeautifyTask]] */
class BeautifyTaskSpec extends MultiTaskMainSpec(MarkdGo, Some(BeautifyTask)) with TmpDir {

  describe(s"${Main.Name} $TaskCmd command line") {

    itShouldThrowOnHelpAndVersionFlags()

    itShouldThrowOnUnknownFlag()

    itShouldThrowOnIncompleteArgs(Seq.empty)
  }

  describe(s"${Main.Name} $TaskCmd beautify basic scenario") {

    /** Generate a scenario with some files to beautify. */
    def scenario(tag: String): Directory = {
      val scenario = (Tmp / tag).createDirectory()
      // A very simple file
      File(scenario / "basic.md").writeAll(
        s"""# Header
           |Some text
           |""".stripMargin
      )
      // The same file as it would be beautified
      File(scenario / "basic_expected.md").writeAll(
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

      (basic / "basic.md").toFile.slurp() shouldNot be((basic / "basic_expected.md").toFile.slurp())

      withGoMatching(TaskCmd, basic, "--dryRun") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe s"Modifying $basic/basic.md\n"
      }

      (basic / "basic.md").toFile.slurp() shouldNot be((basic / "basic_expected.md").toFile.slurp())

      withGoMatching(TaskCmd, basic / "basic.md") { case (stdout, stderr) =>
        stderr shouldBe empty
        stdout shouldBe ""
      }

      (basic / "basic.md").toFile.slurp() shouldBe (basic / "basic_expected.md").toFile.slurp()
    }
  }
}
