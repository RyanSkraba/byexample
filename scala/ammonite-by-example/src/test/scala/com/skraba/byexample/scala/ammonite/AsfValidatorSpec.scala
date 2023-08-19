package com.skraba.byexample.scala.ammonite

import java.io.StringReader
import java.util.Properties
import scala.Console._
import scala.collection.mutable
import scala.io.AnsiColor.{BOLD, RESET}
import scala.jdk.CollectionConverters.PropertiesHasAsScala
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

    /** Helper to extract the environment from the given help text.
      * @param args
      *   The arguments to use in the script, in addition to "help" and
      *   "--plain" (to avoid ANSI colouring).
      * @return
      *   A map containing the environment for the script as received from the
      *   arguments, potentially a config file, and the defaults.
      */
    def extractEnvFromHelp(args: String*): mutable.Map[String, String] = {
      val txtHelp =
        help((Seq("--plain") ++ args): _*).split("Environment:\n\n", 2)
      txtHelp should have size 2
      val asProps = new Properties()
      asProps.load(new StringReader(txtHelp(1)))
      asProps.asScala
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

    it("should print the environment with flink defaults") {
      val env = extractEnvFromHelp()
      env("key") shouldBe "flink"
      env("svnDir") should endWith("working/apache/asf-svn/flink-dev-dist")
      env("svnUrl") shouldBe "https://dist.apache.org/repos/dist/dev/flink/"
    }

    it(
      "should print the environment with custom values from the command line"
    ) {
      val env = extractEnvFromHelp("--key", "avro")
      env("key") shouldBe "avro"
      env("svnDir") should endWith("working/apache/asf-svn/flink-dev-dist")
      env("svnUrl") shouldBe "https://dist.apache.org/repos/dist/dev/flink/"
    }

    it("should print the environment with custom values from a config file") {
      val cfgFile = Tmp / "config.properties"
      cfgFile.toFile.writeAll(
        "key=iceberg\n",
        "svnUrl=https://dist.apache.org/repos/dist/dev/iceberg/\n"
      )

      val env = extractEnvFromHelp("--cfgFile", cfgFile.toString)
      env("key") shouldBe "iceberg"
      env("svnDir") should endWith("working/apache/asf-svn/flink-dev-dist")
      env("svnUrl") shouldBe "https://dist.apache.org/repos/dist/dev/iceberg/"

      // The key from the command line is prioritized, then the file, then the default
      val env2 =
        extractEnvFromHelp("--cfgFile", cfgFile.toString, "--key", "pekko")
      env2("key") shouldBe "pekko"
      env2("svnDir") should endWith("working/apache/asf-svn/flink-dev-dist")
      env2("svnUrl") shouldBe "https://dist.apache.org/repos/dist/dev/iceberg/"
    }

  }
}
