package com.skraba.byexample.scala.ammonite

import java.io.StringReader
import java.util.Properties
import scala.Console._
import scala.collection.mutable
import scala.io.AnsiColor.{BOLD, RESET}
import scala.jdk.CollectionConverters.PropertiesHasAsScala
import scala.reflect.io.File

/** Test the asf_validator.sc script. */
class AsfValidatorSpec extends AmmoniteScriptSpecBase {

  /** The path containing ammonite scripts. */
  override val ScriptPath: File = AmmoniteScriptSpecBase.find("/asf_validator.sc")

  describe(s"Running $ScriptName help") {

    /** Helper to extract the environment from the given help text.
      * @param args
      *   The arguments to use in the script, in addition to "help" and "--plain" (to avoid ANSI colouring).
      * @return
      *   A map containing the environment for the script as received from the arguments, potentially a config file, and
      *   the defaults.
      */
    def extractEnvFromHelp(args: String*): mutable.Map[String, String] = {
      val txtHelp = help(Seq("--plain") ++ args: _*).split("Environment:\n\n", 2)
      txtHelp should have size 2
      val asProps = new Properties()
      asProps.load(new StringReader(txtHelp(1)))
      asProps.asScala
    }

    it("should print a useful message") {
      help() should startWith(s"$BOLD${GREEN}asf_validator.sc$RESET - Validating a release for the ASF")
      // This property is necessary for reproducible help  messages
      val ansiHelp = help("--buildBaseDir", "/tmp/validate")
      help("--verbose", "--buildBaseDir", "/tmp/validate") shouldBe ansiHelp
      help("--plain", "--buildBaseDir", "/tmp/validate") should startWith(
        "asf_validator.sc - Validating a release for the ASF"
      )
    }

    it("should print the environment with flink defaults") {
      val env = extractEnvFromHelp()
      env.keySet.toList.sorted shouldBe List(
        "buildBaseDir",
        "buildGithubDir",
        "buildNexusDir",
        "buildSvnDir",
        "githubRcCommit",
        "githubRcTag",
        "githubRepo",
        "incubation",
        "nexusStaging",
        "rc",
        "svnBaseDir",
        "svnDir",
        "svnRcRevision",
        "svnUrl",
        "top",
        "version"
      )

      env("buildBaseDir") should startWith("/tmp/validate-flink")
      env("buildGithubDir") shouldBe s"${env("buildBaseDir")}/github"
      env("buildNexusDir") shouldBe s"${env("buildBaseDir")}/nexus"
      env("buildSvnDir") shouldBe s"${env("buildBaseDir")}/svn"
      env("githubRcCommit") shouldBe empty
      env("githubRcTag") shouldBe "1.0.0-RC1"
      env("githubRepo") shouldBe "apache/flink"
      env("incubation") shouldBe "false"
      env("nexusStaging") shouldBe empty
      env("rc") shouldBe "RC1"
      env("svnBaseDir") should endWith("working/apache/asf-svn/flink-dev-dist")
      env("svnDir") shouldBe env("svnBaseDir")
      env("svnRcRevision") shouldBe empty
      env("svnUrl") shouldBe "https://dist.apache.org/repos/dist/dev/flink/"
      env("top") shouldBe "flink"
      env("version") shouldBe "1.0.0"
    }

    it("should print the environment with custom values from the command line") {
      val env = extractEnvFromHelp("--top", "avro")
      env("top") shouldBe "avro"
      env("svnDir") should endWith("working/apache/asf-svn/avro-dev-dist")
      env("svnUrl") shouldBe "https://dist.apache.org/repos/dist/dev/avro/"
    }

    it("should print the environment with custom values from a config file") {
      val cfgFile = Tmp / "config.properties"
      cfgFile.toFile.writeAll("top=iceberg\n", "svnUrl=https://dist.apache.org/repos/dist/release/iceberg/\n")

      val env = extractEnvFromHelp("--cfgFile", cfgFile.toString)
      env("top") shouldBe "iceberg"
      env("svnDir") should endWith("working/apache/asf-svn/iceberg-dev-dist")
      env("svnUrl") shouldBe "https://dist.apache.org/repos/dist/release/iceberg/"

      // The key from the command line is prioritized, then the file, then the default
      val env2 =
        extractEnvFromHelp("--cfgFile", cfgFile.toString, "--top", "pekko")
      env2("top") shouldBe "pekko"
      env2("svnDir") should endWith("working/apache/asf-svn/pekko-dev-dist")
      env2("svnUrl") shouldBe "https://dist.apache.org/repos/dist/release/iceberg/"
    }
  }
}
