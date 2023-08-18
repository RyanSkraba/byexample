#!/usr/bin/env amm

/** Validating a release for the ASF. */
import mainargs.{ParserForClass, arg, main}

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("scala-by-example")
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ColourCfg

// ==========================================================================
// Top level variables available to the script

case class AsfReleaseConfig(
    cfgFile: Option[os.Path] = None,
    key: String = "flink",
    svn: String = "https://dist.apache.org/repos/dist/dev/flink/",
    svnDir: os.Path = os.home/ "working"/"apache"/"asf-svn"/"flink-dev-dist"
) {
  lazy val Key: String = key
  lazy val Svn: String = svn
  lazy val SvnDir: os.Path = svnDir
}
implicit def asfReleaseConfigParser: ParserForClass[AsfReleaseConfig] =
  ParserForClass[AsfReleaseConfig]

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(asf: AsfReleaseConfig, cfg: ColourCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "asf_validator.sc"
  println(
    cfg.helpHeader(
      cli,
      "Validating a release for the ASF",
      "help" -> "Help, usage and release validation resources."
    )
  )

  // Usage examples
  println(cfg.helpUse(cli, "test", "[--verbose]"))
  println()

  println(
    s"""${cfg.bold("Environment:")}
       |
       | ${cfg.left("key")} ${cfg.right(asf.Key)}
       | ${cfg.left("svn")} ${cfg.right(asf.Svn)}
       | ${cfg.left("svnDir")} ${cfg.right(asf.SvnDir)}
       |
       |""".stripMargin
  )
}

/** Print out some useful information about the environment variables used to
  * validate ASF releases.
  *
  * @param cfg
  *   Colour configuration for the output
  */
@main
def svn(asf: AsfReleaseConfig, cfg: ColourCfg): Unit = {
  os.proc("svn", "update").call(asf.SvnDir)
}
