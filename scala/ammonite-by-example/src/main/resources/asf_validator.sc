#!/usr/bin/env amm

/** Validating a release for the ASF. */
import mainargs.{ParserForClass, arg, main}

import java.util.Properties
import scala.jdk.CollectionConverters.PropertiesHasAsScala

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

/** Configuration parameters that can be used for this tool.
  *
  * @param cfgFile
  *   A file to read all of the parameters from
  * @param key
  *   Override the key (top-level project name)
  * @param svnUrl
  *   The SVN url that contains the *dev* artifacts for the toplevel project
  * @param svnDir
  *   The local directory where SVN is checked out
  */
case class AsfReleaseConfig(
    cfgFile: Option[os.Path] = None,
    key: Option[String] = None,
    svnUrl: Option[String] = None,
    svnDir: Option[os.Path] = None
) {

  import AsfReleaseConfig._

  val baseCfg: Map[String, String] = cfgFile
    .map { cfg =>
      val tmp: Properties = new Properties()
      tmp.load(cfg.getInputStream)
      tmp
    }
    .getOrElse(new Properties())
    .asScala
    .toMap

  lazy val Key: String = key.orElse(baseCfg.get("key")).getOrElse(DefaultKey)
  lazy val SvnUrl: String =
    svnUrl.orElse(baseCfg.get("svnUrl")).getOrElse(s"https://dist.apache.org/repos/dist/dev/$Key/")
  lazy val SvnDir: os.Path = svnDir
    .orElse(baseCfg.get("svnDir").map(os.Path(_)))
    .getOrElse(os.home / "working" / "apache" / "asf-svn" / s"$Key-dev-dist")
}

object AsfReleaseConfig {
  val DefaultKey: String = "flink"
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
       |${cfg.left("key")}=${cfg.right(asf.Key)}
       |${cfg.left("svnUrl")}=${cfg.right(asf.SvnUrl)}
       |${cfg.left("svnDir")}=${cfg.right(asf.SvnDir)}
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
