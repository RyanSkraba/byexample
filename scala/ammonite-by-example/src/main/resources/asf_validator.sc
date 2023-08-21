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
  * @param top
  *   Override the key (top-level project name)
  * @param svnUrl
  *   The SVN url that contains the *dev* artifacts for the toplevel project
  * @param svnDir
  *   The local directory where SVN is checked out
  */
case class AsfReleaseConfig(
    cfgFile: Option[os.Path] = None,
    top: Option[String] = None,
    incubation: Option[Boolean] = None,
    version: Option[String] = None,
    rc: Option[String] = None,
    svnUrl: Option[String] = None,
    svnBaseDir: Option[os.Path] = None,
    svnDir: Option[os.Path] = None
) {

  import AsfReleaseConfig._

  private val props: Map[String, String] = cfgFile
    .orElse(sys.env.get("ASF_VALIDATOR_PROPERTIES").map(os.Path(_)))
    .map { cfg =>
      val tmp: Properties = new Properties()
      tmp.load(cfg.getInputStream)
      tmp
    }
    .getOrElse(new Properties())
    .asScala
    .toMap

  lazy val Top: String = top.orElse(props.get("top")).getOrElse(DefaultTop)
  lazy val Incubation: Boolean =
    incubation.orElse(props.get("incubation").map(_.toBoolean)).getOrElse(false)
  lazy val Version: String =
    version.orElse(props.get("version")).getOrElse(DefaultVersion)
  lazy val Rc: String = rc.orElse(props.get("rc")).getOrElse(DefaultRc)
  lazy val SvnUrl: String =
    svnUrl
      .orElse(props.get("svnUrl"))
      .getOrElse(
        if (Incubation)
          s"https://dist.apache.org/repos/dist/dev/incubator/$Top/"
        else s"https://dist.apache.org/repos/dist/dev/$Top/"
      )
  lazy val SvnBaseDir: os.Path = svnBaseDir
    .orElse(props.get("svnBaseDir").map(os.Path(_)))
    .getOrElse(os.home / "working" / "apache" / "asf-svn" / s"$Top-dev-dist")
  lazy val SvnDir: os.Path = svnDir
    .orElse(props.get("svnDir").map(os.Path(_)))
    .getOrElse(SvnBaseDir)

  def properties(cfg: ColourCfg): String =
    s"""${cfg.bold("Environment:")}
       |
       |; Basic info --------------------------------------------------
       |; The top level project that provides the artifact
       |${cfg.left("top")}=${cfg.right(Top)}
       |${cfg.left("incubation")}=${cfg.right(Incubation)}
       |${cfg.left("version")}=${cfg.right(Version)}
       |${cfg.left("rc")}=${cfg.right(Rc)}
       |
       |${cfg.left("svnUrl")}=${cfg.right(SvnUrl)}
       |${cfg.left("svnBaseDir")}=${cfg.right(SvnBaseDir)}
       |${cfg.left("svnDir")}=${cfg.right(SvnDir)}
       |
       |""".stripMargin
}

object AsfReleaseConfig {
  val DefaultTop: String = "flink"
  val DefaultVersion: String = "1.0.0"
  val DefaultRc: String = "RC1"
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

  println(asf.properties(cfg));
}

/** Print out some useful information about the environment variables used to
  * validate ASF releases.
  *
  * @param cfg
  *   Colour configuration for the output
  */
@main
def svn(asf: AsfReleaseConfig, cfg: ColourCfg): Unit = {
  cfg.vPrintln(asf.properties(cfg))
  if (!os.exists(asf.SvnDir)) {
    cfg.vPrintln(
      cfg.warn("Creating subversion directory:", bold = true) + cfg.warn(
        asf.SvnDir
      )
    )
    os.makeDir.all(asf.SvnDir / os.up)
    val cmd =
      os.proc("svn", "checkout", asf.SvnUrl, asf.SvnBaseDir)
        .call(asf.SvnBaseDir / os.up)
    println(cmd.out.lines().mkString("\n"))
  }
  println(
    os.proc("svn", "update").call(asf.SvnBaseDir).out.lines().mkString("\n")
  )
  println(os.proc("svn", "info").call(asf.SvnDir).out.lines().mkString("\n"))
}
