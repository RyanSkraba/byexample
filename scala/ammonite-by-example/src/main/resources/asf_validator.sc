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
import com.skraba.byexample.scala.ammonite.ConsoleCfg

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
    buildBaseDir: Option[os.Path] = None,
    buildGithubDir: Option[os.Path] = None,
    buildSvnDir: Option[os.Path] = None,
    buildNexusDir: Option[os.Path] = None,
    githubRepo: Option[String] = None,
    githubRcTag: Option[String] = None,
    githubRcCommit: Option[String] = None,
    svnUrl: Option[String] = None,
    svnBaseDir: Option[os.Path] = None,
    svnDir: Option[os.Path] = None,
    svnRcRevision: Option[String] = None,
    nexusStaging: Option[String] = None
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

  lazy val BuildBaseDir: os.Path = buildBaseDir
    .orElse(props.get("buildBaseDir").map(os.FilePath(_)))
    .getOrElse(os.temp.dir(prefix = s"validate-$Top", deleteOnExit = false))
    .resolveFrom(os.root / "tmp")
  lazy val BuildGithubDir: os.Path = buildGithubDir
    .orElse(props.get("buildGithubDir").map(os.FilePath(_)))
    .getOrElse(BuildBaseDir / "github")
    .resolveFrom(BuildBaseDir)
  lazy val BuildSvnDir: os.Path = buildSvnDir
    .orElse(props.get("buildSvnDir").map(os.FilePath(_)))
    .getOrElse(BuildBaseDir / "svn")
    .resolveFrom(BuildBaseDir)
  lazy val BuildNexusDir: os.Path = buildNexusDir
    .orElse(props.get("buildNexusDir").map(os.FilePath(_)))
    .getOrElse(BuildBaseDir / "nexus")
    .resolveFrom(BuildBaseDir)

  lazy val GithubRepo: String =
    githubRepo.orElse(props.get("githubRepo")).getOrElse(s"apache/$Top")
  lazy val GithubRcTag: String =
    githubRcTag.orElse(props.get("githubRcTag")).getOrElse(s"$Version-$Rc")
  lazy val GithubRcCommit: String =
    githubRcCommit.orElse(props.get("githubRcCommit")).getOrElse("")

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
  lazy val SvnDir = svnDir
    .orElse(props.get("svnDir").map(os.FilePath(_)))
    .getOrElse(SvnBaseDir)
    .resolveFrom(SvnBaseDir)
  lazy val SvnRcRevision: String =
    svnRcRevision.orElse(props.get("svnRcRevision")).getOrElse("")

  lazy val NexusStaging: String =
    nexusStaging.orElse(props.get("nexusStaging")).getOrElse("")

  def properties(cfg: ConsoleCfg): String =
    s"""${cfg.bold("Environment:")}
       |
       |${cfg.left("top")}=${cfg.right(Top)}
       |${cfg.left("incubation")}=${cfg.right(Incubation)}
       |${cfg.left("version")}=${cfg.right(Version)}
       |${cfg.left("rc")}=${cfg.right(Rc)}
       |
       |${cfg.left("buildBaseDir")}=${cfg.right(BuildBaseDir)}
       |${cfg.left("buildGithubDir")}=${cfg.right(BuildGithubDir)}
       |${cfg.left("buildSvnDir")}=${cfg.right(BuildSvnDir)}
       |${cfg.left("buildNexusDir")}=${cfg.right(BuildNexusDir)}
       |
       |${cfg.left("githubRepo")}=${cfg.right(GithubRepo)}
       |${cfg.left("githubRcTag")}=${cfg.right(GithubRcTag)}
       |${cfg.left("githubRcCommit")}=${cfg.right(GithubRcCommit)}
       |
       |${cfg.left("svnUrl")}=${cfg.right(SvnUrl)}
       |${cfg.left("svnBaseDir")}=${cfg.right(SvnBaseDir)}
       |${cfg.left("svnDir")}=${cfg.right(SvnDir)}
       |${cfg.left("svnRcRevision")}=${cfg.right(SvnRcRevision)}
       |
       |${cfg.left("nexusStaging")}=${cfg.right(NexusStaging)}
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
def help(asf: AsfReleaseConfig, cfg: ConsoleCfg): Unit = {
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
def svn(asf: AsfReleaseConfig, cfg: ConsoleCfg): Unit = {
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
