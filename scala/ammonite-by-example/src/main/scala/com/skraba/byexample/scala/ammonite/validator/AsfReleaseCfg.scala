package com.skraba.byexample.scala.ammonite.validator

import com.tinfoiled.docopt4s.AnsiConsole
import os.Path

import java.util.Properties
import scala.jdk.CollectionConverters.PropertiesHasAsScala

/** Settings that can be used for this tool. This is setup so that properties are expected to be given in the properties
  * file but can be overridden on the ammonite command line.
  *
  * @param propsFile
  *   A file to read all of the settings from. All of the settings in this file can be overridden in this constructor.
  */
case class AsfReleaseCfg(
    propsFile: Option[os.Path] = None,
    top: Option[String] = None,
    incubation: Option[Boolean] = None,
    version: Option[String] = None,
    rc: Option[String] = None,
    buildBaseDir: Option[String] = None,
    buildGithubDir: Option[String] = None,
    buildSvnDir: Option[String] = None,
    buildNexusDir: Option[String] = None,
    githubRepo: Option[String] = None,
    githubRcTag: Option[String] = None,
    githubRcCommit: Option[String] = None,
    svnUrl: Option[String] = None,
    svnBaseDir: Option[String] = None,
    svnDir: Option[String] = None,
    svnRcRevision: Option[String] = None,
    nexusStaging: Option[String] = None
) {

  import AsfReleaseCfg._

  private val props: Map[String, String] = propsFile
    .orElse(sys.env.get("ASF_VALIDATOR_PROPERTIES").map(os.Path(_)))
    .map { cfg =>
      val tmp: Properties = new Properties()
      tmp.load(cfg.getInputStream)
      tmp
    }
    .getOrElse(new Properties())
    .asScala
    .toMap

  lazy val Top: Setting[String] =
    Setting.str(top, props, "top", "A project tag, often the bare project name or main github repo.")(DefaultTop)
  lazy val Incubation: Setting[Boolean] =
    Setting.bool(incubation, props, "incubation", "If this is a project in the ASF incubator.")(d = false)
  lazy val Version: Setting[String] =
    Setting.str(version, props, "version", "The version of the release being validated.")(DefaultVersion)
  lazy val Rc: Setting[String] = Setting.str(rc, props, "rc", "The release version being validated.")(DefaultRc)

  lazy val BuildBaseDir: Setting[Path] =
    Setting.path(os.root / "tmp", buildBaseDir, props, "buildBaseDir", "")(
      os.temp.dir(prefix = s"validate-$Top", deleteOnExit = false)
    )
  lazy val BuildGithubDir: Setting[Path] =
    Setting.path(BuildBaseDir.get, buildGithubDir, props, "buildGithubDir", "")(BuildBaseDir.get / "github")
  lazy val BuildSvnDir: Setting[Path] =
    Setting.path(BuildBaseDir.get, buildSvnDir, props, "buildSvnDir", "")(BuildBaseDir.get / "svn")
  lazy val BuildNexusDir: Setting[Path] =
    Setting.path(BuildBaseDir.get, buildNexusDir, props, "buildNexusDir", "")(BuildBaseDir.get / "nexus")

  lazy val GithubRepo: Setting[String] =
    Setting.str(githubRepo, props, "githubRepo", "The github repo containing the source code to be validated.")(
      s"apache/$Top"
    )
  lazy val GithubRcTag: Setting[String] =
    Setting.str(
      githubRcTag,
      props,
      "githubRcTag",
      "The tag in the github repo corresponding to the source being validated."
    )(
      s"$Version-$Rc"
    )
  lazy val GithubRcCommit: Setting[String] = Setting.str(
    githubRcCommit,
    props,
    "githubRcCommit",
    "The commit in the github repo corresponding to the source being validated."
  )()

  lazy val SvnUrl: Setting[String] = Setting.str(svnUrl, props, "svnUrl", "")(
    if (Incubation.get)
      s"https://dist.apache.org/repos/dist/dev/incubator/$Top/"
    else s"https://dist.apache.org/repos/dist/dev/$Top/"
  )
  lazy val SvnBaseDir: Setting[os.Path] =
    Setting.path(os.pwd, svnBaseDir, props, "svnBaseDir", "")(
      os.home / "working" / "apache" / "asf-svn" / s"$Top-dev-dist"
    )
  lazy val SvnDir: Setting[os.Path] =
    Setting.path(SvnBaseDir.get, svnDir, props, "svnDir", "")(SvnBaseDir.get)
  lazy val SvnRcRevision: Setting[String] = Setting.str(svnRcRevision, props, "svnRcRevision", "")()

  lazy val NexusStaging: Setting[String] = Setting.str(nexusStaging, props, "nexusStaging", "")()

  lazy val VoteRequestUrl: Setting[String] = Setting.str(None, props, "voteRequestUrl", "")()
  lazy val VoteResponseUrl: Setting[String] = Setting.str(None, props, "voteResponseUrl", "")()
  lazy val VoteResponse: Setting[String] = Setting.str(None, props, "voteResponse", "")()

  def properties(out: AnsiConsole): String =
    s"""${out.bold("Environment:")}
       |
       |${Setting.properties(out, Top, Incubation, Version, Rc)}
       |
       |${Setting.properties(out, BuildBaseDir, BuildGithubDir, BuildSvnDir, BuildNexusDir)}
       |
       |${Setting.properties(out, GithubRepo, GithubRcTag, GithubRcCommit)}
       |
       |${Setting.properties(out, SvnUrl, SvnBaseDir, SvnDir, SvnRcRevision)}
       |
       |${Setting.properties(out, NexusStaging)}
       |
       |${Setting.properties(out, VoteRequestUrl, VoteResponseUrl, VoteResponse)}
       |""".stripMargin
}

object AsfReleaseCfg {
  val DefaultTop: String = "flink"
  val DefaultVersion: String = "1.0.0"
  val DefaultRc: String = "RC1"
}
