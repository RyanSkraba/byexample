#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import mainargs.{Flag, arg, main}
import scala.io.AnsiColor._
import scala.util._

// ==========================================================================

/** This class finds and loads artifacts from the local machine, which may not
 * have already been published to Maven. If the artifact is published, this can
 * be simpler:
 *
 * {{{
 * import $ivy.`com.skraba.byexample:scala-by-example:0.0.1`
 * }}}
 *
 * If the artifact is on the local maven repository, it's more complicated:
 *
 * {{{
 * interp.repositories() ++=
 *     Seq(coursierapi.MavenRepository.of(LocalM2.toIO.toURI.toString))
 * @
 * // The "@" causes a second stage of compilation with the repo directory
 * import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`
 * }}}
 *
 * Otherwise, this class can be copied into your script to load compiled
 * resources from this maven project.
 *
 * @param g
 *   The group id to load (com.skraba.byexample)
 * @param a
 *   The artifact id (also the maven module name by convention)
 * @param v
 *   The version of the artifact to load.
 */
case class LocalImportUtil(g: String, a: String, v: String) {

  /** Point to the local maven repo. */
  val localM2: os.Path = Option(sys.props("maven.repo.local"))
    .map(os.Path(_))
    .getOrElse(os.home / ".m2" / "repository")

  /** The name of the jar, if it exists. */
  val Jar: String = s"$a-$v.jar"

  /** The root of the maven project to load from. */
  val DepRoot = LazyList
    .iterate(os.Path(sourcecode.File()))(_ / os.up)
    .dropWhile(_.baseName != "byexample")
    .head / "scala" / a / "target"

  // Load it from the maven path if it exists.
  if (os.exists(DepRoot / "classes"))
    interp.load.cp(DepRoot / "classes")
  else if (os.exists(DepRoot / Jar))
    interp.load.cp(DepRoot / Jar)
  else {
    // Load it directly into the classpath here though
    val repoJar =
      localM2 / "com" / "skraba" / "byexample" / a / v / Jar
    if (os.exists(repoJar)) interp.load.cp(repoJar)
    else {
      println(s"""${BOLD}${RED}Unable to load dependency
                 |
                 |This scripts depends on an artifact $Jar but wasn't able to find it.
                 |""".stripMargin)
      sys.exit(1)
    }
  }
}

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).

LocalImportUtil("com.skraba.byexample", "scala-by-example", "0.0.1-SNAPSHOT")
LocalImportUtil("com.skraba.byexample", "ammonite-by-example", "0.0.1-SNAPSHOT")

@

import com.skraba.byexample.scala.ammonite.git._
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

val Cli = AnsiColourCfg.ok("git_checker.sc")

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  val cfg = AnsiColourCfg
  println(s"""${cfg.bold(Cli)} - Do some analysis on git repositories.
             |
             | ${cfg.left("cherryPick")} : Get a status report on two branches
             |
             |Usage:
             |
             | $Cli ${cfg.left("cherryPick")} [repo] [main] [branch]
             |""".stripMargin)
}

// ==========================================================================
// Cherry pick report

@arg(doc = "Create a cherry-picking report between two branches")
@main
def releaseCherryPickPrep(
    repo: String,
    lTag: String = "main",
    rTag: String = "branch",
    statusDoc: Option[os.Path] = None,
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {

  val cfg = AnsiColourCfg
  if (verbose.value) {
    println(cfg.bold("Arguments:"))
    println(cfg.kv("      repo", repo))
    println(cfg.kv("      lTag", lTag))
    println(cfg.kv("      rTag", rTag))
    println(cfg.kv(" statusDoc", statusDoc))
    println(cfg.bold("\nGit command:"))
    println((CherryPickerReport.Cmd :+ s"$lTag...$rTag").mkString(" "))
  }

  // The current state of the git branches
  val current: CherryPickerReport =
    CherryPickerReport.fromGit(repo, lTag, rTag) match {
      case Left(status) => status
      case Right(Success(result)) =>
        println(s"Unsuccessful ${result.exitCode}")
        sys.exit(result.exitCode)
      case Right(Failure(ex)) =>
        println("Failure!")
        ex.printStackTrace()
        sys.exit(1)
    }

  // If there's a status document, merge it with the current state
  val updated = statusDoc match {
    case Some(path) if os.exists(path) =>
      CherryPickerReport.fromDoc(Header.parse(os.read(path))).update(current)
    case _ => current
  }

  val cleaned = Header.parse(updated.toDoc.build().toString)
  val txt = cleaned.build().toString
  if (verbose.value || statusDoc.isEmpty) println(txt)

  statusDoc.foreach(os.write.over(_, txt))
}

@arg(doc = "Use the GitHub API to count the open PRs on a given project")
@main
def countOpenPrs(
    @arg(doc = "The project in the form apache/avro")
    prj: String,
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {
  val prsResponse = requests.get(
    s"https://api.github.com/search/issues?q=repo:$prj%20state:open%20is:pr"
  )
  if (verbose.value) {
    println(prsResponse.text())
  }

  val prs = ujson.read(prsResponse.text())
  println(prs("total_count"))
}
