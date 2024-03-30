// #!/usr/bin/env amm
import scala.io.AnsiColor._

// ==========================================================================

/** This class finds and loads artifacts from the local machine, which may not have already been published to Maven. If
  * the artifact is published, this can be simpler:
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
  * Otherwise, this class can be copied into your script to load compiled resources from this maven project.
  *
  * @param a
  *   The artifact id (also the maven module name by convention)
  * @param g
  *   The group id to load (com.skraba.byexample)
  * @param v
  *   The version of the artifact to load.
  */
def load(
    a: String,
    g: String = "com.skraba.byexample",
    v: String = "0.0.1-SNAPSHOT"
): Unit = {

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
