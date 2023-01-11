#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import ammonite.ops._
import mainargs.{Flag, arg, main}

import scala.io.AnsiColor._

// ==========================================================================
// This is how you would add artifacts coming from the local maven repository

interp.repositories() ++= {
  // Get the local Maven repository.
  val localM2: Path = Option(sys.props("maven.repo.local"))
    .map(Path(_))
    .getOrElse(home / ".m2" / "repository")

  Seq(coursierapi.MavenRepository.of(localM2.toIO.toURI.toString))
}

@
import $ivy.`com.skraba.byexample:scala-by-example:0.0.1-SNAPSHOT`

// ==========================================================================
// Top level variables available to the script

val Cli = s"${GREEN}ammonite_example.sc$RESET"

// ==========================================================================
// Main tasks

@arg(doc = "Print help to the console.")
@main
def help(): Unit = {
  println(s"""$BOLD$Cli - Demonstrate how to script with Ammonite
             |
             |$CYAN    argTest$RESET: Show how ammonite arguments are used
             |$CYAN  githubApi$RESET: Get contribution info from the GitHub API
             |
             |Usage:
             |
             | $Cli ${CYAN}argTest$RESET [USER] [GREETING]
             | $Cli ${CYAN}githubApi$RESET USER
             |""".stripMargin)
}

/** @see
  *   [[https://index.scala-lang.org/com-lihaoyi/mainargs]]
  */
@arg(doc = "Test arguments and defaults")
@main
def argTest(
    @arg(doc = "The user running the script, or current user if not present")
    user: String = sys.env("USER"),
    @arg(doc = "A string value")
    greeting: Option[String] = None,
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {
  println(s"""$YELLOW${greeting.getOrElse("Hello")}, $BOLD$user$RESET""")
  if (verbose.value) {
    println(s"""$RESET
         |Try running these commands:
         |$Cli argTest
         |$Cli argTest Me
         |$Cli argTest --verbose Me
         |$Cli argTest Me Hey
         |$Cli argTest Me "Hello there" Invalid
         |$Cli argTest --greeting Yo
         |
         |Actual arguments
         |$BOLD$MAGENTA     user: $RESET$user
         |$BOLD$MAGENTA greeting: $RESET$greeting
         |""".stripMargin)
  }
}

@arg(doc = "Save the contribution JSON from the GitHub API to a file")
@main
def githubApi(
    @arg(doc = "GitHub user to fetch contribution information")
    user: String,
    @arg(doc = "The destination file to save the JSON")
    dstFile: String = "/tmp/github_contributions.json",
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {
  val token = %%("gh", "auth", "token")(pwd)
  val contributions = requests.post(
    url = "https://api.github.com/graphql",
    headers = Seq(
      ("Authorization", s"Bearer ${token.out.lines.head.trim}"),
      ("Content-Type", "application/json")
    ),
    data = s"""{"query":"query($$userName:String!) {
              |  user(login: $$userName) {
              |    contributionsCollection {
              |      contributionCalendar {
              |        totalContributions
              |        weeks {
              |          contributionDays {
              |            contributionCount
              |            date
              |          }
              |        }
              |      }
              |    }
              |  }
              |}",
              |"variables":{"userName":"$user"}}""".stripMargin
      .replace('\n', ' ')
  )

  write.over(Path(dstFile), contributions.text())
  if (verbose.value) {
    println(contributions.text())
  }
  println(s"${GREEN}Writing to $BOLD$dstFile$RESET")
}
