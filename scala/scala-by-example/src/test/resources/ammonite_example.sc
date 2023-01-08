#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import ammonite.ops._
import mainargs.{arg, main}

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

@arg(doc = "Test arguments and defaults")
@main
def argTest(
    @arg(doc = "The user running the script, or current user if not present")
    user: String = sys.env("USER"),
    @arg(doc = "A string value")
    greeting: Option[String] = None
): Unit = {
  println(s"""${BOLD}Try running these commands:$RESET
       |
       |$Cli argTest
       |$Cli argTest Me
       |$Cli argTest Me Hello
       |$Cli argTest Me Hello Invalid
       |$Cli argTest --greeting Hey
       |$BOLD
       |Actual arguments
       |$BOLD$MAGENTA     user: $RESET$user
       |$BOLD$MAGENTA greeting: $RESET$greeting
       |""".stripMargin)
}

@arg(doc = "Save the contribution JSON from the GitHub API to a file")
@main
def githubApi(user: String): Unit = {
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

  println(contributions.text())
}

