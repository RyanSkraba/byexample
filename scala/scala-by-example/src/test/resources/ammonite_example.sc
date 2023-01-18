#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import ammonite.ops._
import mainargs.{Flag, arg, main}
import ujson.Obj

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, LocalDate}
import scala.collection.SortedMap
import scala.io.AnsiColor._
import scala.util._

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
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

val YyyyMmDd = DateTimeFormatter.ofPattern("yyyy-MM-dd")
val MonthDay = DateTimeFormatter.ofPattern("MMM d")
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
             |$CYAN githubJson$RESET: Parse and use the github JSON with ujson
             |$CYAN    gitExec$RESET: Run a system call (git) and get the results
             |
             |Usage:
             |
             | $Cli ${CYAN}argTest$RESET [USER] [GREETING] [--verbose]
             | $Cli ${CYAN}githubApi$RESET USER [DSTFILE] [--verbose]
             | $Cli ${CYAN}githubJson$RESET [DSTFILE] [--verbose]
             | $Cli ${CYAN}gitExec$RESET [DSTREPO] [--verbose]
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

@arg(doc = "Make a system call")
@main
def gitExec(repo: String): Unit = {
  Try(
    %%(
      "git",
      "--no-pager",
      "log",
      "--pretty=format:\"%h%x09%an%x09%ad%x09%s\"",
      "--date=short"
    )(Path(repo))
  ) match {
    case Success(result) if result.exitCode == 0 =>
      result.out.lines.map(_.replace("\"", "")).foreach(println)
    case Success(result) =>
      println(s"Unsuccessful ${result.exitCode}")
    case Failure(ex) =>
      println("Failure!")
      ex.printStackTrace()
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

@arg(doc = "Read the contribution JSON and print some markdown")
@main
def githubJson(
    @arg(doc = "The source file to read the JSON from")
    srcFile: String = "/tmp/github_contributions.json",
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {
  val contribs = ujson.read(read ! Path(srcFile)).asInstanceOf[Obj]
  val githubContribsByDate = githubJsonParse(contribs).filter(_._2 != 0)
  println(calendarize(githubContribsByDate).build())
}

/** Parse the JSON from the GitHub GraphQL API return the days and the number of
  * contributions for that day.
  * @param contribs
  *   The API results.
  * @return
  *   A SortedMap containing all of the days mapped to their number of
  *   contributions.
  */
private def githubJsonParse(contribs: Obj): SortedMap[Long, Int] = {
  val weeks = contribs("data")("user")("contributionsCollection")(
    "contributionCalendar"
  )("weeks").arr
  val byDate =
    for (x <- weeks; y <- x("contributionDays").arr)
      yield (
        LocalDate.parse(y("date").str, YyyyMmDd).toEpochDay,
        y("contributionCount").num.toInt
      )
  SortedMap.empty[Long, Int] ++ byDate.toSeq.toMap
}

/** Make a pretty Markdown calendar from a map of epoch days.
  * @param in
  *   A map of epoch day mapping to a value.
  * @param default
  *   The default value to use if there isn't any value for the date in the map.
  * @return
  *   The Markd [[Table]] element containing a weekly wrapped calendar of the
  *   string values from the map, from the minimum date to the maximum date.
  */
private def calendarize(
    in: SortedMap[Long, Any],
    default: String = ""
): Table = {
  val start = GettingThingsDone.nextWeekStartByEpoch(
    Some(in.keySet.min),
    DayOfWeek.SUNDAY
  ) - 7
  val rows = (start to in.keySet.max by 7)
    .map(sunday => (sunday, in.range(sunday, sunday + 7)))
    .map { case (sunday, week) =>
      LocalDate.ofEpochDay(sunday).format(MonthDay) +: (sunday until sunday + 7)
        .map(week.get(_).map(_.toString).getOrElse(default))
    }
    .map(TableRow.from)
  Table.from(
    Seq.fill(8)(Align.LEFT),
    TableRow.from(
      "",
      "Sun",
      "Mon",
      "Tue",
      "Wed",
      "Thu",
      "Fri",
      "Sat"
    ) +: rows: _*
  )
}
