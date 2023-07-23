#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import java.time.{DayOfWeek, LocalDate, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import mainargs.{Flag, arg, main}

import scala.collection.{SortedMap, mutable}
import scala.io.AnsiColor._
import scala.util._
import ujson.Obj

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).

import $file.local_import_util
local_import_util.load(
  "com.skraba.byexample",
  "scala-by-example",
  "0.0.1-SNAPSHOT"
)
local_import_util.load(
  "com.skraba.byexample",
  "ammonite-by-example",
  "0.0.1-SNAPSHOT"
)

@
import com.skraba.byexample.scala.ammonite.gtd.GettingThingsDone
import com.skraba.byexample.scala.ammonite.git.Commit.getDateFromRepo
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
         |The --verbose flag was set!
         |
         |Try running these commands:
         |$Cli argTest
         |$Cli argTest Me
         |$Cli argTest --verbose Me
         |$Cli argTest Me Hey
         |$Cli argTest You "Hello there" VerboseFlag
         |$Cli argTest You "Hello there" VerboseFlag Invalid
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
    os.proc(
      "git",
      "--no-pager",
      "log",
      "--pretty=format:\"%h%x09%an%x09%ad%x09%s\"",
      "--date=short"
    ).call(os.Path(repo))
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
  val token = os.proc("gh", "auth", "token").call(os.pwd)
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

  os.write.over(os.Path(dstFile), contributions.text())
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
  val contribs = ujson.read(os.read(os.Path(srcFile))).asInstanceOf[Obj]
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
  SortedMap.empty[Long, Int] ++ byDate.toMap
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

/** An experiment to decorate a git contribution calendar with private
  * information.
  */
@main
def gitJsonDecorated(
    srcFile: String = "/tmp/github_contributions.json",
    spec: Seq[String] = Nil
): Unit = {
  val contribs = ujson.read(os.read(os.Path(srcFile))).asInstanceOf[Obj]
  val byDate = mutable.SortedMap.empty[Long, Int] ++ githubJsonParse(contribs)
    .filter(_._2 != 0)
    .mapValues(_.toString)

  def git(prj: String): Seq[String] = {
    os.proc(
      "git",
      "--no-pager",
      "log",
      "--pretty=format:\"%ad\"",
      "--date=short"
    ).call(os.Path(prj))
      .out
      .lines
      .map(_.replace("\"", ""))
  }

  def augment(
      tag: String,
      repo: String,
      minDate: Long = byDate.keySet.min
  ): Unit =
    git(repo)
      .map(LocalDate.parse(_, YyyyMmDd).toEpochDay)
      .filter(_ > minDate)
      .foreach(day =>
        byDate += (day -> byDate.get(day).map(_ + s" $tag").getOrElse(tag))
      )

  spec.map(_.split(":")).foreach { case Array(tag, repo) => augment(tag, repo) }
  println(calendarize(byDate, "**0**").build())
}

/** An experiment to rewrite a git date on the last commit. */
@main
def gitRewriteDate(
    cmd: String = "next1day",
    prj: String = os.pwd.toString(),
    timeZone: String = ":Europe/Paris",
    fuzz: Double = 0.1,
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {

  // Regex to match command that adjust a base date with a certain number of units.
  val RelativeCommand = "(next|add|sub)(\\d+)(min|mins|hours?|days?|weeks?)".r

  val Formatters: Seq[(String, DateTimeFormatter)] = Seq(
    "ISO" -> DateTimeFormatter.ISO_LOCAL_DATE_TIME,
    "RFC1123" -> DateTimeFormatter.RFC_1123_DATE_TIME,
    "yyyyMMddHHmmss" -> DateTimeFormatter.ofPattern("yyyyMMddHHmmss"),
    "yyyy-MM-dd HH:mm:ss" -> DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"),
    "Git default" -> DateTimeFormatter.ofPattern(
      "[EEE ]MMM dd HH:mm:ss yyyy[ Z]"
    ),
    "Git default-like 1" -> DateTimeFormatter.ofPattern(
      "[EEE ]dd MMM HH:mm:ss yyyy[ Z]"
    ),
    "Git default-like 3" -> DateTimeFormatter.ofPattern(
      "[EEE ]MMM dd yyyy HH:mm:ss[ Z]"
    ),
    "Git default-like 2" -> DateTimeFormatter.ofPattern(
      "[EEE ]dd MMM yyyy HH:mm:ss[ Z]"
    )
  )

  val baseDate = Try(cmd match {
    // Fetch the date to adjust directly from the git repository for
    // some commands.
    case RelativeCommand("next", _, _)    => getDateFromRepo("HEAD^")
    case "zero" | RelativeCommand(_, _, _) => getDateFromRepo()
    case "now" => LocalDateTime.now()

    // If the command matches a day of the week, then use the most
    // recent date of that day with the current time
    case cmd if Try(DayOfWeek.valueOf(cmd.toUpperCase())).isSuccess =>
      val now = LocalDateTime.now()
      now.plusDays(
        (DayOfWeek.valueOf(cmd.toUpperCase).ordinal() -
          now.getDayOfWeek.ordinal() - 7) % 7
      )

    // Otherwise try and parse the command using a variety of formatters.
    case _ =>
      val attempts = Formatters.toStream.map(fmt => {
        val attempt = Try { LocalDateTime.parse(cmd, fmt._2) }
        if (verbose.value) {
          if (attempt.isSuccess)
            println(s"${GREEN}Succeeded parsing ${fmt._1}\n")
          else
            println(s"${RED}Failure trying ${fmt._1}")
        }
        attempt
      })
      attempts.find(_.isSuccess).map(_.get).getOrElse { attempts.head.get }
  })

  // Create a GPG script that can fake the system time from an environment variable.
  val gpgWithRewrite = os.root / "tmp" / "gpgWithRewrite.sh"
  if (!os.exists(gpgWithRewrite)) {
    os.write(
      gpgWithRewrite,
      "#!/bin/sh\ngpg --faked-system-time \"$GPG_FAKED_DATE!\" $@"
    )
    os.perms.set(gpgWithRewrite, os.PermSet.fromString("rwxrwxrwx"))
  }

  // Get an adjusted, fuzzed date off of the base date.
  val fuzzedDate = baseDate.map(bd => {
    val adjusted = cmd match {
      case RelativeCommand("next" | "add", time, "min" | "mins") =>
        bd.plusMinutes(time.toInt)
      case RelativeCommand("sub", time, "min" | "mins") =>
        bd.minusMinutes(time.toInt)
      case RelativeCommand("next" | "add", time, "hour" | "hours") =>
        bd.plusHours(time.toInt)
      case RelativeCommand("sub", time, "hour" | "hours") =>
        bd.minusHours(time.toInt)
      case RelativeCommand("next" | "add", time, "day" | "days") =>
        bd.plusDays(time.toInt)
      case RelativeCommand("sub", time, "day" | "days") =>
        bd.minusDays(time.toInt)
      case RelativeCommand("next" | "add", time, "week" | "weeks") =>
        bd.plusWeeks(time.toInt)
      case RelativeCommand("sub", time, "week" | "weeks") =>
        bd.minusWeeks(time.toInt)
      case _ => bd
    }

    import ChronoUnit.SECONDS
    val adjustedDiff = bd.until(adjusted, SECONDS)
    val fuzzDev = (15 * 60d) min (fuzz * adjustedDiff)
    val fuzzSeconds = (fuzzDev * Random.nextGaussian()).toLong

    val fuzzed =
      bd.plusSeconds(adjustedDiff + fuzzSeconds)

    if (verbose.value) {
      val fuzzedDiff = bd.until(fuzzed, SECONDS)
      println(
        s"""$BOLD$MAGENTA      fuzz: $RESET$fuzz / $fuzzDev / ${fuzzSeconds}s
          |$BOLD$MAGENTA base date: $RESET$bd
          |$BOLD$MAGENTA  adjusted: $RESET$adjusted (${adjustedDiff}s)
          |$BOLD$MAGENTA    fuzzed: $RESET$fuzzed (${fuzzedDiff}s)
          |""".stripMargin
      )
    }

    fuzzed
  })

  // Apply the fuzzed date to the head of the repo
  fuzzedDate.fold(
    dtpe => {
      if (verbose.value) dtpe.printStackTrace()
      println(s"$RED${BOLD}Unexpected command: $cmd")
    },
    fuzzed => {
      val fakedDate =
        fuzzed.atZone(java.time.ZoneId.systemDefault()).toEpochSecond.toString
      if (verbose.value)
        println(
          s"""$BOLD${BLUE}GPG_FAKED_DATE="$fakedDate" GIT_COMMITTER_DATE="$fuzzed" git -c "gpg.program=$gpgWithRewrite" commit --amend --no-edit --date $fuzzed$RESET
             |""".stripMargin
        )
      println(
        os.proc(
          "git",
          "-c",
          s"gpg.program=$gpgWithRewrite",
          "commit",
          "--amend",
          "--no-edit",
          "--date",
          fuzzed.toString
        ).call(
          os.Path(prj),
          env = Map(
            "TZ" -> timeZone,
            "GIT_COMMITTER_DATE" -> fuzzed.toString,
            "GPG_FAKED_DATE" -> fakedDate
          )
        ).out
          .lines
          .mkString
      )
    }
  )
}

/** An experiment to rewrite a git date on the last commit. */
@main
def gitRewriteDates(
    commit: String = "HEAD",
    cmd: String = "next1day",
    prj: String = os.pwd.toString(),
    dstBranch: String = "tmp",
    @arg(doc = "Verbose for extra output")
    verbose: Flag
): Unit = {

  val revList =
    os.proc("git", "rev-list", s"$commit..HEAD").call(os.Path(prj)).out.lines
  os.proc("git", "switch", "-c", dstBranch, commit).call(os.Path(prj))

  revList.reverse.foreach { rev =>
    println(s"Cherry picking $rev")
    os.proc("git", "cherry-pick", rev).call(os.Path(prj))
    gitRewriteDate(cmd, prj, verbose = verbose)
  }
}
