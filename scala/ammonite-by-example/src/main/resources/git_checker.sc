#!/usr/bin/env amm

/** Some ammonite scala scripts that demonstrate using git. */

import mainargs.{Flag, arg, main}
import java.time.{DayOfWeek, LocalDateTime}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import scala.io.AnsiColor._
import scala.util._

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util

local_import_util.load("scala-by-example")
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.git.CherryPickerReport
import com.skraba.byexample.scala.ammonite.git.Commit.getDateFromRepo
import com.skraba.byexample.scala.ammonite.ConsoleCfg
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(cfg: ConsoleCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "git_checker.sc"
  println(
    cfg.helpHeader(
      cli,
      "Do some analysis on git repositories",
      "cherryPick" -> "Get a status report on two branches"
    )
  )

  // Usage examples
  println(cfg.helpUse(cli, "cherryPick", "[repo]", "[main]", "[branch]"))
  println(cfg.helpUse(cli, "ghContrib", "[USER]", "[DSTFILE]", "[--verbose]"))
  println(cfg.helpUse(cli, "ghOpenPrs", "[githuborg/proj]"))
  println(cfg.helpUse(cli, "ghFailedRuns", "[githuborg/proj]"))
  println(cfg.helpUse(cli, "rewriteDate", "[cmd]"))
  println()
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
    cfg: ConsoleCfg
): Unit = {

  cfg.vPrintln(cfg.bold("Arguments:"))
  cfg.vPrintln(cfg.kv("      repo", repo))
  cfg.vPrintln(cfg.kv("      lTag", lTag))
  cfg.vPrintln(cfg.kv("      rTag", rTag))
  cfg.vPrintln(cfg.kv(" statusDoc", statusDoc))
  cfg.vPrintln(cfg.bold("\nGit command:"))
  cfg.vPrintln((CherryPickerReport.Cmd :+ s"$lTag...$rTag").mkString(" "))

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
  if (cfg.verbose.value || statusDoc.isEmpty) println(txt)

  statusDoc.foreach(os.write.over(_, txt))
}

// ==========================================================================
// GitHub API for counting open PRs

@arg(doc = "Use the GitHub API to count the open PRs on a given project")
@main
def ghOpenPrs(
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
  println(s"### ${prs("total_count").str}")
}

// ==========================================================================
// GitHub CLI for finding failed runs
@arg(doc = "Use the GitHub API to count the failed workflow runs on a given project")
@main
def ghFailedRuns(
    @arg(doc = "The project in the form apache/avro")
    prj: String,
    @arg(doc = "The default version for master")
    default: String = "main",
    cfg: ConsoleCfg
): Unit = {

  val failures = os
    .proc(
      "gh",
      "run",
      "list",
      "--repo",
      prj,
      "--json",
      "startedAt,updatedAt,name,number,status,url,conclusion,headBranch",
      "--status",
      "failure"
    )
    .call(os.pwd)
  cfg.vPrintln(cfg.magenta("Output"))
  cfg.vPrintln(failures)

  val prs = ujson
    .read(failures.out.text())
    .arr
    .sortBy(_("startedAt").toString)
    .reverse
    .map(pr => {
      val release = pr("headBranch").str match {
        case "main" | "master"                 => default
        case rel if rel.startsWith("release-") => rel.substring(8)
        case br                                => br
      }
      val date = pr("startedAt").str
      val name = pr("name").str + " #" + pr("number").num
      val url = pr("url").str
      s"### $release $name ($date) $url"
    })
  prs.foreach(println)
}

// ==========================================================================
// GitHub Contributions for a given user using GraphQL

@arg(doc = "Save the contribution JSON from the GitHub API to a file")
@main
def ghContrib(
    @arg(doc = "GitHub user to fetch contribution information")
    user: String,
    @arg(doc = "The destination file to save the JSON")
    dstFile: String = "/tmp/github_contributions.json",
    cfg: ConsoleCfg
): Unit = {
  // You need the gh token to proceed
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
  cfg.vPrintln(contributions.text())
  println(cfg.ok("Writing to " + cfg.bold(dstFile)))
}

// ==========================================================================
// Git from the command line to rewrite the last date

/** Everything necessary too rewrite a git date from the command-line. */
@main
def rewriteDate(
    cmd: String = "next1day",
    prj: String = os.pwd.toString(),
    timeZone: String = ":Europe/Paris",
    fuzz: Double = 0.1,
    cfg: ConsoleCfg
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
    case RelativeCommand("next", _, _)     => getDateFromRepo("HEAD^")
    case "zero" | RelativeCommand(_, _, _) => getDateFromRepo()
    case "now"                             => LocalDateTime.now()

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
        cfg.vPrintln(
          if (attempt.isSuccess) s"${GREEN}Succeeded parsing ${fmt._1}\n"
          else s"${RED}Failure trying ${fmt._1}"
        )
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

    cfg.vPrintln {
      val fuzzedDiff = bd.until(fuzzed, SECONDS)
      s"""$BOLD$MAGENTA      fuzz: $RESET$fuzz / $fuzzDev / ${fuzzSeconds}s
         |$BOLD$MAGENTA base date: $RESET$bd
         |$BOLD$MAGENTA  adjusted: $RESET$adjusted (${adjustedDiff}s)
         |$BOLD$MAGENTA    fuzzed: $RESET$fuzzed (${fuzzedDiff}s)
         |""".stripMargin
    }

    fuzzed
  })

  // Apply the fuzzed date to the head of the repo
  fuzzedDate.fold(
    dtpe => {
      if (cfg.verbose.value) dtpe.printStackTrace()
      println(s"$RED${BOLD}Unexpected command: $cmd")
    },
    fuzzed => {
      val fakedDate =
        fuzzed.atZone(java.time.ZoneId.systemDefault()).toEpochSecond.toString
      cfg.vPrintln(
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
