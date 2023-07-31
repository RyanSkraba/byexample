#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

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
import com.skraba.byexample.scala.ammonite.git.{CherryPickerReport, Commit}
import com.skraba.byexample.scala.ammonite.git.Commit.getDateFromRepo
import com.skraba.byexample.scala.ammonite.ColourCfg
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

val Cli = "git_checker.sc"

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(cfg: ColourCfg): Unit = {
    println(s"""${cfg.ok(Cli, bold=true)} - Do some analysis on git repositories.
             |
             | ${cfg.left("cherryPick")} : Get a status report on two branches
             |
             |Usage:
             |
             | ${cfg.ok(Cli)} ${cfg.left("cherryPick")} [repo] [main] [branch]
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
    cfg:ColourCfg
): Unit = {

  if (cfg.verbose.value) {
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
  if (cfg.verbose.value || statusDoc.isEmpty) println(txt)

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

/** Everything necessary too rewrite a git date from the command-line. */
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
