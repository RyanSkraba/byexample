package com.skraba.byexample.scalatools.gitchecker

import com.skraba.byexample.scalatools.gitchecker.Commit.getDateFromRepo
import com.tinfoiled.docopt4s.FsPath.RichPath
import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, FsPath, Task}

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{Files, Path}
import java.time.format.DateTimeFormatter
import java.time.temporal.ChronoUnit
import java.time.{DayOfWeek, LocalDateTime}
import scala.sys.process.Process
import scala.util.{Random, Try}

object RewriteDateTask extends Task {
  override val Description: String = "Rewrite a git commit date from the command line"
  override val Cmd: String = "rewriteDate"
  override val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${GitCheckerGo.Name} $Cmd [CMD] [options]
       |
       |Options:
       |  -h --help    Show this screen
       |  --version    Show version
       |  CMD          The command to execute for modifying the last commit date
       |  --src=SRC    The directory where the git repo is located (By default, the
       |               current working directory)
       |  --tz=TZ      The time zone to use (as the TZ environment variable when
       |               executing the git command. [Default: :Europe/Paris]
       |  --fuzz=FUZZ  Fuzziness to apply when generating relative times [Default: 0.1]
       |  --dryRun     True if no changes should actually be executed.
       |  --noVerbose  True if the action should be silent (verbose by default)
       |  --plain      Do not show colour output
       |  --yes        Quiet mode, assume "yes" to all Y/N prompts
       |
       |By default, the command (CMD) is "next1day" which rewrites the last commit
       |to be approximately one day after the previous commit.
       |
       |If you set --fuzz=0 it will be exactly one day after the previous commit.
       |
       |Other commands include:
       |
       |  add1day   -> Adds approximately 24h to the current time of the commit
       |  next5min  -> Sets the current commit to be approximately 5 minutes after
       |               the previous commit.
       |  sub1hour  -> Subtracts approximately 1h from the current time of the commit.
       |  add10week -> Adds approximately 10 weeks to the current commit time.
       |  zero      -> Rewrites the commit time to its current value.
       |  now       -> Rewrites the last commit time to right now.
       |
       |""".stripMargin.trim

  override def go(opt: Docopt): Unit = {
    val cmd: String = opt.string.getOr("CMD", "next1day")
    val src: Path = opt.dir.getOr("--src", FsPath.Pwd)
    val timeZone: String = opt.string.getOr("--tz", ":Europe/Paris")
    val fuzz: Double = opt.string.getOr("--fuzz", "0.1").toDouble
    val dryRun = opt.flag("--dryRun")
    val out = AnsiConsole(verbose = !opt.flag("--noVerbose"), plain = opt.flag("--plain"), yes = opt.flag("--yes"))

    // Regex to match command that adjust a base date with a certain number of units.
    val RelativeCommand = "(next|add|sub)(\\d+)(min|mins|hours?|days?|weeks?)".r

    val Formatters: Seq[(String, DateTimeFormatter)] = Seq(
      "ISO" -> DateTimeFormatter.ISO_LOCAL_DATE_TIME,
      "RFC1123" -> DateTimeFormatter.RFC_1123_DATE_TIME,
      "yyyyMMddHHmmss" -> DateTimeFormatter.ofPattern("yyyyMMddHHmmss"),
      "yyyy-MM-dd HH:mm:ss" -> DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss"),
      "Git default" -> DateTimeFormatter.ofPattern("[EEE ]MMM dd HH:mm:ss yyyy[ Z]"),
      "Git default-like 1" -> DateTimeFormatter.ofPattern("[EEE ]dd MMM HH:mm:ss yyyy[ Z]"),
      "Git default-like 3" -> DateTimeFormatter.ofPattern("[EEE ]MMM dd yyyy HH:mm:ss[ Z]"),
      "Git default-like 2" -> DateTimeFormatter.ofPattern("[EEE ]dd MMM yyyy HH:mm:ss[ Z]")
    )

    val baseDate = Try(cmd match {
      // Fetch the date to adjust directly from the git repository for
      // some commands.
      case RelativeCommand("next", _, _)     => getDateFromRepo(commit = "HEAD^", repo = src)
      case "zero" | RelativeCommand(_, _, _) => getDateFromRepo(repo = src)
      case "now"                             => LocalDateTime.now()

      // If the command matches a day of the week, then use the most
      // recent date of that day with the current time
      case cmd if Try(DayOfWeek.valueOf(cmd.toUpperCase())).isSuccess =>
        val now = LocalDateTime.now()
        now.plusDays((DayOfWeek.valueOf(cmd.toUpperCase).ordinal() - now.getDayOfWeek.ordinal() - 7) % 7)

      // Otherwise try and parse the command using a variety of formatters.
      case _ =>
        val attempts = Formatters
          .to(LazyList)
          .map(fmt => {
            val attempt = Try { LocalDateTime.parse(cmd, fmt._2) }
            out.vPrintln(
              if (attempt.isSuccess) out.ok(s"Succeeded parsing ${fmt._1}\n")
              else out.error(s"Failure trying ${fmt._1}")
            )
            attempt
          })
        attempts.find(_.isSuccess).map(_.get).getOrElse { attempts.head.get }
    })

    // Create a GPG script that can fake the system time from an environment variable.
    val gpgWithRewrite = src.getRoot / "tmp" / "gpgWithRewrite.sh"
    if (!dryRun && !gpgWithRewrite.exists) {
      gpgWithRewrite.writeAll("#!/bin/sh\ngpg --faked-system-time \"$GPG_FAKED_DATE!\" $@")
      Files.setPosixFilePermissions(gpgWithRewrite, PosixFilePermissions.fromString("rwxrwxrwx"))
    }

    // Get an adjusted, fuzzed date off of the base date.
    val fuzzedDate = baseDate.map(bd => {
      val adjusted = cmd match {
        case RelativeCommand("next" | "add", time, "min" | "mins")   => bd.plusMinutes(time.toInt)
        case RelativeCommand("sub", time, "min" | "mins")            => bd.minusMinutes(time.toInt)
        case RelativeCommand("next" | "add", time, "hour" | "hours") => bd.plusHours(time.toInt)
        case RelativeCommand("sub", time, "hour" | "hours")          => bd.minusHours(time.toInt)
        case RelativeCommand("next" | "add", time, "day" | "days")   => bd.plusDays(time.toInt)
        case RelativeCommand("sub", time, "day" | "days")            => bd.minusDays(time.toInt)
        case RelativeCommand("next" | "add", time, "week" | "weeks") => bd.plusWeeks(time.toInt)
        case RelativeCommand("sub", time, "week" | "weeks")          => bd.minusWeeks(time.toInt)
        case _                                                       => bd
      }

      import ChronoUnit.SECONDS
      val adjustedDiff = bd.until(adjusted, SECONDS)
      val fuzzDev = (15 * 60d) min (fuzz * adjustedDiff)
      val fuzzSeconds = (fuzzDev * Random.nextGaussian()).toLong

      val fuzzed = bd.plusSeconds(adjustedDiff + fuzzSeconds)

      val fuzzedDiff = bd.until(fuzzed, SECONDS)
      out.vPrintln(out.msg3("      fuzz:", s"$fuzz / $fuzzDev / ${fuzzSeconds}s"))
      out.vPrintln(out.msg3(" base date:", bd))
      out.vPrintln(out.msg3("  adjusted:", s"$adjusted (${adjustedDiff}s)"))
      out.vPrintln(out.msg3("    fuzzed:", s"$fuzzed (${fuzzedDiff}s)"))

      fuzzed
    })

    // Apply the fuzzed date to the head of the repo
    fuzzedDate.fold(
      dtpe => {
        if (opt.flag("--verbose")) dtpe.printStackTrace()
        out.vPrintln(out.error("Unexpected command:", cmd))
      },
      fuzzed => {
        val fakedDate = fuzzed.atZone(java.time.ZoneId.systemDefault()).toEpochSecond.toString
        out.vPrintln(out.msg(s"""GPG_FAKED_DATE="$fakedDate" GIT_COMMITTER_DATE="$fuzzed" \\
             |    git -c "gpg.program=$gpgWithRewrite" commit --amend --no-edit --date $fuzzed""".stripMargin))
        if (!dryRun)
          out.vPrintln(
            Process(
              Seq(
                "git",
                "-c",
                s"gpg.program=$gpgWithRewrite",
                "commit",
                "--amend",
                "--no-edit",
                "--date",
                fuzzed.toString
              ),
              src.toFile,
              "TZ" -> timeZone,
              "GIT_COMMITTER_DATE" -> fuzzed.toString,
              "GPG_FAKED_DATE" -> fakedDate
            ).!!.linesIterator.mkString
          )
      }
    )
  }
}
