#!/usr/bin/env amm

/** Some examples on using ammonite for scripting in scala. */

import java.time.{DayOfWeek, LocalDate}
import java.time.format.DateTimeFormatter
import mainargs.{Flag, ParserForClass, arg, main}
import scala.collection.{SortedMap, mutable}
import scala.io.AnsiColor._
import scala.util._
import scala.util.matching.Regex
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
import com.skraba.byexample.scala.markd._

// ==========================================================================
// Top level variables available to the script

@main
case class Config(
    @arg(doc = "Verbose for extra output")
    verbose: Flag
)

implicit def configParser: ParserForClass[Config] = ParserForClass[Config]

val YyyyMmDd = DateTimeFormatter.ofPattern("yyyy-MM-dd")
val MonthDay = DateTimeFormatter.ofPattern("MMM d")
val Cli = s"${GREEN}ammonite_example.sc$RESET"

// ==========================================================================
// Main tasks

@arg(doc = "Print help to the console.")
@main
def help(
    cfg: Config
): Unit = {
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
  if (cfg.verbose.value)
    println(s"The --verbose flag was set!")
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
    cfg: Config
): Unit = {
  println(s"""$YELLOW${greeting.getOrElse("Hello")}, $BOLD$user$RESET""")
  if (cfg.verbose.value) {
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

@arg(doc = "Search and replace text patterns recursively in this directory.")
@main
def sar(
    @arg(doc = "The directory to recursely search")
    dir: Option[os.Path] = None,
    @arg(doc = "Files to exclude from the search")
    exclude: Seq[String] = Seq(),
    @arg(doc = "Files to include in the search")
    include: Seq[String] = Seq(".*"),
    @arg(doc = "Pairs of regular expressions to search and replace")
    re: Seq[String] = Seq(),
    cfg: Config
): Unit = {

  // The source path to analyse
  val src: os.Path = dir.getOrElse(os.pwd)
  if (!(os.exists(src))) {
    println(s"$RED${BOLD}ERROR:$RESET $src does not exist.")
    System.exit(1)
  }

  // Adapt the exclude and include rules to regular expressions
  val includeRe =
    if (include.isEmpty) Seq(".*".r) else include.map(new Regex(_).unanchored)
  val excludeRe =
    if (exclude.isEmpty) Seq("^\\.git".r, "\\btarget\\b".r.unanchored)
    else exclude.map(new Regex(_).unanchored)

  // Find all of the files in the directory that aren't excluded
  val files: Seq[os.FilePath] = os
    .walk(
      src,
      skip = p => excludeRe.exists(_.matches(p.relativeTo(src).toString))
    )
    .filter(os.isFile)
    .map(_.relativeTo(src))

  val included: Set[os.FilePath] =
    files.filter(p => includeRe.exists(_.matches(p.toString))).toSet

  if (cfg.verbose.value) {
    println(s"$GREEN${BOLD}Matching files:$RESET")
    for (f <- files.sortBy(_.toString) if included(f))
      println(s"  $f")
  }

  val modified = for (f <- files) yield {
    if (included(f)) {
      val original = os.read(f.resolveFrom(src))
      val contents = re.grouped(2).foldLeft(original) {
        case (acc, Seq(search, replace)) =>
          new Regex(search).replaceAllIn(acc, replace)
        case (acc, _) => acc // Ignore any leftover
      }
      val modified = contents != original
      if (cfg.verbose.value) print(if (modified) s"${RED}x" else s"${GREEN}x")
      if (modified) {
        os.write.over(f.resolveFrom(src), contents)
        Seq(f)
      } else Seq.empty
    } else {
      if (cfg.verbose.value) print(s"$RESET.")
      Seq.empty
    }
  }

  if (cfg.verbose.value) {
    println(
      excludeRe.mkString(
        s"\n$RED${BOLD}Exclude patterns (leaving ${files.size} file to scan):$RESET$RED\n  ",
        "\n  ",
        if (excludeRe.isEmpty) RESET else s"\n$RESET"
      )
    )
    println(
      includeRe.mkString(
        s"$GREEN${BOLD}Include patterns (${included.size}) :$RESET$GREEN\n  ",
        "\n  ",
        if (includeRe.isEmpty) RESET else s"\n$RESET"
      )
    )
    println(s"$RESET${BOLD}Modified ${modified.flatten.size} files.")
  }
}

@arg(doc = "Save the contribution JSON from the GitHub API to a file")
@main
def githubApi(
    @arg(doc = "GitHub user to fetch contribution information")
    user: String,
    @arg(doc = "The destination file to save the JSON")
    dstFile: String = "/tmp/github_contributions.json",
    cfg: Config
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
  if (cfg.verbose.value) {
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
