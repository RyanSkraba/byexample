#!/usr/bin/env amm

/** Some ammonite scala scripts that demonstrate using git. */

import mainargs.{Flag, arg, main}

// ==========================================================================
// Adding artifacts to your local build (from this project, from maven and
// from local maven).
import $file.local_import_util
local_import_util.load("ammonite-by-example")

@
import com.skraba.byexample.scala.ammonite.ConsoleCfg

// ==========================================================================
// Top level variables available to the script

// ==========================================================================
// Help

@arg(doc = "Print help to the console.")
@main
def help(out: ConsoleCfg): Unit = {
  // The help header includes all of the subcommands
  val cli = "git_checker.sc"

  // Usage examples
  println(out.helpUse(cli, "ghContrib", "[USER]", "[DSTFILE]", "[--verbose]"))
  println(out.helpUse(cli, "ghOpenPrs", "[githuborg/proj]"))
  println(out.helpUse(cli, "ghFailedRuns", "[githuborg/proj]"))
  println()
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
    @arg(doc = "Output the text in HTML")
    html: Flag,
    out: ConsoleCfg
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
  out.vPrintln(out.magenta("Output"))
  out.vPrintln(failures)

  // Extract the information that we need from the build failures
  val prs: Seq[(String, String, String, String)] = ujson
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
      val name = pr("name").str + " #" + pr("number").num.toInt
      val url = pr("url").str
      (release, name, date, url)
    })
    .toSeq

  if (html.value) {
    println("""<!DOCTYPE html>
              |<html><body>
              |""".stripMargin)
    prs
      .map { case (release, name, date, url) => name -> s"### $release $name ($date) $url" }
      .map { case label -> clipboard => s"  <p><button clipboard='$clipboard'>$label</button></p>" }
      .foreach(println)
    println("""<script>
              |// Add event listener to handle clicks on all buttons
              |document.querySelectorAll('button').forEach(button => {
              |  button.addEventListener('click', function() {
              |    var copyText = button.getAttribute('clipboard');
              |    if (copyText === null || copyText === undefined) {
              |      copyText = button.textContent;
              |    }
              |    navigator.clipboard.writeText(copyText);
              |  });
              |});
              |</script>
              |</body></html>
              |""".stripMargin)
  } else {
    prs
      .map { case (release, name, date, url) => s"### $release $name ($date) $url" }
      .foreach(println)
  }
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
    out: ConsoleCfg
): Unit = {
  // You need the gh token to proceed
  val token = os.proc("gh", "auth", "token").call(os.pwd)
  val contributions = requests.post(
    url = "https://api.github.com/graphql",
    headers = Seq(
      ("Authorization", s"Bearer ${token.out.lines().head.trim}"),
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
  out.vPrintln(contributions.text())
  println(out.ok("Writing to " + out.bold(dstFile)))
}
