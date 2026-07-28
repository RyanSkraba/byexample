package com.skraba.byexample.scalatools.gitchecker

import com.tinfoiled.docopt4s.FsPath.{Pwd, RichPath}
import com.tinfoiled.docopt4s.{AnsiConsole, Docopt, PathValidator, Task}
import play.api.libs.json.{JsArray, Json}

import java.nio.file.Path
import scala.io.Source
import scala.sys.process.Process
import scala.util.Using

object GitHubTask extends Task {
  override val Description: String = "Access GitHub information from the command line"
  override val Cmd: String = "github"
  override val Doc: String =
    s"""$Description
       |
       |Usage:
       |  ${GitCheckerGo.Name} $Cmd openPr PRJ [options]
       |  ${GitCheckerGo.Name} $Cmd contrib USER [options]
       |  ${GitCheckerGo.Name} $Cmd failedRun PRJ [MAINVER] [--html] [options]
       |
       |Options:
       |  -h --help    Show this screen
       |  --version    Show version
       |  USER         A GitHub user name to look up
       |  PRJ          The repository in the form org/repo
       |  --dst=DST    A destination file to write
       |  --verbose    True if the action should be silent (verbose by default)
       |  --plain      Do not show colour output
       |  --mock=JSON  Used for testing, provides fake JSON results from the GitHub API
       |
       |""".stripMargin.trim

  /** @param dst
    *   Optional destination file to save the results, otherwise printed to STDOUT.
    * @param html
    *   Whether to output the results in HTML
    * @param mock
    *   When present, use this as the JSON returned from GitHub (for testing)
    * @param out
    *   Console for output.
    */
  case class Cfg(dst: Option[Path], html: Boolean, mock: Option[String], out: AnsiConsole)

  override def go(opt: Docopt): Unit = {
    // Common configuration for the sub-tasks
    val cfg = Cfg(
      dst = opt.file.getOption("--dst", PathValidator().doesntExist()),
      html = opt.flag("--html"),
      mock = opt.string.getOption("--mock"),
      out = AnsiConsole(verbose = opt.flag("--verbose"), plain = opt.flag("--plain"), yes = opt.flag("--yes"))
    )

    // Execute the subtasks
    if (opt.flag("openPr")) {
      githubOpenPullRequests(opt.string.get("PRJ"), cfg)
    } else if (opt.flag("contrib")) {
      githubContributions(opt.string.get("USER"), cfg)
    } else if (opt.flag("failedRun")) {
      githubFailedRuns(opt.string.get("PRJ"), opt.string.getOr("MAINVER", "main"), cfg)
    }
  }

  /** One of the simplest requests you can make on GitHub, doesn't require an auth token, just the organisation and
    * repository name.
    *
    * @param prj
    *   The organisation and repository name, like apache/avro
    * @param cfg
    *   Configuration for the output
    */
  def githubOpenPullRequests(prj: String, cfg: Cfg): Unit = {
    val prsResponse = cfg.mock.getOrElse(
      // The real call, an example of getting information directly from the GitHub API
      Using.resource(
        Source.fromURL(s"https://api.github.com/search/issues?q=repo:$prj%20state:open%20is:pr")("UTF-8")
      ) { _.mkString }
    )

    // Print the response as formatted JSON if verbose, but just print
    val prsAsJson = Json.parse(prsResponse)
    cfg.out.vPrintln(Json.prettyPrint(prsAsJson))
    val prCount = (prsAsJson \ "total_count").as[Int]

    cfg.dst
      .map(_.writeAll(Json.prettyPrint(prsAsJson)))
      .foreach(dst => println(cfg.out.ok("Writing to " + dst)))
    print(cfg.out.ok(prCount))
  }

  /** Uses the GitHub GraphQL endpoint to fetch the contributions from a given user.
    *
    * This is more com
    *
    * @param user
    *   GitHub user to fetch contribution information
    * @param cfg
    *   Configuration for the output
    */
  def githubContributions(
      user: String,
      cfg: Cfg
  ): Unit = {

    // You need the gh token to proceed
    val token =
      Git.Token.getOrElse {
        Console.out.println(cfg.out.error("Cannot find 'GH_TOKEN' or 'gh auth token'"))
        return
      }

    import java.net.URI
    import java.net.http.{HttpClient, HttpRequest, HttpResponse}
    import java.time.Duration

    val client = HttpClient.newBuilder().connectTimeout(Duration.ofSeconds(10)).build()

    val url = "https://api.github.com/graphql"
    val body = s"""{"query":"query($$userName:String!) {
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
                  |"variables":{"userName":"$user"}}""".stripMargin.replace('\n', ' ')

    val contributions = cfg.mock match {
      case Some(json) => json
      case _ =>
        val request = HttpRequest
          .newBuilder()
          .uri(URI.create(url))
          .header("Content-Type", "application/json")
          .header("Authorization", s"Bearer $token")
          .timeout(Duration.ofSeconds(10))
          .POST(HttpRequest.BodyPublishers.ofString(body))
          .build()
        val response = client.send(request, HttpResponse.BodyHandlers.ofString())
        if (response.statusCode() == 200)
          cfg.out.vPrintln(s"Status: ${response.statusCode()}")
        else {
          println(cfg.out.error(s"Status: ${response.statusCode()}"))
          return
        }
        response.body()
    }

    cfg.dst.map(_.writeAll(contributions)).foreach(dst => println(cfg.out.ok("Writing to " + cfg.out.bold(dst))))
    cfg.out.vPrintln(contributions)
  }

  /** @param prj
    *   The project in the form apache/avro
    * @param mainVersion
    *   The version string for the main branch
    * @param cfg
    *   Configuration for the output
    */

  def githubFailedRuns(
      prj: String,
      mainVersion: String = "main",
      cfg: Cfg
  ): Unit = {

    val failures = cfg.mock match {
      case Some(json) => json
      case _ =>
        Git(
          Pwd,
          "gh",
          Seq(
            "run",
            "list",
            "--repo",
            prj,
            "--json",
            "startedAt,updatedAt,name,number,status,url,conclusion,headBranch",
            "--status",
            "failure"
          )
        )
    }

    cfg.out.vPrintln(cfg.out.magenta("Output"))
    cfg.out.vPrintln(failures)

    // Extract the information that we need from the build failures
    val prs: Seq[(String, String, String, String)] = Json
      .parse(failures)
      .as[JsArray]
      .value
      .sortBy(_("startedAt").toString)
      .reverse
      .map(pr => {
        val release = pr("headBranch").as[String] match {
          case "main" | "master"                 => mainVersion
          case rel if rel.startsWith("release-") => rel.substring(8)
          case br                                => br
        }
        val date = pr("startedAt").as[String]
        val name = pr("name").as[String] + " #" + pr("number").as[Int]
        val url = pr("url").as[String]
        (release, name, date, url)
      })
      .toSeq

    if (cfg.html) {
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

}
