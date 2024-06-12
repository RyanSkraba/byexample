package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo
import play.api.libs.json.{JsArray, Json}

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.collection.SortedMap
import scala.util.matching.Regex
import scala.io.Source
import scala.util.Using

/** All of the information that was collected during a build failure investigation for one specific step. A build can
  * contain more than one failed job and step.
  *
  * @param investigateDate
  *   The date that the build failure was investigated (not the date it failed)
  * @param investigateOrder
  *   The order of the investigate date section, with zero being the most recent.
  * @param buildVersion
  *   The version that was being built
  * @param buildDesc
  *   A short description that the CI uses to refer to the build (workflow name)
  * @param buildLink
  *   The link to the CI build failure
  * @param buildOrder
  *   The order that the specific build was investigated, with zero being the most recent.
  * @param stepDesc
  *   The specific job and step in the build that failed
  * @param stepLink
  *   A link to the error in the build logs (if any)
  * @param issueTag
  *   The issue or defect that was identified to have caused the failure
  * @param issueDesc
  *   A helpful hint to refer to the issue
  * @param issueLink
  *   The link to the issue if available on the web
  * @param comment
  *   If present, additional comments in a code block to describe the investigation
  */
case class FailedStep(
    investigateDate: String,
    investigateOrder: Int = 0,
    buildVersion: String = "",
    buildDesc: String = "",
    buildDate: String = "",
    buildLink: String = "",
    buildOrder: Int = 0,
    stepDesc: String = "",
    stepLink: String = "",
    issueTag: String = "",
    issueDesc: String = "",
    issueLink: String = "",
    comment: Option[String] = None
) {

  /** Regex for separating off an http(s) URL from the end of a string. */
  private val SplitHttpsRegex: Regex = raw"(.*?)((?<!\S)https?://\S+)\s*".r

  /** Regex for separating off an http(s) URL from the end of a string. */
  private val SplitLastBracketRegex: Regex = raw"(.*?)(?<!\S)\((\S+)\)\s*".r

  /** A string to use as a markdown notification for the build failure. */
  lazy val notifBuildMd: String = s":red_circle:  Build *[$buildVersion $buildDesc]($buildLink)* failed"

  /** A string to use as a markdown notification for this specific step. */
  lazy val notifDetailMd: String = s"* [$stepDesc]($stepLink) *[$issueTag]($issueLink)* $issueDesc".trim

  /** A string to use as a notification for this specific step (without formatting). */
  lazy val notifDetail: String = s"* $buildVersion $stepDesc $stepLink".trim

  /** Creates a copy of this class enriched with some build information from the build title
    *
    * @param in
    *   The CI build title
    * @return
    *   The buildVersion, buildDesc and buildLink to be put into a [[FailedStep]]
    */
  def addBuildInfo(in: String): FailedStep = {
    val (buildPreLink, buildLink) = in match {
      case SplitHttpsRegex(before, uri) => (before, uri)
      case _                            => (in, "")
    }
    val (buildPreDate, buildDate) = buildPreLink match {
      case SplitLastBracketRegex(before, date) => (before, date)
      case _                                   => (buildPreLink, "")
    }
    val (buildVersion, buildDesc) = buildPreDate.span(!_.isWhitespace)
    copy(
      buildVersion = buildVersion.trim,
      buildDesc = buildDesc.trim,
      buildDate = buildDate.trim,
      buildLink = buildLink.trim
    )
  }

  /** Creates a copy of this class enriched with some build information from the step analysis
    *
    * @param in
    *   A paragraph describing the build failure for the step
    * @param issueLinkTemplate
    *   A String format that can be used to create a link for the issue from its tag
    * @return
    *   A tuple containing (in order) the stepDesc, stepLink, issueTag and issueDesc to be put into a [[FailedStep]]
    *   instance.
    */
  def addStepAndIssueInfo(in: String, issueLinkTemplate: String): FailedStep = {
    // Get the step information from the first line
    val lines = in.split('\n')
    val (stepDesc, stepLink) = lines.headOption match {
      case Some(SplitHttpsRegex(before, uri)) => (before, uri)
      case Some(str)                          => (str, "")
      case _                                  => ("", "")
    }
    // And get the issue information from the second line
    val (issueTag, issueDesc) = lines.drop(1).headOption.getOrElse("").span(!_.isWhitespace)
    copy(
      stepDesc = stepDesc.trim,
      stepLink = stepLink.trim,
      issueTag = issueTag.trim,
      issueDesc = issueDesc.trim,
      issueLink = if (issueTag.isEmpty) "" else issueLinkTemplate.format(issueTag)
    )
  }
}

/** Represents the build failure report that is being read (and optionally updated).
  *
  * @param doc
  *   The entire markdown file to parse
  * @param filterDays
  *   The number of days (second level headers) that should be included in any output
  * @param filterAfter
  *   A String to compare to day titles (the second level headers) to only include those that are greater than or equal
  *   to.
  * @param filterUntil
  *   A String to compare to day titles (the second level headers) to only include those that are less than or equal to.
  */
class BuildFailureReport(
    val doc: Header,
    filterDays: Int,
    filterAfter: Option[String],
    filterUntil: Option[String]
) {

  /** Links to issues are generated using this template, given the issueTag. */
  lazy val IssueTemplate: String = sys.props
    .get("issue.template")
    .orElse(sys.env.get("ISSUE_TEMPLATE"))
    .getOrElse("https://issues.apache.org/jira/browse/%s")

  /** The URL used to find new run failures is generated using this template, given the repo. */
  private lazy val RunFailsUriTemplate: String = sys.props
    .get("run.fails.template")
    .orElse(sys.env.get("GITHUB_RUN_FAILS_TEMPLATE"))
    .getOrElse("https://api.github.com/repos/%s/actions/runs?status=failure&exclude_pull_requests=true")

  /** The section containing the build failures reports. */
  private lazy val buildFailureSection: Header = doc
    .collectFirstRecursive {
      case section @ Header(title, 1, _) if title.toLowerCase.matches(raw".*\bbuild failures\b.*") => section
    }
    .getOrElse {
      throw new IllegalArgumentException("Can't find failure report")
    }

  /** All of the documented failures contained in the build failure report. */
  lazy val all: Seq[FailedStep] = buildFailureSection.mds
    .collect { case daily @ Header(_, 2, _) => daily }
    .zipWithIndex
    .flatMap { case (daily @ Header(investigateDate, 2, _)) -> investigateOrder =>
      daily.mds
        .collect { case buildDetails @ Header(buildTitle, 3, _) =>
          val base = FailedStep(investigateDate, investigateOrder).addBuildInfo(buildTitle)
          (buildDetails.mds.collect {
            case p: Paragraph => p
            case c: Code      => c
          } :+ Comment("Ignored"))
            .sliding(2)
            .flatMap {
              case Seq(Paragraph(content), c: Code) =>
                Some(base.addStepAndIssueInfo(content, IssueTemplate).copy(comment = Some(c.content)))
              case Seq(Paragraph(content), _*) =>
                Some(base.addStepAndIssueInfo(content, IssueTemplate))
              case _ => None
            }
            .toSeq
        }
    }
    .flatten
    .zipWithIndex
    .map { case step -> index => step.copy(buildOrder = index) }

  /** All of the documented failures that match the filters set for the report. */
  private lazy val filtered: Seq[FailedStep] = {
    val withMin = filterAfter.map(minDate => all.filter(_.investigateDate >= minDate)).getOrElse(all)
    val filteredDates = filterUntil.map(maxDate => withMin.filter(_.investigateDate <= maxDate)).getOrElse(withMin)

    if (filterDays == Int.MaxValue) filteredDates
    else {
      val headOrder = filteredDates.headOption.map(_.investigateOrder).getOrElse(0) + filterDays
      filteredDates.filter(_.investigateOrder < headOrder)
    }
  }

  /** All of the failures organised by the reported issue. */
  lazy val allStepsByIssue: SortedMap[String, Seq[FailedStep]] = SortedMap(
    filtered.groupBy(_.issueTag).toList: _*
  )

  /** All of the failures organised by the specific build. */
  lazy val allStepsByBuild: Seq[Seq[FailedStep]] = SortedMap(
    filtered.groupBy(_.buildLink).toList: _*
  ).values.toSeq.sortWith(_.head.buildOrder < _.head.buildOrder)

  def addNewFailures(repo: String, mainVersion: Option[String]): BuildFailureReport = {
    // The GitHub Actions workflow failures
    case class WorkflowRun(
        id: String,
        name: String,
        runNumber: String,
        headBranch: String,
        htmlUrl: String,
        createdAt: String
    ) {
      lazy val branch = mainVersion
        .map(default =>
          if (headBranch.startsWith("release-")) headBranch.substring(8)
          else if (headBranch == "main" || headBranch == "master") default
          else headBranch
        )
        .getOrElse(headBranch)

      lazy val buildFailureTitleMd: String = s"$branch $name #$runNumber ($createdAt) $htmlUrl".trim
    }

    // The list of the currently known builds
    val buildLinks = all.map(_.buildLink).toSet

    // The list of failed builds fetched from the GitHub API
    val jsString = Using(Source.fromURL(RunFailsUriTemplate.format(repo)))(_.mkString)
    val workflows = jsString
      .map(Json.parse)
      .map(_ \ "workflow_runs")
      .map(
        _.get
          .as[JsArray]
          .value
          .map(js =>
            WorkflowRun(
              (js \ "id").get.as[Long].toString,
              (js \ "name").get.as[String],
              (js \ "run_number").get.as[Long].toString,
              (js \ "head_branch").get.as[String],
              (js \ "html_url").get.as[String],
              (js \ "created_at").get.as[String]
            )
          )
      )
      .get // Throws any exception that was found on the way.
      .toSeq

    // The list of failed runs that are currently not documented
    val newRuns: Seq[WorkflowRun] = workflows.filterNot(run => buildLinks.apply(run.htmlUrl))

    if (newRuns.isEmpty) this
    else {
      // Today's date for the investigation
      val investigateDate = DateTimeFormatter.ofPattern("yyyy-MM-dd").format(LocalDate.now())
      // The section includes all of the new failures
      val newBuildFailures = newRuns.map(run => Header(3, run.buildFailureTitleMd, Paragraph("TODO")))
      val newDoc = doc.mapFirstIn() {
        case section: Header if section == buildFailureSection =>
          section.prepend(investigateDate, newBuildFailures: _*)
      }

      new BuildFailureReport(newDoc, filterDays, filterAfter, filterUntil)
    }
  }
}

/** Generates an HTML page with all of the notifications and reports present. */
class HtmlOutput(report: BuildFailureReport) {

  /** The start of the HTML document. */
  private val HtmlStart: String =
    """<!DOCTYPE html>
      |<html><head><style>
      |  button {white-space: pre-wrap;text-align:left;}
      |  a:active, a:hover, a:link, a:visited {text-decoration: none;}
      |</style></head>
      |<body>
      |""".stripMargin

  /** The end of the HTML document. */
  private val HtmlEnd: String =
    """<script>
      |// Add event listener to handle clicks on all buttons
      |document.querySelectorAll('button').forEach(button => {
      |  button.addEventListener('click', function() {
      |    navigator.clipboard.writeText(button.getAttribute('clipboard') || button.textContent);
      |    if (button.getAttribute('dest')) window.open(button.getAttribute('dest'), '_blank');
      |  });
      |});
      |</script>
      |</body></html>
      |""".stripMargin

  /** A button in HTML that can be clicked to copy text to the clipboard, optionally opening a new tab at the same time.
    *
    * @param label
    *   The label of the button.
    * @param clipboard
    *   If present, the text to copy to the clipboard when the button is clicked (otherwise the label is copied)
    * @param dest
    *   If present, pops up this URL in a new tab when the button is clicked.
    * @return
    *   The string that contains the button.
    */
  private def htmlButton(label: String, clipboard: String = "", dest: String = ""): String = {
    val sb = new StringBuilder("<button")
    if (clipboard.nonEmpty) sb ++= s""" clipboard="$clipboard""""
    if (dest.nonEmpty) sb ++= s""" dest="$dest""""
    sb ++= s">$label</button>"
    sb.toString
  }

  override def toString: String = {
    val sb = new StringBuilder(HtmlStart)
    sb ++= "<h1>Build failure notifications</h1>"
    report.allStepsByBuild
      .map { steps =>
        val buildButton = htmlButton(steps.head.buildDesc, steps.head.notifBuildMd)
        val buildLink = if (steps.head.buildLink.nonEmpty) s"""<a href="${steps.head.buildLink}">🔗</a>""" else ""
        val issuesButton =
          htmlButton(steps.map(_.issueTag).mkString(" "), steps.map(_.notifDetailMd).mkString("\n"))
        val issuesLink = steps
          .map(step =>
            (if (step.stepLink.nonEmpty) s"""<a href="${step.stepLink}">🔗</a>""" else "") +
              (if (step.issueLink.nonEmpty) s"""<a href="${step.issueLink}">🐞</a>""" else "")
          )
          .mkString(" · ")
        s"""<p>$buildButton $buildLink $issuesButton $issuesLink</p>""".stripMargin
      }
      .foreach(sb.addAll)
    sb ++= "<h1>By issue</h1>"
    report.allStepsByIssue
      .map { case issue -> steps =>
        val stepInfo =
          steps.map(step => s"* ${step.buildVersion} ${step.stepDesc} ${step.stepLink}").mkString("\n")
        val issueButton =
          htmlButton(
            if (issue.isEmpty) "Unknown" else issue,
            clipboard = stepInfo,
            if (issue.isEmpty) "" else report.IssueTemplate.format(issue)
          )
        val issueLink = s"""<a href="${report.IssueTemplate.format(issue)}">🐞</a>"""
        val buildsButton = htmlButton(stepInfo)
        s"<p>$issueButton $issueLink $buildsButton</p>"
      }
      .foreach(sb.addAll)
    sb ++= HtmlEnd
    sb.toString
  }
}

class ByIssueOutput(report: BuildFailureReport) {
  override def toString: String = {
    Header(
      "By Issue",
      1,
      report.allStepsByIssue.map { case issue -> steps =>
        Header(
          2,
          if (issue.isEmpty) "Unknown" else issue + " " + report.IssueTemplate.format(issue),
          Paragraph(steps.map(_.notifDetail).mkString("\n"))
        )
      }.toSeq
    ).build().toString
  }
}

class ByFailureOutput(report: BuildFailureReport) {
  override def toString: String = {
    Header(
      "By Failure",
      1,
      report.allStepsByBuild.map { steps =>
        Header(
          2,
          steps.head.notifBuildMd,
          Paragraph(steps.map(_.notifDetailMd).mkString("\n"))
        )
      }.toSeq
    ).build().toString
  }
}
object BuildFailureReportTask extends DocoptCliGo.Task {

  val Cmd = "buildfail"

  val Description = "Summarize a markdown report on build failures."

  val Doc: String =
    """Summarize a markdown report on build failures.
      |
      |Usage:
      |  MarkdGo buildfail FILE [options]
      |
      |Options:
      |  -h --help         Show this screen.
      |  --version         Show version.
      |  FILE              File to read and summarize
      |  --all             Report using all of the build investigations in the file
      |  --after=DATE      Exclude any investigations that occurred before DATE
      |  --until=DATE      Exclude any investigations that occurred after DATE
      |  --days=DAYS       The number of days to include in the report [default: 1]
      |  --html            If present, writes the text as an html file
      |  --add-fails=REPO  If present, adds any GitHub action fails
      |  --markdown-msg    If present, rewrites the investigations as markdown
      |                    notifications.
      |  --main-version=V  If present, uses V as the build version for main/master
      |
      |This has a very specific use, but is also a nice example for parsing and
      |generating markdown.  The input file should have a format like:
      |
      |  # Project Build Failures
      |
      |  ## YYYY-MM-DD The date the failure was investigated
      |
      |  ### 1.99 CI build description http://buildlink
      |
      |  Job or step description http://joblink
      |  ISSUE-1234 Issue description
      |
      |  ```
      |  Optional extra information in a code block of any size
      |  ```
      |
      |  Job or step description http://joblink
      |  ISSUE-1234 Issue description
      |
      |  Job or step description http://joblink
      |  ISSUE-1234 Issue description
      |
      |By default, only the last investigation date is reported (one day).  If --all
      |is specified, the entire file is used.
      |
      |""".stripMargin.trim

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: String = opts.get("FILE").asInstanceOf[String]
    // If --all is present, ignore the --day parameter to include ALL days.
    val filterDays: Int =
      if (opts.get("--all").toString.toBoolean) Int.MaxValue
      else Option(opts.get("--days")).map(_.toString.toInt).getOrElse(1)
    val filterAfter: Option[String] = Option(opts.get("--after")).map(_.toString)
    val filterUntil: Option[String] = Option(opts.get("--until")).map(_.toString)
    val asMarkdownMsg: Boolean = opts.get("--markdown-msg").toString.toBoolean
    val asHtml: Boolean = opts.get("--html").toString.toBoolean

    // Whether to modify the document by adding any new failed build before processing.
    val addFails: Option[String] = Option(opts.get("--add-fails")).map(_.toString)
    val mainVersion: Option[String] = Option(opts.get("--main-version")).map(_.toString)

    MarkdGo.processMd(Seq(files)) { f =>
      // Modify the file with new build failures on request
      val report0 = new BuildFailureReport(Header.parse(f.slurp()), filterDays, filterAfter, filterUntil)
      val report = addFails
        .map { repo =>
          val rep = report0.addNewFailures(repo, mainVersion)
          f.writeAll(rep.doc.build().toString)
          rep
        }
        .getOrElse(report0)

      // Print the requested output
      if (asHtml) print(new HtmlOutput(report).toString)
      else if (asMarkdownMsg) print(new ByFailureOutput(report).toString)
      else print(new ByIssueOutput(report).toString)
    }
  }
}
