package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.collection.SortedMap
import scala.util.matching.Regex

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
      |  -h --help     Show this screen.
      |  --version     Show version.
      |  FILE          File to read and summarize
      |  --all         Report using all of the build investigations in the file.
      |  --after=DATE  Exclude any investigations that occurred before DATE
      |  --until=DATE  Exclude any investigations that occurred after DATE
      |  --days=DAYS   The number of days to include in the report [default: 1]
      |  --html        If present, writes the text as an html file.
      |  --rewrite     If present, rewrites the investigations as markdown messages.
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

  /** Regex for separating off an http(s) URL from the end of a string. */
  val SplitHttpsRegex: Regex = raw"(.*?)((?<!\S)https?://\S+)\s*".r

  /** Regex for separating off an http(s) URL from the end of a string. */
  val SplitLastBracketRegex: Regex = raw"(.*?)(?<!\S)\((\S+)\)\s*".r

  val HtmlStart: String =
    """<!DOCTYPE html>
      |<html><head><style>
      |  button {white-space: pre-wrap;text-align:left;}
      |  a:active, a:hover, a:link, a:visited {text-decoration: none;}
      |</style></head>
      |<body>
      |""".stripMargin

  val HtmlEnd: String =
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

  val AsfIssueTemplate: String = "https://issues.apache.org/jira/browse/%s"

  def htmlButton(label: String, clipboard: String = "", dest: String = ""): String = {
    val sb = new StringBuilder("<button")
    if (clipboard.nonEmpty) sb ++= s""" clipboard="$clipboard""""
    if (dest.nonEmpty) sb ++= s""" dest="$dest""""
    sb ++= s">$label</button>"
    sb.toString
  }

  /** All of the information that was collected during a build failure investigation
    * @param investigateDate
    *   The date that the build failure was investigated (not the date it failed)
    * @param investigateOrder
    *   The order that the build failure was investigated
    * @param buildVersion
    *   The version that was being built
    * @param buildDesc
    *   A short description that the CI uses to refer to the build (workflow name)
    * @param buildLink
    *   The link to the CI build failure
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
      stepDesc: String = "",
      stepLink: String = "",
      issueTag: String = "",
      issueDesc: String = "",
      issueLink: String = "",
      comment: Option[String] = None
  ) {

    /** A string to use as a markdown notification for the build failure. */
    lazy val notifBuildMd: String = s":red_circle:  Build *[$buildVersion $buildDesc]($buildLink)* failed"

    /** A string to use as a markdown notification for this specific step. */
    lazy val notifDetailMd: String = s"* [$stepDesc]($stepLink) *[$issueTag]($issueLink)* $issueDesc"

    /** A string to use as a notification for this specific step (without formatting). */
    lazy val notifDetail: String = s"* $buildVersion $stepDesc $stepLink"

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

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: String = opts.get("FILE").asInstanceOf[String]
    val filterAll: Boolean = opts.get("--all").toString.toBoolean
    val filterDays: Int = Option(opts.get("--days")).map(_.toString.toInt).getOrElse(1)
    val filterAfter: Option[String] = Option(opts.get("--after")).map(_.toString)
    val filterUntil: Option[String] = Option(opts.get("--until")).map(_.toString)
    val rewrite: Boolean = opts.get("--rewrite").toString.toBoolean
    val asHtml: Boolean = opts.get("--html").toString.toBoolean
    // TODO: This could be configurable
    val issueTemplate = AsfIssueTemplate

    MarkdGo.processMd(Seq(files)) { f =>
      val buildFailureSection: Header = Header
        .parse(f.slurp())
        .collectFirstRecursive {
          case section @ Header(title, 1, _) if title.toLowerCase.matches(raw".*\bbuild failures\b.*") => section
        }
        .getOrElse { throw new IllegalArgumentException("Can't find failure report") }

      // Limit the selection of dates if they are specified.
      lazy val dates = buildFailureSection.mds.collect { case Header(date, 2, _) => date }
      val minDate = filterAfter.getOrElse(dates.minOption.getOrElse(""))
      val maxDate = filterUntil.getOrElse(dates.maxOption.getOrElse(""))

      val results: Seq[Seq[Seq[FailedStep]]] = buildFailureSection.mds
        .collect {
          case daily @ Header(investigateDate, 2, _) if investigateDate >= minDate && investigateDate <= maxDate =>
            daily.mds
              .collect { case buildDetails @ Header(buildTitle, 3, _) =>
                val base = FailedStep(investigateDate).addBuildInfo(buildTitle)
                (buildDetails.mds.collect {
                  case p: Paragraph => p
                  case c: Code      => c
                } :+ Comment("Ignored"))
                  .sliding(2)
                  .flatMap {
                    case Seq(Paragraph(content), c: Code) =>
                      Some(base.addStepAndIssueInfo(content, issueTemplate).copy(comment = Some(c.content)))
                    case Seq(Paragraph(content), _*) =>
                      Some(base.addStepAndIssueInfo(content, issueTemplate))
                    case _ => None
                  }
                  .toSeq
              }
              .map {
                // Add the order that the build was investigated every day
                _.zipWithIndex.map { case step -> index =>
                  step.copy(investigateOrder = index)
                }
              }
        }
        .take(if (filterAll) dates.size else filterDays)

      // Sort by the issue reference and create an output that includes all of the investigations
      lazy val byIssue = SortedMap(results.flatten.flatten.groupBy(_.issueTag).toList: _*)

      if (asHtml) {
        println(HtmlStart)
        println("<h1>Build failure notifications</h1>")
        results.flatten
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
          .foreach(println)

        /*
         * [Java 8 / Test (module: tests)](https://github.com/apache/flink/actions/runs/9110398985/job/25045798401#step:10:8192) *[FLINK-35382](https://issues.apache.org/jira/browse/FLINK-35382)* ChangelogCompatibilityITCase.testRestore fails with an NPE
         */

        println("<h1>By issue</h1>")
        byIssue
          .map { case issue -> steps =>
            val stepInfo =
              steps.map(step => s"* ${step.buildVersion} ${step.stepDesc} ${step.stepLink}").mkString("\n")
            val issueButton =
              htmlButton(issue, clipboard = stepInfo, dest = issueTemplate.format(issue))
            val issueLink = s"""<a href="${issueTemplate.format(issue)}">🐞</a>"""
            val buildsButton = htmlButton(stepInfo)
            s"<p>$issueButton $issueLink $buildsButton</p>"
          }
          .foreach(println)
        println(HtmlEnd)
      } else if (rewrite) {
        val output = Header(
          "By Failure",
          1,
          results.flatten.map { steps =>
            Header(
              2,
              steps.head.notifBuildMd,
              Paragraph(steps.map(_.notifDetailMd).mkString("\n"))
            )
          }
        )
        print(output.build().toString)
      } else {
        val outputByIssue: Header = Header(
          "By Issue",
          1,
          byIssue.map { case issue -> steps =>
            Header(
              2,
              issue + " " + issueTemplate.format(issue),
              Paragraph(steps.map(_.notifDetail).mkString("\n"))
            )
          }.toSeq
        )
        print(outputByIssue.build().toString)
      }
    }
  }
}
