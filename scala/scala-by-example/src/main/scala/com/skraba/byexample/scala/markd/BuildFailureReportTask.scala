package com.skraba.byexample.scala.markd

import com.skraba.docoptcli.DocoptCliGo

import scala.util.matching.Regex

object BuildFailureReportTask extends DocoptCliGo.Task {

  val Cmd = "buildfail"

  val Description = "Summarize a markdown report on build failures."

  val Doc: String =
    """Summarize a markdown report on build failures.
      |
      |Usage:
      |  MarkdGo buildfail FILE
      |
      |Options:
      |  -h --help       Show this screen.
      |  --version       Show version.
      |  FILE            File to read and summarize
      |
      |This has a very specific use, but is also a nice example for parsing and
      |generating markdown.  The input file should have a format like:
      |
      |```
      |# Project Build Failures
      |
      |## YYYY-MM-DD The date the failure was investigated
      |
      |### 1.99 CI build description
      |
      |Job or step description http://linksToLogs
      |JIRA-1234 Jira description
      |
      |Job or step description http://linksToLogs
      |JIRA-1234 Jira description
      |```
      |""".stripMargin.trim

  /** All of the information that was collected during a build failure investigation
    * @param date
    *   The date that the build failure was investigated (not the date it failed)
    * @param buildVersion
    *   The version that was being built
    * @param buildDesc
    *   A short description that the CI uses to refer to the build (workflow name)
    * @param buildLink
    *   The link to the CI build failure
    * @param jobAndStep
    *   The specific job and step in the build that failed
    * @param jogLogLink
    *   A link to the error in the build logs (if any)
    * @param jira
    *   The JIRA/defect that was identified to have caused the failure
    * @param jiraDesc
    *   A helpful hint to refer to the JIRA
    * @param comment
    *   If present, additional comments in a code block to add to the JIRA
    */
  case class FailedBuild(
      date: String,
      buildVersion: String,
      buildDesc: String,
      buildLink: String,
      jobAndStep: String,
      jogLogLink: String,
      jira: String,
      jiraDesc: String,
      comment: Option[String]
  )

  object FailedBuild {

    /** Regex for separating off an http(s) URL from the end of a string. */
    val SplitHttpsRegex: Regex = raw"(.*?)((?<!\S)https?://\S*)".r

    /** Construct a description of a failed build where
      * @param date
      *   The date that the build failure was investigated (not the date it failed)
      * @param buildVersion
      *   The version that was being built
      * @param buildDesc
      *   A short description that the CI uses to refer to the build (workflow name)
      * @param buildLink
      *   The link to the CI build failure
      * @param jobAndJiraInfo
      *   A paragraph of information about the job that should be parsed further
      * @param comment
      *   If present, additional comments in a code block to add to the JIRA
      */
    def apply(
        date: String,
        buildVersion: String,
        buildDesc: String,
        buildLink: String,
        jobAndJiraInfo: String,
        comment: Option[String]
    ): FailedBuild = {
      val (jobAndStep, jogLogLink, jira, jiraDesc) = parseJobAndJiraInfo(jobAndJiraInfo.split('\n'): _*)
      FailedBuild(
        date,
        buildVersion,
        buildDesc,
        buildLink,
        jobAndStep,
        jogLogLink,
        jira,
        jiraDesc,
        comment
      )
    }

    /** Separates content about a specific build failure into some specific attributes
      * @param in
      *   The CI build title
      * @return
      *   The buildVersion, buildDesc and buildLink to be put into a [[FailedBuild]]
      */
    def parseBuildTitle(in: String): (String, String, String) = {
      val (buildVersionAndDescription, buildLink) = in match {
        case SplitHttpsRegex(before, uri) => (before, uri)
        case _                            => (in, "")
      }
      val (buildVersion, buildDesc) = buildVersionAndDescription.span(!_.isWhitespace)
      (buildVersion.trim, buildDesc.trim, buildLink.trim)
    }

    /** Separates content about a specific build failure for a job step into some specific attributes.
      * @param in
      *   A paragraph describing the build failure
      * @return
      *   A tuple containing (in order) the jobAndStep, jobLogLink, jira and jiraDesc to be put into a [[FailedBuild]]
      *   instance.
      */
    def parseJobAndJiraInfo(in: String*): (String, String, String, String) = {
      // Get the jobAndStep and jobLinkLog from the first line
      val (jobAndStep, jogLogLink) = in.headOption match {
        case Some(SplitHttpsRegex(before, uri)) => (before, uri)
        case Some(str)                          => (str, "")
        case _                                  => ("", "")
      }

      // And get the jira information from the second line
      val (jira, jiraDesc) = in.drop(1).headOption.getOrElse("XXXXX").span(!_.isWhitespace)
      (jobAndStep.trim, jogLogLink.trim, jira.trim, jiraDesc.trim)
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: String = opts.get("FILE").asInstanceOf[String]

    MarkdGo.processMd(Seq(files)) { f =>
      val global: Header = Header
        .parse(f.slurp())
        .collectFirstRecursive { case global @ Header("Flink Build Failures", 1, _) => global }
        .getOrElse { throw new IllegalArgumentException("Can't find failure report") }

      val results = global.mds
        .collect { case daily @ Header(investigateDate, 2, _) =>
          daily.mds.collect { case buildDetails @ Header(buildTitle, 3, _) =>
            val (buildVersion, buildDescription, buildLink) = FailedBuild.parseBuildTitle(buildTitle)

            (buildDetails.mds.collect {
              case p: Paragraph => p
              case c: Code      => c
            } :+ Comment("Ignored"))
              .sliding(2)
              .flatMap {
                case Seq(Paragraph(content), c: Code) =>
                  Some(
                    FailedBuild(investigateDate, buildVersion, buildDescription, buildLink, content, Some(c.content))
                  )
                case Seq(Paragraph(content), _*) =>
                  Some(FailedBuild(investigateDate, buildVersion, buildDescription, buildLink, content, None))
                case _ => None
              }
          }
        }

      val byJira = results.flatten.flatten
        .groupBy(_.jira)

      val output: Header = Header(
        "By Jira",
        1,
        byJira.map { case jira -> list =>
          Header(
            2,
            s"$jira https://issues.apache.org/jira/browse/$jira",
            Paragraph(list.map(inv => s"* ${inv.buildVersion} ${inv.jobAndStep} ${inv.jogLogLink}").mkString("\n"))
          )
        }.toSeq
      )

      print(output.build().toString)
    }
  }
}
