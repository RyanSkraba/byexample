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
      |  MarkdGo buildfail [--all] FILE
      |
      |Options:
      |  -h --help  Show this screen.
      |  --version  Show version.
      |  --all      Report using all of the build investigations in the file.
      |  FILE       File to read and summarize
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

  /** All of the information that was collected during a build failure investigation
    * @param date
    *   The date that the build failure was investigated (not the date it failed)
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
    * @param comment
    *   If present, additional comments in a code block to describe the investigation
    */
  case class FailedStep(
      date: String,
      buildVersion: String,
      buildDesc: String,
      buildLink: String,
      stepDesc: String,
      stepLink: String,
      issueTag: String,
      issueDesc: String,
      comment: Option[String]
  )

  object FailedStep {

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
      * @param stepAndIssueContent
      *   A paragraph of information about the job that should be parsed further
      * @param comment
      *   If present, additional comments in a code block to describe the investigation
      */
    def apply(
        date: String,
        buildVersion: String,
        buildDesc: String,
        buildLink: String,
        stepAndIssueContent: String,
        comment: Option[String]
    ): FailedStep = {
      val (stepDesc, stepLink, issueTag, issueDesc) = parseStepAndIssueContent(stepAndIssueContent.split('\n'): _*)
      FailedStep(
        date,
        buildVersion,
        buildDesc,
        buildLink,
        stepDesc,
        stepLink,
        issueTag,
        issueDesc,
        comment
      )
    }

    /** Separates content about a specific build failure into some specific attributes
      *
      * @param in
      *   The CI build title
      * @return
      *   The buildVersion, buildDesc and buildLink to be put into a [[FailedStep]]
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
      *
      * @param in
      *   A paragraph describing the build failure
      * @return
      *   A tuple containing (in order) the stepDesc, stepLink, issueTag and issueDesc to be put into a [[FailedStep]]
      *   instance.
      */
    def parseStepAndIssueContent(in: String*): (String, String, String, String) = {
      // Get the jobAndStep and jobLinkLog from the first line
      val (stepDesc, stepLink) = in.headOption match {
        case Some(SplitHttpsRegex(before, uri)) => (before, uri)
        case Some(str)                          => (str, "")
        case _                                  => ("", "")
      }

      // And get the issue information from the second line
      val (issueTag, issueDesc) = in.drop(1).headOption.getOrElse("").span(!_.isWhitespace)
      (stepDesc.trim, stepLink.trim, issueTag.trim, issueDesc.trim)
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: String = opts.get("FILE").asInstanceOf[String]
    val chooseAll: Boolean = opts.get("--all").toString.toBoolean

    MarkdGo.processMd(Seq(files)) { f =>
      val global: Header = Header
        .parse(f.slurp())
        .collectFirstRecursive { case global @ Header("Flink Build Failures", 1, _) => global }
        .getOrElse { throw new IllegalArgumentException("Can't find failure report") }

      val byInvestigationDate: Seq[Seq[Seq[FailedStep]]] = global.mds
        .collect { case daily @ Header(investigateDate, 2, _) =>
          daily.mds.collect { case buildDetails @ Header(buildTitle, 3, _) =>
            val (buildVersion, buildDescription, buildLink) = FailedStep.parseBuildTitle(buildTitle)

            (buildDetails.mds.collect {
              case p: Paragraph => p
              case c: Code      => c
            } :+ Comment("Ignored"))
              .sliding(2)
              .flatMap {
                case Seq(Paragraph(content), c: Code) =>
                  Some(
                    FailedStep(investigateDate, buildVersion, buildDescription, buildLink, content, Some(c.content))
                  )
                case Seq(Paragraph(content), _*) =>
                  Some(FailedStep(investigateDate, buildVersion, buildDescription, buildLink, content, None))
                case _ => None
              }
              .toSeq
          }
        }

      val results: Seq[Seq[Seq[FailedStep]]] = if (chooseAll) {
        byInvestigationDate
      } else {
        byInvestigationDate.take(1)
      }

      // Sort by the issue reference
      val byIssue = SortedMap(
        results.flatten.flatten
          .groupBy(_.issueTag)
          .toList: _*
      )

      val output: Header = Header(
        "By Issue",
        1,
        byIssue.map { case issue -> list =>
          Header(
            2,
            s"$issue https://issues.apache.org/jira/browse/$issue",
            Paragraph(list.map(inv => s"* ${inv.buildVersion} ${inv.stepDesc} ${inv.stepLink}").mkString("\n"))
          )
        }.toSeq
      )

      print(output.build().toString)
    }
  }
}
