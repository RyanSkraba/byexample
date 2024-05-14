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
  val SplitHttpsRegex: Regex = raw"(.*?)((?<!\S)https?://\S*)".r

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
      buildVersion: String = "",
      buildDesc: String = "",
      buildLink: String = "",
      stepDesc: String = "",
      stepLink: String = "",
      issueTag: String = "",
      issueDesc: String = "",
      comment: Option[String] = None
  ) {

    /** Creates a copy of this class enriched with some build information from the build title
      *
      * @param in
      *   The CI build title
      * @return
      *   The buildVersion, buildDesc and buildLink to be put into a [[FailedStep]]
      */
    def addBuildInfo(in: String): FailedStep = {
      val (buildVersionAndDescription, buildLink) = in match {
        case SplitHttpsRegex(before, uri) => (before, uri)
        case _                            => (in, "")
      }
      val (buildVersion, buildDesc) = buildVersionAndDescription.span(!_.isWhitespace)
      copy(buildVersion = buildVersion.trim, buildDesc = buildDesc.trim, buildLink = buildLink.trim)
    }

    /** Creates a copy of this class enriched with some build information from the step analysis
      *
      * @param in
      *   A paragraph describing the build failure for the step
      * @return
      *   A tuple containing (in order) the stepDesc, stepLink, issueTag and issueDesc to be put into a [[FailedStep]]
      *   instance.
      */
    def addStepAndIssueInfo(in: String): FailedStep = {
      // Get the step information from the first line
      val lines = in.split('\n')
      val (stepDesc, stepLink) = lines.headOption match {
        case Some(SplitHttpsRegex(before, uri)) => (before, uri)
        case Some(str)                          => (str, "")
        case _                                  => ("", "")
      }
      // And get the issue information from the second line
      val (issueTag, issueDesc) = lines.drop(1).headOption.getOrElse("").span(!_.isWhitespace)
      copy(stepDesc = stepDesc.trim, stepLink = stepLink.trim, issueTag = issueTag.trim, issueDesc = issueDesc.trim)
    }
  }

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: String = opts.get("FILE").asInstanceOf[String]
    val chooseAll: Boolean = opts.get("--all").toString.toBoolean
    val chooseDays: Int = Option(opts.get("--days")).map(_.toString.toInt).getOrElse(1)
    val chooseAfter: Option[String] = Option(opts.get("--after")).map(_.toString)
    val chooseUntil: Option[String] = Option(opts.get("--until")).map(_.toString)

    MarkdGo.processMd(Seq(files)) { f =>
      val buildFailureSection: Header = Header
        .parse(f.slurp())
        .collectFirstRecursive {
          case section @ Header(title, 1, _) if title.toLowerCase.matches(raw".*\bbuild failures\b.*") => section
        }
        .getOrElse { throw new IllegalArgumentException("Can't find failure report") }

      // Limit the selection of dates if they are specified.
      lazy val dates = buildFailureSection.mds.collect { case Header(date, 2, _) => date }
      val minDate = chooseAfter.getOrElse(dates.minOption.getOrElse(""))
      val maxDate = chooseUntil.getOrElse(dates.maxOption.getOrElse(""))

      val results: Seq[Seq[Seq[FailedStep]]] = buildFailureSection.mds
        .collect {
          case daily @ Header(investigateDate, 2, _) if investigateDate >= minDate && investigateDate <= maxDate =>
            daily.mds.collect { case buildDetails @ Header(buildTitle, 3, _) =>
              val base = FailedStep(investigateDate).addBuildInfo(buildTitle)
              (buildDetails.mds.collect {
                case p: Paragraph => p
                case c: Code      => c
              } :+ Comment("Ignored"))
                .sliding(2)
                .flatMap {
                  case Seq(Paragraph(content), c: Code) =>
                    Some(base.addStepAndIssueInfo(content).copy(comment = Some(c.content)))
                  case Seq(Paragraph(content), _*) =>
                    Some(base.addStepAndIssueInfo(content))
                  case _ => None
                }
                .toSeq
            }
        }
        .take(if (chooseAll) dates.size else chooseDays)

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
