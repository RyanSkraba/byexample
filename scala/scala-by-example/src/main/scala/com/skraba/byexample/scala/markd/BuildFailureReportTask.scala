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
      |This has a very specific use, but is also a nice task example.
      |""".stripMargin.trim

  case class Investigation(
      date: String,
      buildVersion: String,
      buildDesc: String,
      buildLink: String,
      jobAndStep: String,
      jogLogLink: String,
      jira: String,
      jiraHint: String,
      comment: Option[String]
  )

  def go(opts: java.util.Map[String, AnyRef]): Unit = {

    val files: String = opts.get("FILE").asInstanceOf[String]

    MarkdGo.processMd(Seq(files)) { f =>
      val global: Header = Header
        .parse(f.slurp())
        .collectFirstRecursive { case global @ Header("Flink Build Failures", 1, _) => global }
        .getOrElse { throw new IllegalArgumentException("Can't find failure report") }

      val byJira: Map[String, Seq[Investigation]] =
        Map(
          "FLINK-12345" -> Seq(
            Investigation(
              date = "date",
              buildVersion = "buildVersion",
              buildDesc = "buildDesc",
              buildLink = "buildLink",
              jobAndStep = "jobAndStep",
              jogLogLink = "jogLogLink",
              jira = "jira",
              jiraHint = "jiraHint",
              comment = None
            )
          )
        )

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
