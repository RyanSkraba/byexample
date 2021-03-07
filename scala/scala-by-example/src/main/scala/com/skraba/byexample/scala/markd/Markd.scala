package com.skraba.byexample.scala.markd

import scala.util.matching.Regex

/** Markd is a hierarchical snippet of text that can be used to parse, modify and write some
  * simple markdown files.
  *
  * @param title The title of this snippet, if any.
  * @param text The text included in this snippet.
  * @param sub The sub-snippets that might be included inside this one.
  * @param linkRefs Any markdown link references at the end of the section.
  */
case class Markd(
    title: String,
    text: String = "",
    sub: Seq[Markd] = Seq(),
    linkRefs: Seq[String] = Seq()
) {

  def addLinkRef(
      ref: String,
      url: String = "",
      fixer: Seq[String] => Seq[String] = identity
  ): Markd = {
    copy(linkRefs = fixer(linkRefs :+ s"[$ref]: $url".trim))
  }

  def build(
      sb: StringBuilder = new StringBuilder(),
      createTitle: String => String = Markd.SectionH2Title
  ): StringBuilder = {
    if (text != "")
      sb ++= s"$text\n\n"

    for (sectionMd <- sub) {
      sb ++= s"${createTitle(sectionMd.title)}\n\n"
      sectionMd.build(sb)
    }

    if (linkRefs.nonEmpty)
      sb ++= linkRefs.mkString("", "\n", "\n\n")

    sb
  }
}

object Markd {

  /** Regex used to split H1 sections. */
  val SectionH1: Regex = raw"""(?x)
          (?=(^|\n)                    # Lookahead
            (
              (?<title1>[^\n]+)\n       # Multiline header
              (===+)
            |
              \#\s+(?<title2>[^\n]+)   # or single line header
            )
            (\n))
         """.r("", "", "title1", "", "title2")

  /** Regex used to split H2 sections. */
  val SectionH2: Regex = raw"""(?x)
          (?=(^|\n)                    # Lookahead
            (
              (?<title1>[^\n]+)\n       # Multiline header
              (---+)
            |
              \#\#\s+(?<title2>[^\n]+)   # or single line header
            )
            (\n))
         """.r("", "", "title1", "", "title2")

  /** The divider we want to use to generate H1 titles. */
  val SectionH1Title: String => String = title => title + "\n" + "=" * 78

  /** The divider we want to use to generate H2 titles. */
  val SectionH2Title: String => String = title => title + "\n" + "-" * 78

  /** Regex used to find link references. */
  val Link: Regex = raw"\s*\[([^\]]+)]:\s*(.*)".r

  /** Regex used to find Jira-style link references. */
  val JiraLink: Regex = raw"\s*\[(\S+)-(\d+)\]:\s*(.*)".r

  /** Regex used to find Github PR-style link references. */
  val GithubPrLink: Regex = raw"\s*\[(\S+)\s+PR#(\d+)\]:\s*(.*)".r

  def parse(
      title: String,
      contents: String,
      sectionSplitter: Regex = SectionH1
  ): Markd = {

    // Pull any links off of the bottom of these contents.
    val (links, prelinks) =
      contents.trim
        .split("\n")
        .reverse
        .span(Markd.Link.findFirstIn(_).isDefined)

    // Split the rest into text and subsections
    sectionSplitter
      .split(prelinks.reverse.mkString("\n"))
      .map { text =>
        sectionSplitter.findPrefixMatchOf(text) match {
          case None       => Markd(title = title, text = text.trim())
          case Some(sect) =>
            // The title is the first non-null named group starting with title
            val subTitle = sect.groupNames
              .filter(_.startsWith("title"))
              .sorted
              .find(sect.group(_) != null)
              .map(sect.group)
              .getOrElse("")
            val lastMatchedGroup = 1 + sect.subgroups.lastIndexWhere(_ != null)
            val subContents = sect.after(lastMatchedGroup).toString.trim()
            Markd(
              title = title,
              sub = Seq(
                Markd.parse(
                  title = subTitle,
                  contents = subContents,
                  sectionSplitter = SectionH2
                )
              )
            )
        }
      }
      // And merge them into one doc.
      .reduce { (md1, md2) =>
        Markd(
          title = md1.title,
          text = md1.text + md2.text,
          sub = md1.sub ++ md2.sub
        )
      }
      // While copying the links back in.
      .copy(linkRefs = links.reverse)
  }
}
