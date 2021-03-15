package com.skraba.byexample.scala.markd

import scala.util.matching.Regex

/** Markd is a hierarchical snippet of text that can be used to parse, modify and write some
  * simple markdown files.
  */
trait Markd {
  def build(sb: StringBuilder = new StringBuilder()): StringBuilder = sb
}

trait MultiMarkd extends Markd {
  def sub: Seq[Markd]

  override def build(sb: StringBuilder = new StringBuilder()): StringBuilder = {
    super.build(sb)
    if (sub.nonEmpty) {
      sub.headOption.map(_.build(sb))
      for (md <- sub.tail) {
        sb ++= "\n"
        md.build(sb)
      }
    }
    sb
  }

}

/** A simple text paragraph of Markdown.
  *
  * @param content the text contents for the paragraph.
  */
case class Paragraph(content: String) extends Markd {
  override def build(sb: StringBuilder = new StringBuilder()): StringBuilder = {
    sb ++= content.trim() ++= "\n"
  }
}

object Paragraph {
  def parse(text: String): Seq[Markd] = {
    if (text.isBlank) Seq()
    else Seq(Paragraph(text.trim))
  }
}

/** A link reference.
  *
  * [ref]: https://link.url "Optional description"
  *
  * @param ref the markdown tag used to reference the link
  * @param url the url that is being linked to
  * @param title optionally a title or description of the link for hover text
  */
case class LinkRef(ref: String, url: String, title: String = "") extends Markd {
  override def build(sb: StringBuilder = new StringBuilder()): StringBuilder = {
    sb ++= "[" ++= ref ++= "]:"
    if (url.nonEmpty) sb ++= " " ++= url
    if (title.nonEmpty) sb += '"' ++= ref += '"'
    sb ++= "\n"
  }
}

object LinkRef {

  /** Regex used to find link references. */
  val LinkRegex: Regex = raw"\s*\[([^\]]+)]:\s*(.*)".r

  /** Regex used to find Jira-style link references. */
  val JiraLinkRegex: Regex = raw"\s*\[(\S+)-(\d+)\]:\s*(.*)".r

  /** Regex used to find Github PR-style link references. */
  val GithubPrLinkRegex: Regex = raw"\s*\[(\S+)\s+PR#(\d+)\]:\s*(.*)".r
}

/** Markdown header or section.
  *
  * # Header 1
  *
  * Header 2
  * --------
  *
  * ### Header 3
  *
  * @param level The level (from 1 to 9).  A level of 0 can be used to represent an entire document.
  * @param title The title of the section
  * @param sub The internal subsections and parsed [[Markd]] elements.
  */
case class Header(title: String, level: Int, sub: Seq[Markd])
    extends MultiMarkd {

  override def build(sb: StringBuilder = new StringBuilder()): StringBuilder = {
    level match {
      case 0 => // No title section for a document.
      case 1 => sb ++= title ++= "\n" ++= "=" * 78 ++= "\n"
      case 2 => sb ++= title ++= "\n" ++= "-" * 78 ++= "\n"
      case _ => sb ++= "#" * level ++= " " ++= title ++= "\n"
    }
    if (sub.nonEmpty && level > 0) {
      sb ++= "\n"
    }
    super.build(sb)
  }

}

object Header {

  /** Regex used to split header section. */
  val HeaderRegex: Regex = raw"""(?x)
          (?=(^|\n)                           # Lookahead
            (
              (?<titleml>[^\n]+)\n            # Multiline header
              (===+|---+)
            |
              (\#{1,9})\s+(?<titlesl>[^\n]+)  # or single line header
            )
            (\n))
         """.r(
    "",
    "",
    "title_ml",
    "level_ml",
    "level_sl",
    "title_sl"
  )

  def apply(level: Int, title: String, sub: Markd*): Header =
    Header(title, level, sub.toSeq)

  /** Extract the level and title from a matching header. */
  private[this] def getHeaderLevelAndTitle(m: Regex.Match): (Int, String) = {
    if (Option(m.group("title_ml")).isDefined)
      (if (m.group("level_ml").startsWith("=")) 1 else 2, m.group("title_ml"))
    else
      (m.group("level_sl").length, m.group("title_sl"))
  }

  /** Splits the content into sections, as a tree of headers. */
  def parse(content: String): Header = {
    // Split the entire contents into Markd elements as a flat list.
    val flat: Array[Markd] = HeaderRegex
      .split(content)
      .flatMap { text =>
        HeaderRegex.findPrefixMatchOf(s"$text\n") match {
          case None => Paragraph.parse(text)
          case Some(m: Regex.Match) =>
            val (level, title) = getHeaderLevelAndTitle(m)
            val lastMatchedGroup = 1 + m.subgroups.lastIndexWhere(_ != null)
            val headerContents = m.after(lastMatchedGroup).toString
            Header(level, title) +: Paragraph.parse(headerContents)
        }
      }

    // Recursive function that makes the flat list into a tree.
    def treeify(node: Header, flat: Seq[Markd]): (Header, Seq[Markd]) =
      flat.headOption match {
        // If the next element in the list is a sub-section (i.e. greater level)
        case Some(next: Header) if next.level > node.level => {
          // then the sub-section should be treeified, using as many elements as necessary from
          // the list.
          val (subsection, flatRemainder) = treeify(next, flat.tail)
          // Add the subsection to this node, and continue to treeify this node with the rest.
          treeify(node.copy(sub = node.sub :+ subsection), flatRemainder)
        }
        // If the next element in the list is a section of the same or lower level,
        // then just return, and it can be added to the current node's parent.
        case Some(_: Header) => (node, flat)
        // If the next element in the list is any other Markd, then just add it to this node.
        case Some(next) => treeify(node.copy(sub = node.sub :+ next), flat.tail)
        // Otherwise processing is complete.
        case _ => (node, Seq.empty)
      }

    treeify(Header(0, ""), flat)._1
  }
}

/** ******************************************************************************
  * OLD
  */

/** Markd is a hierarchical snippet of text that can be used to parse, modify and write some
  * simple markdown files.
  *
  * @param title The title of this snippet, if any.
  * @param text The text included in this snippet.
  * @param sub The sub-snippets that might be included inside this one.
  * @param linkRefs Any markdown link references at the end of the section.
  * @deprecated moving this logic to Markd
  */
case class Markdo(
    title: String,
    text: String = "",
    sub: Seq[Markdo] = Seq(),
    linkRefs: Seq[String] = Seq()
) {

  def addLinkRef(
      ref: String,
      url: String = "",
      fixer: Seq[String] => Seq[String] = identity
  ): Markdo = {
    copy(linkRefs = fixer(linkRefs :+ s"[$ref]: $url".trim))
  }

  def build(
      sb: StringBuilder = new StringBuilder(),
      createTitle: String => String = Markdo.SectionH2Title
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

object Markdo {

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
  ): Markdo = {

    // Pull any links off of the bottom of these contents.
    val (links, prelinks) =
      contents.trim
        .split("\n")
        .reverse
        .span(Markdo.Link.findFirstIn(_).isDefined)

    // Split the rest into text and subsections
    sectionSplitter
      .split(prelinks.reverse.mkString("\n"))
      .map { text =>
        sectionSplitter.findPrefixMatchOf(text) match {
          case None       => Markdo(title = title, text = text.trim())
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
            Markdo(
              title = title,
              sub = Seq(
                Markdo.parse(
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
        Markdo(
          title = md1.title,
          text = md1.text + md2.text,
          sub = md1.sub ++ md2.sub
        )
      }
      // While copying the links back in.
      .copy(linkRefs = links.reverse)
  }
}
