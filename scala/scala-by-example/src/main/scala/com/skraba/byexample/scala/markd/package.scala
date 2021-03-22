package com.skraba.byexample.scala

import scala.util.matching.Regex

/** Markd is a hierarchical snippet of text that can be used to parse, modify and write some
  * simple markdown files.
  *
  * The model is simple and includes many (but not all) features of markdown.
  */
package object markd {

  trait Markd {

    /** Write some whitespace before this element.
      *
      * @param sb The builder to write to.
      * @param prev The element before this element (if any).
      * @return The builder passed in.
      */
    def buildPreSpace(
        sb: StringBuilder = new StringBuilder(),
        prev: Option[Markd]
    ): StringBuilder = if (prev.isDefined) sb ++= "\n" else sb

    /** Write this element to the builder.
      *
      * @param sb The builder to write to.
      * @return The builder passed in.
      */
    def build(sb: StringBuilder = new StringBuilder()): StringBuilder = sb
  }

  /** A simple text paragraph of Markdown.
    *
    * @param content the text contents for the paragraph.
    */
  case class Paragraph(content: String) extends Markd {
    override def build(
        sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {
      sb ++= content.trim() ++= "\n"
    }
  }

  object Paragraph {

    /** Parse section text (between section headers) into Markd instances.
      *
      * @param content The text inside the section.
      * @return a list of corresponding Markd instances.
      */
    def parse(content: String): Seq[Markd] = {
      // Extract all of the LinkRefs from the content.
      val (lines: Seq[String], links: Seq[Markd]) = content.trim
        .split("\n")
        .foldLeft((Seq.empty[String], Seq.empty[Markd])) { case ((a, b), s) =>
          LinkRef.LinkRegex
            .findFirstMatchIn(s)
            .map(m =>
              (
                a :+ "",
                b :+ LinkRef(
                  m.group("ref"),
                  Option(m.group("url")).filter(!_.isBlank).map(_.trim),
                  Option(m.group("title")).filter(!_.isBlank)
                )
              )
            )
            .getOrElse((a :+ s, b))
        }

      // TODO: The contents in the single paragraph still need to be cleaned.
      val nonlinks = lines.mkString("\n")
      if (nonlinks.isBlank) links
      else Paragraph(nonlinks.trim) +: links
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
  case class LinkRef(
      ref: String,
      url: Option[String] = None,
      title: Option[String] = None
  ) extends Markd {

    /** Don't space between LinkRefs */
    override def buildPreSpace(
        sb: StringBuilder = new StringBuilder(),
        prev: Option[Markd]
    ): StringBuilder = prev match {
      case Some(LinkRef(_, _, _)) => sb
      case _                      => super.buildPreSpace(sb, prev)
    }

    override def build(
        sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {
      sb ++= "[" ++= ref ++= "]:"
      url.filterNot(_.isBlank).map(sb ++= " " ++= _)
      title.filterNot(_.isBlank).map(sb ++= " \"" ++= _ += '"')
      sb ++= "\n"
    }
  }

  object LinkRef {

    /** Regex used to find link references. */
    val LinkRegex: Regex = raw"""(?x)
          ^
          \s*\[(?<ref>[^\]]+)]:
          \s*(?<url>[^"].*?)?
          (\s*"(?<title>[^"]*?)")?
          \s*
          $$
          """.r

    /** Regex used to find Jira-style link references. */
    val JiraLinkRegex: Regex = raw"\s*\[(\S+)-(\d+)\]:\s*(.*)".r

    /** Regex used to find Github PR-style link references. */
    val GithubPrLinkRegex: Regex = raw"\s*\[(\S+)\s+PR#(\d+)\]:\s*(.*)".r

    def apply(ref: String, url: String): LinkRef = LinkRef(ref, Some(url), None)
    def apply(ref: String, url: String, title: String): LinkRef =
      LinkRef(ref, Some(url), Some(title))
  }

  /** An element that can contain other elements. */
  trait MultiMarkd extends Markd {
    def sub: Seq[Markd]

    def subBuild(
        sb: StringBuilder = new StringBuilder(),
        prev: Option[Markd]
    ): StringBuilder = {
      if (sub.nonEmpty) {
        sub.headOption.map { head =>
          head.buildPreSpace(sb, prev)
          head.build(sb)
        }
        for (md: Seq[Markd] <- sub.sliding(2) if md.size == 2) {
          md.last.buildPreSpace(sb, Some(md.head))
          md.last.build(sb)
        }
      }
      sb
    }
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

    override def build(
        sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {
      level match {
        case 0 => // No title section for a document.
        case 1 => sb ++= title ++= "\n" ++= "=" * 78 ++= "\n"
        case 2 => sb ++= title ++= "\n" ++= "-" * 78 ++= "\n"
        case _ => sb ++= "#" * level ++= " " ++= title ++= "\n"
      }
      subBuild(sb, if (level == 0) None else Some(this))
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
          case Some(next: Header) if next.level > node.level =>
            // then the sub-section should be treeified, using as many elements as necessary from
            // the list.
            val (subsection, flatRemainder) = treeify(next, flat.tail)
            // Add the subsection to this node, and continue to treeify this node with the rest.
            treeify(node.copy(sub = node.sub :+ subsection), flatRemainder)
          // If the next element in the list is a section of the same or lower level,
          // then just return, and it can be added to the current node's parent.
          case Some(_: Header) => (node, flat)
          // If the next element in the list is any other Markd, then just add it to this node.
          case Some(next) =>
            treeify(node.copy(sub = node.sub :+ next), flat.tail)
          // Otherwise processing is complete.
          case _ => (node, Seq.empty)
        }

      treeify(Header(0, ""), flat)._1
    }
  }
}
