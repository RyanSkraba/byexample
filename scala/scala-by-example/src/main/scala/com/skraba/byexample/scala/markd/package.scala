package com.skraba.byexample.scala

import scala.util.matching.Regex

/** Markd is a hierarchical snippet of text that can be used to parse, modify and write some
  * simple markdown files.
  *
  * The model is simple and includes many (but not all) features of markdown.
  *
  * You can clean a markdown file into a model then writing out again.
  */
package object markd {

  trait Markd {

    /** Write some whitespace before this element.
      *
      * @param sb   The builder to write to.
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

  object Markd {

    /** Splits text into strings ready to be placed into [[Comment]], [[Code]],
      * [[LinkRef]] and [[Paragraph]] instances.
      */
    private[this] val Pass1Regex: Regex =
      raw"""(?x)(?s)
            ( <!--(.*?)-->                                 # Comment
            | (?<=(^|\n))```(\S*)\s*\n(.*?)```\s*(\n|$$)   # Code
            | (?<=(^|\n))\s*(\[[^\n]*)                     # LinkRef
            | .*?(?=$$|<!--|```|\n\s*\[|\n\s*\n)           # All other text
            )
         """.r

    /** Parse the text between section headers into Markd instances.
      *
      * @param content The text inside the section.
      * @return a list of corresponding Markd instances.
      */
    def parse(content: String, cfg: ParserCfg = new ParserCfg()): Seq[Markd] = {

      // The first pass splits everything into code, comments, links and paragraphs
      val pass1 = Pass1Regex
        .findAllMatchIn(content)
        .flatMap {
          case Pass1Regex(_, _, _, code_type, code, _*) if code != null =>
            Seq(Code(code_type, code))
          case Pass1Regex(_, comment, _*) if comment != null =>
            Seq(Comment(comment))
          case Pass1Regex(_, _, _, _, _, _, _, linkRef, _*)
              if linkRef != null =>
            LinkRef.parse(linkRef)
          case Pass1Regex(all, _*) if !all.isBlank => Seq(Paragraph(all.trim))
          case _                                   => Nil
        }

      // Clean the links and put them at the end
      val (xs, linkRefs) = pass1
        .foldRight((List.empty[Markd], List.empty[LinkRef])) {
          case (md, (mds, refs)) =>
            md match {
              case ref: LinkRef => (mds, ref :: refs)
              case _            => (md :: mds, refs)
            }
        }

      xs ++ cfg.linkCleaner(linkRefs)
    }
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

  /** Contents outside of the markdown processing.
    *
    * {{{
    *   <!-- comment -->
    * }}}
    *
    * @param content the contents of the comment.
    */
  case class Comment(content: String) extends Markd {
    override def build(
        sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {
      sb ++= "<!--" ++= content ++= "-->\n"
    }
  }

  /** A fenced code block.
    *
    * {{{
    * ```bash
    * echo Hello world!
    * ```
    * }}}
    *
    * @param content the contents of the comment.
    */
  case class Code(code_type: String, content: String) extends Markd {
    override def build(
        sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {
      sb ++= "```" ++= code_type ++= "\n" ++= content ++= "```\n"
    }
  }

  /** A link reference.
    *
    * [ref]: https://link.url "Optional description"
    *
    * @param ref   the markdown tag used to reference the link
    * @param url   the url that is being linked to
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
    val LinkRegex: Regex =
      raw"""(?x)
          ^
          \s*\[(?<ref>[^\]]+)]:
          \s*(?<url>[^"].*?)?
          (\s*"(?<title>[^"]*?)")?
          \s*
          $$
          """.r

    /** Regex used to find Jira-style link references. */
    val JiraLinkRefRegex: Regex = raw"(\S+)-(\d+)".r

    /** Regex used to find Github PR-style link references. */
    val GithubPrLinkRefRegex: Regex = raw"(\S+)\s+PR#(\d+)".r

    def apply(ref: String, url: String): LinkRef = LinkRef(ref, Some(url), None)

    def apply(ref: String, url: String, title: String): LinkRef =
      LinkRef(ref, Some(url), Some(title))

    def parse(content: String): Option[LinkRef] = {
      LinkRef.LinkRegex
        .findFirstMatchIn(content)
        .map(m =>
          LinkRef(
            m.group("ref"),
            Option(m.group("url")).filter(!_.isBlank).map(_.trim),
            Option(m.group("title")).filter(!_.isBlank)
          )
        )
    }
  }

  /** An element that can contain other elements. */
  trait MultiMarkd extends Markd {

    type Self <: MultiMarkd

    /** The subelements of this element. */
    def sub: Seq[Markd]

    /** Write this element to the builder.
      *
      * @param sb   The builder to write to.
      * @param prev If known, the previous element written to the builder.  This can be used to
      *             adjust spacing.
      * @return The builder passed in.
      */
    def buildSub(
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

    /** Create a copy of the element with the new subelements.
      * @param newSub The subelements to replace the existing ones in the copy.
      */
    def copySub(newSub: Seq[Markd]): Self

    /** Create a copy of the list of subelements, replacing some as necessary.
      *
      * A partial function matches and replaces Markd subelements.  If the partial function is
      * defined for one of the subelements, it supplies the list of replacements.  It matches
      * on the element (or None to match the end of the list) and its index.
      *
      * @param filter True if non-matching subelements should be removed, false to leave
      *               non-matching elements unchanged.
      * @param pf A partial function to replace markd elements.
      * @return A copy of this [[MultiMarkd]] with the replaced subelements
      */
    def replaceInSub(
        filter: Boolean = false
    )(pf: PartialFunction[(Option[Markd], Int), Seq[Markd]]): Self = {
      // Elements undefined by the partial function should either be filtered from the results
      // or passed through without modification.
      val unmatched: PartialFunction[(Option[Markd], Int), Seq[Markd]] =
        if (filter) { case _ => Seq() }
        else { case (md, _) => md.toSeq }

      // Map the sub elements with the function, using None for the end.
      copySub(
        (sub.map { Option(_) }.zipWithIndex :+ (None, sub.size))
          .flatMap(pf orElse unmatched)
      )
    }

    /** Create a copy of the list of subelements, replacing the first one that matches.
      *
      * A partial function matches and replaces Markd subelements.  If the partial function is
      * defined for one of the subelements, it supplies the list of replacements.
      *
      * @param ifNotFound If nothing is matched, try again using this list instead.  This permits
      *                   "insert and update" if not found.
      * @param pf A partial function to replace markd elements.
      * @return A copy of this [[MultiMarkd]] with the replaced subelements
      */
    def replaceFirstInSub(ifNotFound: => Seq[Markd] = Seq.empty)(
        pf: PartialFunction[Markd, Seq[Markd]]
    ): Self = {
      copySub(
        Option(sub.indexWhere(pf.isDefinedAt))
          .filter(_ != -1)
          .map((_, sub))
          .orElse {
            // First fallback, use the ifNotFound instead.
            Option(ifNotFound.indexWhere(pf.isDefinedAt))
              .filter(_ != -1)
              .map((_, ifNotFound))
          }
          .map { case (idx, mds) =>
            mds.patch(idx, pf(mds(idx)), 1)
          }
          .getOrElse(sub)
      )
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
    * @param sub   The internal subsections and parsed [[Markd]] elements.
    */
  case class Header(title: String, level: Int, sub: Seq[Markd])
      extends MultiMarkd {

    type Self = Header

    override def copySub(newSub: Seq[Markd]): Self = copy(sub = newSub)

    override def build(
        sb: StringBuilder = new StringBuilder()
    ): StringBuilder = {
      level match {
        case 0 => // No title section for a document.
        case 1 => sb ++= title ++= "\n" ++= "=" * 78 ++= "\n"
        case 2 => sb ++= title ++= "\n" ++= "-" * 78 ++= "\n"
        case _ => sb ++= "#" * level ++= " " ++= title ++= "\n"
      }
      buildSub(sb, if (level == 0) None else Some(this))
    }
  }

  object Header {

    /** Regex used to split header section. */
    val HeaderRegex: Regex =
      raw"""(?x)
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
    def parse(content: String, cfg: ParserCfg = new ParserCfg()): Header = {
      // Split the entire contents into Markd elements as a flat list.
      val flat: Array[Markd] = HeaderRegex
        .split(content)
        .flatMap { text =>
          HeaderRegex.findPrefixMatchOf(s"$text\n") match {
            case None => Markd.parse(text, cfg)
            case Some(m: Regex.Match) =>
              val (level, title) = getHeaderLevelAndTitle(m)
              val lastMatchedGroup = 1 + m.subgroups.lastIndexWhere(_ != null)
              val headerContents = m.after(lastMatchedGroup).toString
              Header(level, title) +: Markd.parse(headerContents, cfg)
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

  /** Helps build the model when parsing contents. */
  class ParserCfg {

    /** Clean up the references at the end of a section. */
    def linkCleaner(links: Seq[LinkRef]): Seq[LinkRef] = links
  }
}
