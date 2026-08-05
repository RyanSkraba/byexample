package com.skraba.byexample.scalatools.gtd

import com.tinfoiled.markd.Table

/** When you're getting stuff done, you have projects to accomplish! This case class describes a project and gives it a
  * useful reference tag that can be used to automatically find the title, issue link and pull request link for your
  * project.
  *
  * For example, the "avro" tag might have the "Avro" (human-readable) title, the link to
  * https://github.com/apache/avro/pull/NNN for pull requests and https://issues.apache.org/AVRO-NNNN for bug reports.
  *
  * @param tag
  *   The short key to refer to the project, usually all lowercase.
  * @param title
  *   The title of the project to use in the weekly To Do, usually in CamelCase.
  * @param issueRef
  *   The text to use for a reference to an issue on this project.
  * @param issueLink
  *   The URL to use for an issue on this project. If this contains %s, the issue number will replace it, otherwise the
  *   issue number is appended.
  * @param prRef
  *   The text to use for a reference to a PR on this project.
  * @param prLink
  *   The URL to use for a pull request on this project. If this contains %s, the pull request number will replace it,
  *   otherwise the pull request number is appended.
  */
case class Project(tag: String, title: String, issueRef: String, issueLink: String, prRef: String, prLink: String) {
  private def replace(tmpl: String, in: String): String = if (tmpl.contains("%s")) tmpl.format(in) else tmpl + in
  def issueRefOf(in: String): String = replace(issueRef, in)
  def issueLinkOf(in: String): String = replace(issueLink, in)
  def prRefOf(in: String): String = replace(prRef, in)
  def prLinkOf(in: String): String = replace(prLink, in)
}

object Project {

  /** A helper method to either define or use defaults for the attributes.
    *
    * @param parse
    *   If true, attempts to find default values for any of the attributes that are empty.
    * @param tag
    *   The short key to refer to the project, usually all lowercase.
    * @param title
    *   The title of the project to use in the weekly To Do, usually in CamelCase. If it's not explicitly present, this
    *   is created from the tag.
    * @param issueRef
    *   The text to use for a reference to an issue on this project. If it's not explicitly present, this is created
    *   from the tag.
    * @param issueLink
    *   The URL to use for an issue on this project. If it's not explicitly present, this is created from the tag. If
    *   the issue ref ends with a #, we assume it is a GitHub link.
    * @param prRef
    *   The text to use for a reference to a PR on this project.
    * @param prLink
    *   The URL to use for a PR on this project. If the PR ref ends with a #, we assume it is a GitHub link.
    */
  def apply(
      parse: Boolean,
      tag: String,
      title: String = "",
      issueRef: String = "",
      issueLink: String = "",
      prRef: String = "",
      prLink: String = ""
  ): Project =
    if (parse) {
      // Apply defaults to the strings if they haven't been specified
      val title2 = if (title.nonEmpty) title else tag.toLowerCase.capitalize
      val issueRef2 = if (issueRef.nonEmpty) issueRef else tag.toUpperCase + "-"
      val issueLink2 =
        if (issueLink.nonEmpty) issueLink
        else if (looksLikeGitHub(issueRef2)) s"https://github.com/${issueRef2.dropRight(1)}/issues/"
        else s"https://issues.apache.org/jira/browse/${tag.toUpperCase}-"
      val prRef2 =
        if (prRef.nonEmpty) prRef else if (looksLikeGitHub(issueRef2)) issueRef2 else s"apache/${tag.toLowerCase}#"
      val prLink2 =
        if (prLink.nonEmpty) prLink
        else if (looksLikeGitHub(prRef2)) s"https://github.com/${prRef2.dropRight(1)}/pull/"
        else s"https://github.com/apache/${tag.toLowerCase}/pull/"
      Project(tag, title2, issueRef2, issueLink2, prRef2, prLink2)
    } else {
      // Otherwise use the attributes directly
      Project(tag, title, issueRef, issueLink, prRef, prLink)
    }

  /** @return true if the reference looks like a GitHub reference (org/repo#) */
  private def looksLikeGitHub(ref: String): Boolean = ref.endsWith("#") && ref.count(_ == '/') == 1

  /** Logical defaults for a root ASF project. */
  def asf(tag: String): Project = Project(parse = true, tag, "", "", "", "", "")
  def asf(tag: String, title: String, jira: String, repo: String): Project = Project(
    parse = true,
    tag = tag,
    title = title,
    issueRef = s"$jira-",
    issueLink = s"https://issues.apache.org/jira/browse/$jira-",
    prRef = s"apache/$repo#",
    prLink = s"https://github.com/apache/$repo/pull/"
  )

  def fromCfg(gtd: GettingThingsDone): Seq[Project] = gtd.cfg
    .flatMap(_.collectFirstRecursive {
      case tbl: Table if tbl.title == "Projects" =>
        for (row <- 1 until tbl.rowSize)
          yield Project(parse = true, tbl(0, row), tbl(1, row), tbl(2, row), tbl(3, row), tbl(4, row), tbl(5, row))
    })
    .getOrElse(Seq.empty)
}
