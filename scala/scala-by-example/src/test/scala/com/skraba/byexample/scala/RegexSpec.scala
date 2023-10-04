package com.skraba.byexample.scala

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** Using [[Regex]] in Scala.
  *
  * @see
  *   [[com.skraba.byexample.scala.tour.Tour090SingletonRegexExtractorsSpec]]
  */
class RegexSpec extends AnyFunSpecLike with Matchers {

  /** Group into the project and the issue number, like BYEX-1234 */
  val IssueRegex: Regex = raw"(?<prj>[A-Z]+)-(?<num>\d+)".r

  /** Split on :: or , or ; punctuation. */
  val SplitRegex: Regex = raw"(::|,|;)".r

  // Constructing regular expressions

  describe("Using Regex") {

    // findPrefixOf
    // findPrefixMatchOf
    // replaceAllIn (x2)
    // replaceSomeIn
    // replaceFirstIn

    it("should find the first match anywhere in the string") {
      // findFirstIn returns Option[String]
      IssueRegex.findFirstIn("No match") shouldBe None
      IssueRegex.findFirstIn("BYEX-1234") shouldBe Some("BYEX-1234")
      IssueRegex.findFirstIn("This is BYEX-123.") shouldBe Some("BYEX-123")
      IssueRegex.findFirstIn("Not BYEX-123 but BYEX-234.") shouldBe Some(
        "BYEX-123"
      )
      IssueRegex.findFirstIn("byex-23 BYEX -123") shouldBe None
    }

    it("should find information in the match") {
      // the findFirstMatch returns the instance describing the match, not the matching string
      val m = IssueRegex.findFirstMatchIn("Not BYEX-123 but BYEX-234.")
      m.value.start shouldBe 4
      m.value.end shouldBe 12
      m.value.source shouldBe "Not BYEX-123 but BYEX-234."
      m.value.matched shouldBe "BYEX-123"
      m.value.groupCount shouldBe 2
      m.value.group(0) shouldBe "BYEX-123"
      m.value.group(1) shouldBe "BYEX"
      m.value.group(2) shouldBe "123"
      m.value.subgroups shouldBe List("BYEX", "123")
      m.value.group("prj") shouldBe "BYEX"
      m.value.group("num") shouldBe "123"
      m.value.before shouldBe "Not "
      m.value.before(0) shouldBe "Not "
      m.value.before(1) shouldBe "Not "
      m.value.before(2) shouldBe "Not BYEX-"
      m.value.after shouldBe " but BYEX-234."
      m.value.after(0) shouldBe " but BYEX-234."
      m.value.after(1) shouldBe "-123 but BYEX-234."
      m.value.after(2) shouldBe " but BYEX-234."
    }

    it("should find all matches anywhere in the string") {
      // findAllIn returns Iterator[String]
      IssueRegex.findAllIn("No match") shouldBe empty
      IssueRegex.findAllIn("BYEX-1234").toSeq shouldBe Seq("BYEX-1234")
      IssueRegex.findAllIn("This is BYEX-123.").toSeq shouldBe Seq("BYEX-123")
      IssueRegex.findAllIn("Not BYEX-123 but BYEX-234.").toSeq shouldBe Seq(
        "BYEX-123",
        "BYEX-234"
      )
      IssueRegex.findAllIn("byex-23 BYEX -123") shouldBe empty

      // returns Iterator[Match]
      val ms = IssueRegex.findAllMatchIn("Not BYEX-123 but BYEX-234.").toSeq
      ms should have size 2
      ms.collect { case Regex.Match(s) => s } shouldBe Seq(
        "BYEX-123",
        "BYEX-234"
      )
    }

    it("should split based on a regex") {
      // Endings tokens are thrown away
      SplitRegex.split("a::b,c") shouldBe Seq("a", "b", "c")
      SplitRegex.split("a:::b::c") shouldBe Seq("a", ":b", "c")
      SplitRegex.split("a::::b::c") shouldBe Seq("a", "", "b", "c")
      SplitRegex.split("::a::b,c") shouldBe Seq("", "a", "b", "c")
      SplitRegex.split("a::b,c::") shouldBe Seq("a", "b", "c")
    }
  }

  describe("Using Regex extractors in a match") {
    it("should extract groups to variables") {
      // Matching all the groups
      ("BYEX-1234" match {
        case IssueRegex(prj, num) => Some((prj, num))
        case _                    => None
      }) shouldBe Some("BYEX", "1234")

      // Unanchored matches
      val re = IssueRegex.unanchored
      ("My favourite bug is BYEX-1234 hooray!" match {
        case re(prj, num) => Some((prj, num))
        case _            => None
      }) shouldBe Some("BYEX", "1234")
    }
  }
}
