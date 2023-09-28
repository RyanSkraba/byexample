package com.skraba.byexample.scala

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
  val IssueRegex: Regex = raw"([A-Z]+)-(\d+)".r

  /** Split on :: or , or ; punctuation. */
  val SplitRegex: Regex = raw"(::|,|;)".r

  // Constructing regular expressions

  describe("Using Regex") {
    // findAllIn
    // findAllMatchIn
    // findFirstIn
    // findFirstMatchIn
    // findPrefixOf
    // findPrefixMatchOf
    // replaceAllIn (x2)
    // replaceSomeIn
    // replaceFirstIn

    it("should find the first match anywhere in the string") {
      IssueRegex.findFirstIn("No match") shouldBe None
      IssueRegex.findFirstIn("BYEX-1234") shouldBe Some("BYEX-1234")
      IssueRegex.findFirstIn("This is BYEX-123.") shouldBe Some("BYEX-123")
      IssueRegex.findFirstIn("Not BYEX-123 but BYEX-234.") shouldBe Some(
        "BYEX-123"
      )
      IssueRegex.findFirstIn("byex-23 BYEX -123") shouldBe None
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
    }

    it("should split based on a regex") {
      SplitRegex.split("a::b,c") shouldBe Seq("a", "b", "c")
      SplitRegex.split("a:::b::c") shouldBe Seq("a", ":b", "c")
      SplitRegex.split("a::::b::c") shouldBe Seq("a", "", "b", "c")
      SplitRegex.split("::a::b,c") shouldBe Seq("", "a", "b", "c")
      SplitRegex.split("a::b,c::") shouldBe Seq("a", "b", "c")
    }
  }

  describe("Using Regex extractors in a match") {

    it("should extract groups to variables") {
      ("BYEX-1234" match {
        case IssueRegex(prj, num) => Some((prj, num))
        case _                    => None
      }) shouldBe Some("BYEX", "1234")
    }
  }
}
