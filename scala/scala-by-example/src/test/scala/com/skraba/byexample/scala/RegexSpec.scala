package com.skraba.byexample.scala

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import java.io.ByteArrayInputStream
import java.nio.charset.StandardCharsets
import scala.sys.process._
import scala.util.{Success, Try}
import scala.util.matching.Regex

/** Using [[Regex]] in Scala.
  *
  * @see
  *   [[com.skraba.byexample.scala.tour.Tour090SingletonRegexExtractorsSpec]]
  */
class RegexSpec extends AnyFunSpecLike with Matchers {

  /** Group into the project and the issue number, like BYEX-1234 */
  val IssueRegex: Regex = raw"([A-Z]+)-(\d+)".r

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
    // split

    it("should find the first match anywhere in the string") {
      IssueRegex.findFirstIn("No match") shouldBe None
      IssueRegex.findFirstIn("BYEX-1234") shouldBe Some("BYEX-1234")
      IssueRegex.findFirstIn("This is BYEX-123.") shouldBe Some("BYEX-123")
      IssueRegex.findFirstIn("byex-23 BYEX -123") shouldBe None
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
