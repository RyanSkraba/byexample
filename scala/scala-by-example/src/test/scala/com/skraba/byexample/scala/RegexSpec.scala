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

  /** Match an issue tag with a project and an issue number, like BYEX-1234 */
  val IssueRegex: Regex = raw"\b(?<prj>[A-Z]+)-(?<num>\d+)\b".r

  /** Some common example strings to use with the IssueRegex. */
  val IssueExamples: Seq[String] = Seq(
    "",
    "No match",
    "BYEX-123",
    "pre BYEX-123",
    "BYEX-123 post",
    "pre BYEX-123 post",
    "** BYEX-123, BYEX-234 **",
    "- ByEx-123, BYEX-I23, BY3X-123 -",
    "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -"
  )

  /** Split on :: or , or ; punctuation. */
  val SplitRegex: Regex = raw"(::|,|;)".r

  describe("Using Regex for matching") {

    it("should match an entire or partial string") {
      IssueRegex.matches("BYEX-1234") shouldBe true
      IssueRegex.matches(" BYEX-1234 ") shouldBe false
      IssueRegex.matches("byex-1234") shouldBe false

      // In all of the examples, only the full string is matched
      IssueExamples.filter(IssueRegex.matches) shouldBe Seq("BYEX-123")

      // But unanchored checks anywhere in the string
      IssueRegex.unanchored.matches("Not BYEX-123 but BYEX-234.") shouldBe true
      IssueExamples.filter(IssueRegex.unanchored.matches) shouldBe Seq(
        "BYEX-123",
        "pre BYEX-123",
        "BYEX-123 post",
        "pre BYEX-123 post",
        "** BYEX-123, BYEX-234 **",
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -"
      )

      // Other constructions
      raw"[A-Z]+".r.matches("ABCDE") shouldBe true
    }

    it("should find the first match anywhere in the string") {
      // findFirstIn returns Option[String]
      IssueRegex.findFirstIn("No match") shouldBe None
      IssueRegex.findFirstIn("This is BYEX-123.") shouldBe Some("BYEX-123")
      IssueRegex.findFirstIn("** BYEX-123, BYEX-234 **") shouldBe Some(
        "BYEX-123"
      )

      // Over all the examples, excluding the non matches
      IssueExamples.flatMap(x =>
        IssueRegex.findFirstIn(x).map(x -> _)
      ) shouldBe Seq(
        "BYEX-123" -> "BYEX-123",
        "pre BYEX-123" -> "BYEX-123",
        "BYEX-123 post" -> "BYEX-123",
        "pre BYEX-123 post" -> "BYEX-123",
        "** BYEX-123, BYEX-234 **" -> "BYEX-123",
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -" -> "BYEX-123"
      )
    }

    it("should find information in the match") {
      // the findFirstMatch returns the instance describing the match, not the matching string
      val m = IssueRegex.findFirstMatchIn("** BYEX-123, BYEX-234 **")
      m.value.start shouldBe 3
      m.value.end shouldBe 11
      m.value.source shouldBe "** BYEX-123, BYEX-234 **"
      m.value.matched shouldBe "BYEX-123"
      m.value.groupCount shouldBe 2
      m.value.group(0) shouldBe "BYEX-123"
      m.value.group(1) shouldBe "BYEX"
      m.value.group(2) shouldBe "123"
      m.value.subgroups shouldBe List("BYEX", "123")
      m.value.group("prj") shouldBe "BYEX"
      m.value.group("num") shouldBe "123"
      m.value.before shouldBe "** "
      m.value.before(0) shouldBe "** "
      m.value.before(1) shouldBe "** "
      m.value.before(2) shouldBe "** BYEX-"
      m.value.after shouldBe ", BYEX-234 **"
      m.value.after(0) shouldBe ", BYEX-234 **"
      m.value.after(1) shouldBe "-123, BYEX-234 **"
      m.value.after(2) shouldBe ", BYEX-234 **"
    }

    it("should find all matches anywhere in the string") {
      // findAllIn returns Iterator[String]
      IssueRegex.findAllIn("No match") shouldBe empty
      IssueRegex.findAllIn("BYEX-123 and BYEX-234").toSeq shouldBe Seq(
        "BYEX-123",
        "BYEX-234"
      )

      // Over all the examples, excluding the non matches
      IssueExamples
        .map(x => x -> IssueRegex.findAllIn(x).toSeq)
        .filter(_._2.nonEmpty) shouldBe Seq(
        "BYEX-123" -> Seq("BYEX-123"),
        "pre BYEX-123" -> Seq("BYEX-123"),
        "BYEX-123 post" -> Seq("BYEX-123"),
        "pre BYEX-123 post" -> Seq("BYEX-123"),
        "** BYEX-123, BYEX-234 **" -> Seq("BYEX-123", "BYEX-234"),
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -" -> Seq("BYEX-123")
      )

      // returns Iterator[Match]
      val ms = IssueRegex.findAllMatchIn("Not BYEX-123 but BYEX-234.").toSeq
      ms should have size 2
      ms.collect { case Regex.Match(s) => s } shouldBe Seq(
        "BYEX-123",
        "BYEX-234"
      )
    }

    it("should find the first match if it's a prefix of the string") {
      // findPrefixOf returns Option[String]
      IssueRegex.findPrefixOf("No match") shouldBe None
      IssueRegex.findPrefixOf("BYEX-1234 matches") shouldBe Some("BYEX-1234")

      // Over all the examples, excluding the non matches
      IssueExamples.flatMap(x =>
        IssueRegex.findPrefixOf(x).map(x -> _)
      ) shouldBe Seq(
        "BYEX-123" -> "BYEX-123",
        "BYEX-123 post" -> "BYEX-123"
      )

      // findPrefixMatchOf returns Option[Match]
      IssueRegex
        .findPrefixMatchOf("BYEX-1234 matches")
        .value
        .matched shouldBe "BYEX-1234"
    }
  }

  describe("Using Regex groups") {

    /** Separates into three named groups */
    val Group3Regex: Regex =
      raw"(?<one>[A-Z]+)-(?<two>[A-Z]+)-(?<three>[A-Z]+)".r

    /** Separates into N groups. The inner group is named and repeated. */
    val RepeatedGroupsRegex: Regex = raw"((?<one>[A-Z]+)-?)+".r

    it("Should separate into three groups") {
      val m = Group3Regex.findFirstMatchIn("xxA-B-Cxx")
      m.value.matched shouldBe "A-B-C"
      m.value.groupCount shouldBe 3
      m.value.subgroups shouldBe Seq("A", "B", "C")
      m.value.group(0) shouldBe "A-B-C"
      m.value.group(1) shouldBe "A"
      m.value.group(2) shouldBe "B"
      m.value.group(3) shouldBe "C"
      m.value.group("one") shouldBe "A"
      m.value.group("two") shouldBe "B"
      m.value.group("three") shouldBe "C"
    }

    it("Should separate into N groups") {
      // When a group is repeated internally, we can only find the strictly LAST match
      val m = RepeatedGroupsRegex.findFirstMatchIn("xxA-B-Cxx")
      m.value.matched shouldBe "A-B-C"
      // There are two groups: the outer one that is repeated and the inner one containing the token
      m.value.groupCount shouldBe 2
      m.value.subgroups shouldBe Seq("C", "C")
      m.value.group(0) shouldBe "A-B-C"
      m.value.group(1) shouldBe "C"
      m.value.group(2) shouldBe "C"
      m.value.group("one") shouldBe "C"
    }
  }

  describe("Using a Regex to split and replace") {

    it("should replace first matches") {
      IssueRegex.replaceFirstIn("No match", "PRJ-000") shouldBe "No match"
      IssueRegex.replaceFirstIn(
        "BYEX-123 BYEX-234",
        "PRJ-000"
      ) shouldBe "PRJ-000 BYEX-234"

      // Over all the examples, excluding the unmodified ones
      IssueExamples
        .map(x => x -> IssueRegex.replaceFirstIn(x, "PRJ-000"))
        .filterNot(x => x._1 == x._2) shouldBe Seq(
        "BYEX-123" -> "PRJ-000",
        "pre BYEX-123" -> "pre PRJ-000",
        "BYEX-123 post" -> "PRJ-000 post",
        "pre BYEX-123 post" -> "pre PRJ-000 post",
        "** BYEX-123, BYEX-234 **" -> "** PRJ-000, BYEX-234 **",
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -" -> "- ByEx-123, PRJ-000, BYEX-23A, BY3X-234A -"
      )
    }

    it("should replace all matches") {
      IssueRegex.replaceAllIn("No match", "PRJ-000") shouldBe "No match"
      IssueRegex.replaceAllIn(
        "BYEX-123 BYEX-234",
        "PRJ-000"
      ) shouldBe "PRJ-000 PRJ-000"

      // Over all the examples, excluding the unmodified ones
      IssueExamples
        .map(x => x -> IssueRegex.replaceAllIn(x, "PRJ-000"))
        .filterNot(x => x._1 == x._2) shouldBe Seq(
        "BYEX-123" -> "PRJ-000",
        "pre BYEX-123" -> "pre PRJ-000",
        "BYEX-123 post" -> "PRJ-000 post",
        "pre BYEX-123 post" -> "pre PRJ-000 post",
        "** BYEX-123, BYEX-234 **" -> "** PRJ-000, PRJ-000 **",
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -" -> "- ByEx-123, PRJ-000, BYEX-23A, BY3X-234A -"
      )
    }

    it("should replace all matches using a function") {
      val fn: Regex.Match => String =
        m => s"${m.group("prj").reverse}-${m.group("num").toInt + 1}"

      IssueRegex.replaceAllIn("No match", fn) shouldBe "No match"
      IssueRegex.replaceAllIn("BYEX-1234", fn) shouldBe "XEYB-1235"
      IssueRegex.replaceAllIn(
        "BYEX-1234 matches",
        fn
      ) shouldBe "XEYB-1235 matches"
      IssueRegex.replaceAllIn(
        "This is XEYB-123.",
        fn
      ) shouldBe "This is BYEX-124."
      IssueRegex.replaceAllIn(
        "Not XEYB-123 but XEYB-234.",
        fn
      ) shouldBe "Not BYEX-124 but BYEX-235."
      IssueRegex.replaceAllIn(
        "byex-23 BYEX -123",
        fn
      ) shouldBe "byex-23 BYEX -123"
    }

    // replaceSomeIn

    it("should split a string") {
      // Endings tokens are thrown away, but middle empty tokens are kept
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
