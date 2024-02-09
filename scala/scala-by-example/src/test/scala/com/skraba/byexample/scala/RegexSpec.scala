package com.skraba.byexample.scala

import org.scalatest.OptionValues._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** Using [[Regex]] in Scala.
  *
  * @see
  *   [[com.skraba.byexample.scala.tour.Tour090SingletonRegexExtractorsSpec]]
  * @see
  *   [[https://regex101.com/]]
  * @see
  *   [[https://www.regular-expressions.info/]]
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

  describe("Using ScalaTest to test regex") {
    it("can match regular expressions") {
      val name = "Example"
      name should startWith regex "Ex.m"
      name should endWith regex "m.le"
      name should include regex "x.m"
      name should fullyMatch regex """[Ee].*[Ee]"""
    }

    it("can match regular expressions with groups") {
      "ExxampleStuff" should startWith regex ("E(x*).*(p.e)" withGroups ("xx", "ple"))
      "myExxxxampLe" should endWith regex ("E(x*).*(p.e)" withGroups ("xxxx", "pLe"))
      "myExxxampieStuff" should include regex ("E(x*).*(p.e)" withGroups ("xxx", "pie"))
      "Eamp&e" should fullyMatch regex ("E(x*).*(p.e)" withGroups ("", "p&e"))
    }
  }

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
      IssueExamples.flatMap(x => IssueRegex.findFirstIn(x).map(x -> _)) shouldBe Seq(
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
      IssueExamples.flatMap(x => IssueRegex.findPrefixOf(x).map(x -> _)) shouldBe Seq(
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
    val RepeatedGroupsRegex: Regex = raw"(-?(?<one>[A-Z]+))+".r

    it("Should separate into three named groups") {
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

    it("Should separate into repeated groups") {
      // When a group is repeated internally, we can only find the strictly LAST match
      val m = RepeatedGroupsRegex.findFirstMatchIn("xxA-B-Cxx")
      m.value.matched shouldBe "A-B-C"
      // There are two groups: the outer one that is repeated and the inner one containing the token
      m.value.groupCount shouldBe 2
      m.value.subgroups shouldBe Seq("-C", "C")
      m.value.group(0) shouldBe "A-B-C"
      m.value.group(1) shouldBe "-C"
      m.value.group(2) shouldBe "C"
      m.value.group("one") shouldBe "C"
    }

    it("should mix names and numbers") {
      // Named groups can be used in addition to their position
      val re = raw"(.)(?<one>.)(.)(?<two>.)".r
      val m = re.findFirstMatchIn("1234")
      m.value.group(1) shouldBe "1"
      m.value.group(2) shouldBe "2"
      m.value.group(3) shouldBe "3"
      m.value.group(4) shouldBe "4"
      m.value.group("one") shouldBe "2"
      m.value.group("two") shouldBe "4"
    }

    it("supports non-capturing groups") {
      // Similar to RepeatedGroupsRegex without names
      val re1 = raw"(-?([A-Z]+))+".r
      // Specifying the outer group as non-capturing
      val re2 = raw"(?:-?([A-Z]+))+".r

      val m1 = re1.findFirstMatchIn("BY-EX")
      m1.value.groupCount shouldBe 2
      m1.value.subgroups shouldBe Seq("-EX", "EX")
      val m2 = re2.findFirstMatchIn("BY-EX")
      m2.value.groupCount shouldBe 1
      m2.value.subgroups shouldBe Seq("EX")
    }

    it("supports backreference to groups") {
      // The same as Group3Regex but where the last group MUST be the same as the first
      val re1 = raw"(?<one>[A-Z]+)-(?<two>[A-Z]+)-\1".r

      Group3Regex.matches("A-B-C") shouldBe true
      re1.matches("A-B-C") shouldBe false
      re1.matches("A-B-A") shouldBe true
      re1.matches("XXX-B-XXX") shouldBe true

      // You can also do a backreference by name
      val re2 = raw"(?<one>[A-Z]+)-(?<two>[A-Z]+)-\k<two>".r
      re2.matches("A-B-A") shouldBe false
      re2.matches("A-B-B") shouldBe true
      re2.matches("A-XXX-XXX") shouldBe true
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

    it("should replace groups by number or name") {
      // The replacement text can refer to the entire match or the group by number or name
      IssueRegex.replaceAllIn("BYEX-123", "($0)") shouldBe "(BYEX-123)"
      IssueRegex.replaceAllIn("BYEX-123", "$2-$1") shouldBe "123-BYEX"
      IssueRegex.replaceAllIn("BYEX-123", "${num}-${prj}") shouldBe "123-BYEX"
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
      IssueRegex.replaceAllIn(
        "BYEX-123 BYEX-234",
        fn
      ) shouldBe "XEYB-124 XEYB-235"

      // Over all the examples, excluding the unmodified ones
      IssueExamples
        .map(x => x -> IssueRegex.replaceAllIn(x, fn))
        .filterNot(x => x._1 == x._2) shouldBe Seq(
        "BYEX-123" -> "XEYB-124",
        "pre BYEX-123" -> "pre XEYB-124",
        "BYEX-123 post" -> "XEYB-124 post",
        "pre BYEX-123 post" -> "pre XEYB-124 post",
        "** BYEX-123, BYEX-234 **" -> "** XEYB-124, XEYB-235 **",
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -" -> "- ByEx-123, XEYB-124, BYEX-23A, BY3X-234A -"
      )
    }

    it("should replace some matches using a function") {
      // This is similar to the preceding replaceAllIn example, but returns an option
      // If the return value is none, the replacement doesn't proceed.
      val fn: Regex.Match => Option[String] = { m =>
        if (m.matched.startsWith("PRJ-99")) None // Skip all PRJ-99* issues
        else Some(s"${m.group("prj").reverse}-${m.group("num").toInt + 1}")
      }

      IssueRegex.replaceSomeIn(
        "BYEX-999 BYEX-123 PRJ-112 PRJ-999",
        fn
      ) shouldBe "XEYB-1000 XEYB-124 JRP-113 PRJ-999"

      // Over all the examples, excluding the unmodified ones
      IssueExamples
        .map(x => x -> IssueRegex.replaceSomeIn(x, fn))
        .filterNot(x => x._1 == x._2) shouldBe Seq(
        "BYEX-123" -> "XEYB-124",
        "pre BYEX-123" -> "pre XEYB-124",
        "BYEX-123 post" -> "XEYB-124 post",
        "pre BYEX-123 post" -> "pre XEYB-124 post",
        "** BYEX-123, BYEX-234 **" -> "** XEYB-124, XEYB-235 **",
        "- ByEx-123, BYEX-123, BYEX-23A, BY3X-234A -" -> "- ByEx-123, XEYB-124, BYEX-23A, BY3X-234A -"
      )
    }

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
