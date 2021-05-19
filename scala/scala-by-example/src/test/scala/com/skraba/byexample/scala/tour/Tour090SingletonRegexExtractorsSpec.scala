package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour090SingletonRegexExtractorsSpec extends AnyFunSpecLike with Matchers {

  describe("Singletons and objects") {

    // Shorthand for creating a single immutable val at the level it is defined.
    // Usually at the same level as a class (known as companion object), but can be anywhere.
    object DeviceA {
      val MaxWidth: Int = 750
    }

    // A singleton object can extend classes and traits.
    trait ThreeDimensional {
      val MaxDepth: Int = 10
    }

    object DeviceB extends ThreeDimensional {
      val MaxWidth: Int = 1000
    }

    class Device(val name: String) {
      import Device._

      def secretExposed(): Int = secret + name.length
    }

    object Device {
      // Only visible to the companion class.
      private def secret: Int = superSecret() + 2

      // Visible to nobody but this object.
      private[this] def superSecret(): Int = 40

      def apply(name: String): Device = new Device(name)
    }

    it("provide simple constants and static methods") {
      DeviceA.MaxWidth shouldBe 750
      DeviceB.MaxWidth shouldBe 1000
      DeviceB.MaxDepth shouldBe 10
      DeviceA shouldBe a[DeviceA.type]
      DeviceB shouldBe a[DeviceB.type]
      DeviceB shouldBe a[ThreeDimensional]
    }

    it("use companion objects to access secrets and constructors") {
      // Use the apply in the companion object to build the instance.
      val deviceC: Device = Device("c")
      deviceC.name shouldBe "c"
      deviceC.secretExposed() shouldBe 43
      "print(deviceC.secret)" shouldNot compile
      "print(Device.secret)" shouldNot compile
    }
  }

  describe("Regex patterns") {
    import scala.util.matching.Regex

    // Group into the project and the issue number, like BYEX-1234
    val IssueRegex: Regex = raw"([A-Z]+)-(\d+)".r

    it("can be contructed from any string") {
      IssueRegex.findFirstIn("No match") shouldBe None
      IssueRegex.findFirstIn("BYEX-1234") shouldBe Some("BYEX-1234")
      IssueRegex.findFirstIn("This addresses BYEX-123.") shouldBe Some(
        "BYEX-123"
      )
      // None of these match.
      IssueRegex.findFirstIn("byex-23 BYEX -123") shouldBe None
    }

    it("can use groups") {
      val input: String =
        "BYEX-123 AVRO-1234 BEAM-4321 byex-1 BYEX- -1234 - ignored"

      // Iterating over all matches to find the project
      val allProjects = {
        for (patternMatch <- IssueRegex.findAllMatchIn(input))
          yield patternMatch.group(1)
      }.toList

      allProjects should have size 3
      allProjects should (contain("AVRO") and contain("BEAM"))
    }

    it("can be used as an extractor") {
      val IssueRegex(project, number) = "BYEX-1234"

      project shouldBe "BYEX"
      number shouldBe "1234"

      // When used as an extractor, the entire string must match.
      intercept[MatchError] {
        val IssueRegex(project, number) = "I fixed BYEX-123 and BYEX-124 today"
      }

      // If you just want to find one, then unanchor it.
      val unanchoredIssue = IssueRegex.unanchored
      val unanchoredIssue(project2, number2) =
        "Fixed BYEX-123 and BYEX-124 today"
      project2 shouldBe "BYEX"
      number2 shouldBe "123"

      unanchoredIssue.anchored shouldBe theSameInstanceAs(IssueRegex)
    }

    it("can be used as an extractor in match statements") {

      def matcher(s: String): String = s match {
        // Matching all groups
        case IssueRegex(prj, num) if num.startsWith("1") => s"BenfordsLaw"
        // Ignoring one of the values
        case IssueRegex(prj, _) if prj == "BYEX" => s"ByExample"
        // Match while ignoring groups
        case IssueRegex(_*) => "Valid"
        case _              => "Invalid"
      }

      matcher("BYEX") shouldBe "Invalid"
      matcher("AVRO-1234") shouldBe "BenfordsLaw"
      matcher("BYEX-234") shouldBe "ByExample"
      matcher("BEAM-999") shouldBe "Valid"
      // See unanchored for the reason.
      matcher("Fixed BYEX-123 and BYEX-124 today") shouldBe "Invalid"
    }
  }

  describe("Extractors") {

    it("can be used to create and extract info from instances.") {

      object Issue {
        // Apply takes parameters and returns any object, often the companion
        def apply(project: String, number: Int) = s"$project-$number"

        // Unapply takes that object and returns parameters.
        def unapply(customerID: String): Option[(String, Int)] = try {
          customerID.split("-") match {
            case Array(project, number) if project.nonEmpty =>
              Some((project, number.toInt))
            case _ => None
          }
        } catch {
          case e: NumberFormatException => None
        }
      }

      // Creating an Issue.  Here, it's encoded as a string, but usually will be a
      // composite object like a case class.
      val issue1: String = Issue("BYEX", 1234)
      issue1 shouldBe "BYEX-1234"

      val Issue(prj1, num1) = issue1
      prj1 shouldBe "BYEX"
      num1 shouldBe 1234

      // Throws a match error if it can't be unapplied.
      intercept[MatchError] { val Issue(prjX, _) = "-1234" }
      intercept[MatchError] { val Issue(prjX, numX) = "BYEX1234" }
      intercept[MatchError] { val Issue(prjX, numX) = "BYEX-123X" }
    }

    it("returns a boolean to test") {

      object SecretCode {
        def apply() = "abc123"
        def unapply(name: String): Boolean = name.equals("abc123")
      }

      // SecretCode() creates the instance, in this case a string. It could be
      // a much more advanced data container.
      val x = SecretCode()
      x shouldBe a[String]
      x shouldBe "abc123"

      def attempt(key: String) = key match {
        // The unapply doesn't extract anything but tests the value.  In this case, it
        // could be a much more advanced test.
        case SecretCode() => "Unlocked"
        case _            => "LOCKED"
      }

      attempt(x) shouldBe "Unlocked"
      attempt("HACKED") shouldBe "LOCKED"
    }

    it("can use more than one argument") {
      object CustomerID {

        // Apply takes arguments and returns an object.
        def apply(name: String, id: String) = s"$name--$id"

        // Unapply takes an object and returns arguments.
        def unapply(customerID: String): Option[(String, String)] = {
          val values = customerID.split("--")
          if (values.length == 2) Some((values(0), values(1))) else None
        }
      }

      val customer1ID = CustomerID("Sukyoung", "12345") // Sukyoung--12345
      val x = customer1ID match {
        case CustomerID(name, id) =>
          s"The name is $name." // The name is Sukyoung.
        case _ => "Could not match."
      }
      x shouldBe "The name is Sukyoung."

      val customer2ID = CustomerID("Nico", "54321")
      val CustomerID(name2, id2) = customer2ID
      // Equivalent to:
      // val name = CustomerID.unapply(customer2ID).get
      customer2ID shouldBe "Nico--54321"
      name2 shouldBe "Nico"
      id2 shouldBe "54321"

      // Throws a match error if it can't be unapplied.
      intercept[MatchError] {
        val CustomerID(name2) = "Badguy"
      }
    }

    it("can use a list of arguments") {

      object CustomerID {

        // Apply takes arguments and returns an object.
        def apply(name: String, id: String) = s"$name--$id"

        // Unapply takes an object and returns arguments.
        def unapplySeq(customerID: String): Option[Seq[String]] = {
          Some(customerID.split("--"))
        }
      }

      val customer1ID = CustomerID("Sukyoung", "12345") // Sukyoung--12345
      val x = customer1ID match {
        case CustomerID(name, id) =>
          s"The name is $name." // The name is Sukyoung.
        case _ => "Could not match."
      }
      x shouldBe "The name is Sukyoung."

      val customer2ID = CustomerID("Nico", "54321")
      val CustomerID(name2, id2) = customer2ID
      // Equivalent to:
      // val name = CustomerID.unapply(customer2ID).get
      customer2ID shouldBe "Nico--54321"
      name2 shouldBe "Nico"
      id2 shouldBe "54321"
    }
  }
}
