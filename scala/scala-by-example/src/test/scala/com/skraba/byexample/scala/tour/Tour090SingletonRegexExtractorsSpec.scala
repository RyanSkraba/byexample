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

    it("is simple") {
      DeviceA.MaxWidth shouldBe 750
      DeviceB.MaxWidth shouldBe 1000
      DeviceB.MaxDepth shouldBe 10
      DeviceA shouldBe a[DeviceA.type]
      DeviceB shouldBe a[DeviceB.type]
      DeviceB shouldBe a[ThreeDimensional]

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

    it("is simple") {
      // Any string turns into a Regex using .r
      val numberPattern: Regex = "[0-9]".r
      numberPattern.findFirstMatchIn("awesome123password") should not be empty
      numberPattern.findFirstMatchIn("awesomepassword") shouldBe empty
    }

    it("can use groups") {
      val keyValPattern: Regex = "([a-z-]+): (.+);".r

      val input: String =
        """background-color: #A03300;
          |background-image: url(img/header100.png);
          |background-position: top center;
          |background-repeat: repeat-x;
          |background-size: 2160px 108px;
          |margin: 0;
          |height: 108px;
          |width: 100%;""".stripMargin

      val x =
        for (patternMatch <- keyValPattern.findAllMatchIn(input))
          yield s"key: ${patternMatch.group(1)}"
      x.toList should (contain("key: background-color") and contain(
        "key: margin"
      ))
    }

    it("can be used as an extractor.") {
      val date: Regex = raw"(\d{4})-(\d{2})-(\d{2})".r

      // Used as an extractor and labelling the groups.
      val x = "2004-01-20" match {
        case date(year, month, day) => s"$year was a good year for PLs."
      }
      x shouldBe "2004 was a good year for PLs."

      // Used as an extractor but ignoring the groups.
      val x2 = "2004-01-20" match {
        case date(_*) => "It's a date!"
      }
      x2 shouldBe "It's a date!"

      // Used as an extractor but ignoring some groups.
      val x3 = "2004-01-20" match {
        case date(year, _*) => s"$year was a good year for PLs."
      }
      x3 shouldBe "2004 was a good year for PLs."

      // To match anywhere in the string in a case.
      val embeddedDate = date.unanchored
      val x4 =
        "Date: 2004-01-20 17:25:18 GMT (10 years, 28 weeks, 5 days, 17 hours and 51 minutes ago)" match {
          case embeddedDate("2004", "01", "20") => "A Scala is born."
        }
      x4 shouldBe "A Scala is born."
    }
  }

  describe("Extractor") {
    it("is simple") {
      import scala.util.Random

      object CustomerID {

        // Apply takes arguments and returns an object.
        def apply(name: String) = s"$name--${Random.nextLong}"

        // Unapply takes an object and returns arguments.
        def unapply(customerID: String): Option[String] = {
          val name = customerID.split("--").head
          if (name.nonEmpty) Some(name) else None
        }
      }

      val customer1ID = CustomerID("Sukyoung") // Sukyoung--23098234908
      val x = customer1ID match {
        case CustomerID(name) => s"The name is $name." // The name is Sukyoung.
        case _                => "Could not get name."
      }
      x shouldBe "The name is Sukyoung."

      val customer2ID = CustomerID("Nico")
      val CustomerID(name2) = customer2ID
      // Equivalent to:
      // val name = CustomerID.unapply(customer2ID).get
      customer2ID should startWith("Nico--")
      name2 shouldBe "Nico"

      // Throws a match error if it can't be unapplied.
      intercept[MatchError] {
        val CustomerID(_) = "--12345"
      }
    }

    it("returns a boolean") {
      object X {
        // Apply takes arguments and returns an object.
        def apply() = "12345"

        // Unapply takes an object and returns arguments.
        def unapply(name: String): Boolean = {
          name.equals("12345")
        }
      }

      val a = X()

      val x = a match {
        case X() => s"Is an X." // The name is Sukyoung.
        case _   => "Not an X."
      }
      a shouldBe "12345"
      x shouldBe "Is an X."
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
