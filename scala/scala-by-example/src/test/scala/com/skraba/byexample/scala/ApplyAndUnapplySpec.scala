package com.skraba.byexample.scala

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** The apply and unapply methods are used to create instances and pattern match.
  *
  * @see https://docs.scala-lang.org/overviews/scala-book/companion-objects.html
  * @see https://dzone.com/articles/eta-expansion-and-partially-applied-functions-in-s
  */
class ApplyAndUnapplySpec extends AnyFunSpecLike with Matchers {

  /** A case class is created with a default apply function. */
  case class Person(name: String, birthYear: Int)

  object Person {

    /** A custom apply to create an anonymous person, with the age rounded down to the decade. */
    def apply(birthYear: Int): Person =
      Person("Anonymous", birthYear / 10 * 10)
  }

  /** A normal class can have an apply in the companion object. */
  class Department(val name: String, val code: String)

  object Department {
    def apply(name: String, code: String): Department =
      new Department(name, code)
  }

  describe("Using apply") {

    it("creates an instance with the default case class apply") {
      // These are equivalent
      val p = Person.apply("Betty White", 1922)
      val p2 = Person("Betty White", 1922)

      // And the instances are equal
      p shouldBe p2
      p.name shouldBe "Betty White"
      p.birthYear shouldBe 1922
    }

    it("creates an instance with the customized case class apply") {
      val anon = Person(1945)
      anon should be(Person("Anonymous", 1940))
    }

    it("creates an instance with the class apply") {
      // These are equivalent
      val d = Department.apply("Finance", "FIN001")
      val d2 = Department("Finance", "FIN001")

      // And the instances are the same, but not equals (unlike case classes, there is no default
      // equals method for normal classes).
      d.name shouldBe "Finance"
      d.code shouldBe "FIN001"
      d2.name shouldBe d.name
      d2.code shouldBe d.code
      d should not be d2
    }

    it("creates an instance from a tuple.") {
      val t = ("Ruth McClanahan", 1934)

      // Turning a def into a val function is called Eta expansion.
      val personDefaultApply: (String, Int) => Person = Person.apply
      val personCustomApply: Int => Person = Person.apply
      val departmentApply: (String, String) => Department = Department.apply

      // These are all equivalent
      val p1 = personDefaultApply.apply(t._1, t._2)
      val p2 = personDefaultApply(t._1, t._2)
      val p3 = personDefaultApply tupled t
      p1 shouldBe p2
      p2 shouldBe p3

      // These are all equivalent
      val a1 = personCustomApply.apply(1934)
      val a2 = personCustomApply(1934)
      a1 shouldBe a2

      // These are all equivalent (but create non-equals instances).
      val d1 = departmentApply.apply("R&D", "RD002")
      val d2 = departmentApply("R&D", "RD002")
      val d3 = departmentApply tupled ("R&D", "RD002")
      d1 should not be d2
      d2 should not be d3
    }
  }

  describe("Using unapply") {

    it("creates an instance with the default case class unapply") {

      // The default case class unapply is a function that returns an optional tuple of its
      // apply arguments.
      Person.unapply _ shouldBe a[Person => Option[(String, Int)]]

      // So you can do things link this.
      val sophia = Person("Estelle Getty", 1923)
      val Person(extractName, extractYear) = sophia
      extractName shouldBe ("Estelle Getty")
      extractYear shouldBe 1923

      // Unapplying matches the name and sets the birth year internally.
      def birthday(goldenGirl: Person) =
        goldenGirl match {
          case Person("Bea Arthur", birthYear)  => s"May 13, $birthYear"
          case Person("Betty White", birthYear) => s"January 17, $birthYear"
          case Person("Estelle Getty", birthYear) if birthYear < 1923 =>
            "INVALID-SOPHIA"
          case Person("Estelle Getty", birthYear)  => s"July 25, $birthYear"
          case Person("Rue McClanahan", birthYear) => s"February 21, $birthYear"
          case _                                   => "INVALID-NOT-GOLDEN"
        }

      birthday(sophia) should be("July 25, 1923")
      // Actually, Estelle Getty was younger than Bea Arthur.
      birthday(Person("Estelle Getty", 1922)) should be("INVALID-SOPHIA")
      birthday(Person(1922)) should be("INVALID-NOT-GOLDEN")
    }
  }

}
