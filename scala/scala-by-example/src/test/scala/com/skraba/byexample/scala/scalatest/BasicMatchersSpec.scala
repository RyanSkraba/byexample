package com.skraba.byexample.scala.scalatest

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Basic matchers and assertions in the FunSpec style.
  *
  * @see
  *   http://www.scalatest.org/user_guide/using_matchers
  */
class BasicMatchersSpec extends AnyFunSpecLike with Matchers {

  describe("Basic matchers") {

    it("can test for equality") {
      val x: Int = 3

      // These are all ways that matchers can check for an equals value
      x shouldBe 3 // cannot customize equality, so fastest to compile, no parentheses required
      x shouldEqual 3 // can customize equality, no parentheses required
      x should equal(3) // can customize equality
      x should ===(3) // can customize equality and enforce type constraints
      x should be(3) // cannot customize equality, so fastest to compile

      x shouldNot be(2)

      assert(x == 3) // Any boolean condition
      assert(x === 3) // But === gives you a better error message.
    }

    it("can test for same instances") {
      // These are equal, but not the same instance
      val xs = Set("c", "b", "a")
      val ys = Set("a", "b", "c")
      xs shouldBe ys
      xs shouldNot be theSameInstanceAs ys
      // Does removing a non-existent element from the set return the same instance?
      // Apparently yes!
      xs - "x" should be theSameInstanceAs xs
    }

    it("can test for types equality") {
      val x: Int = 3
      x shouldBe an[Integer]
      x shouldBe a[Int]
      x shouldBe a[Number]
      x shouldBe an[AnyVal]

      def increase(i: Int) = i + 1
      // Actually Int => Int, but types are erased at runtime.
      increase _ shouldBe a[_ => _]
      increase(x) shouldBe a[Int]

      // You can use compilation to check the exact types
      "val check: Int => Int = increase" should compile
      // Or typeCheck for failures
      "val check: String => Int = increase" shouldNot compile
      "val check: String => Int = increase" shouldNot typeCheck
    }

    it("can test numbers to be close enough") {
      val approx: Double = 3.0
      approx shouldBe 2.9 +- 0.1
      approx should not be 2.9 +- 0.05
    }

    it("can test greater than / less than") {
      val x: Int = 3
      x should be < 5
      x should be > 0
      x should be <= 5
      x should be >= 0
      // For any comparable (note that the lower case comes after upper case)
      "Apples" should be < "Bananas"
      "apples" should be > "Bananas"
    }

    it("can be composed in boolean expressions") {
      val xs = Set(1, 2)

      // These expressions are evaluated left to right ("and" is not a priority) and do not short
      // circuit.
      xs.toSeq should (be(Seq(1, 2)) or be(Seq(2, 1)) and contain(1))
    }
  }

  describe("Exceptions") {
    it("can be intercepted") {
      val t = intercept[IndexOutOfBoundsException] {
        "hi" (10)
      }
      t.getMessage shouldBe "String index out of range: 10"
      t should have message "String index out of range: 10"
    }

    it("can be asserted with thrownBy") {
      an[IndexOutOfBoundsException] should be thrownBy "hi" (10)

      // Or looking at the Exception.
      the[IndexOutOfBoundsException] thrownBy "hi" (
        10
      ) should have message "String index out of range: 10"

      // Or capturing the exception for future use.
      val t2 = the[IndexOutOfBoundsException] thrownBy "hi" (10)
      t2.getMessage shouldBe "String index out of range: 10"
    }

    it("can be asserted to not happen") {
      noException should be thrownBy "hi" (0)
    }
  }

  describe("String matchers") {
    it("can check size and length") {
      val name = "Example"
      name should have size 7
      name should have length 7
    }

    it("can find substrings") {
      val name = "Example"
      name should startWith("Exa")
      name should endWith("ple")
      name should include("xam")
    }

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

  case class Person(name: String, birthYear: Int, golden: Boolean) {
    def getCentury: Int = birthYear / 100
  }

  describe("Property matchers") {

    val p = Person("Betty White", 1922, golden = true)

    it("can check properties") {
      p shouldBe 'golden
      p should have(
        'name("Betty White"),
        'birthYear(1922),
        'golden(true),
        'century(19)
      )
    }

    it("can pattern match") {
      // Either using inside
      import org.scalatest.Inside.inside
      inside(p) { case Person(first, _, _) =>
        first should startWith("B")
      }
      // Or patterns directly
      p should matchPattern { case Person("Betty White", _, _) => }
    }
  }

  describe("Boolean property matchers") {
    it("can call boolean methods") {
      // String has an isEmpty method that can be called and tested as a symbol
      "" shouldBe 'empty
      "xx" should not be 'empty

      // This works for any isXxxx property on any class, like isBlank
      "    \n\t" shouldBe 'blank
      "    \n\t" should be a 'blank
    }
  }

}
