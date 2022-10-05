package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour070CaseClassesSpec extends AnyFunSpecLike with Matchers {

  case class Simple(id: Long, name: String)

  describe("Case classes") {

    it("are simple and immutable") {
      val one = Simple(1, "one")

      one shouldBe a[Simple]
      one.id shouldBe 1

      one.name shouldBe "one"
      // Immutability
      "one.id = 2" shouldNot compile
    }

    it("define equality nicely") {
      val one = Simple(1, "one")
      val other = Simple(1, "one")

      one shouldBe other
      one.hashCode shouldBe other.hashCode
    }

    it("create a nice toString") {
      val one = Simple(1, "one")

      one.toString shouldBe "Simple(1,one)"
    }

    it("have a nice copy constructor") {
      val one = Simple(1, "one")

      val un = one.copy(name = "un")

      un.id shouldBe 1
      un.name shouldBe "un"

      un shouldNot be(one)
      // Technically, this _could_ have been a collision, but in practice
      // it's not.
      un.hashCode shouldNot be(one.hashCode)
    }

    it("have an unapply") {
      val one = Simple(1, "one")

      // Unapply here
      val Simple(idx, _) = one
      idx shouldBe 1

      // But also in pattern matching
      def go(s: Simple): Char = s match {
        case Simple(d, "digit")               => d.toString.charAt(0)
        case Simple(1, name) if name.nonEmpty => name.charAt(0)
        case _                                => 'X'
      }

      go(one) shouldBe 'o'
      go(Simple(3, "digit")) shouldBe '3'
      go(Simple(2, "two")) shouldBe 'X'
    }
  }
}
