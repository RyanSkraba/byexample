package com.skraba.byexample.scala

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** String formats and interpolation in scala.
  *
  * @see
  *   [[java.util.Formatter]]
  */
class StringInterpolationSpec extends AnyFunSpecLike with Matchers {

  describe("Using an s-string or S interpolator") {
    it("should easily format strings and numbers") {
      val unit1 = "inch"
      val conversion = 2.54d
      val unit2 = "centimetre"
      s"$unit1 is $conversion $unit2" shouldBe "inch is 2.54 centimetre"
    }

    it("should easily escape dollar signs and execute arbitrary expressions") {
      val quantity = 4
      val price = 1.23
      s"At $$$price each, buying $quantity comes to $$${price * quantity}" shouldBe
        "At $1.23 each, buying 4 comes to $4.92"
    }
  }

  describe("Using a format from a string") {
    it("should order arguments correctly") {
      "%1s %2s".format("one", "two") shouldBe "one two"
      "%2$s %1$s".format("one", "two") shouldBe "two one"
    }

    it("should align strings") {
      "%1$-10s".format("one") shouldBe "one       "
      "%1$10s".format("one") shouldBe "       one"
    }
  }

  describe("Using an f-string or F interpolator") {
    it("should format strings and numbers") {
      val unit1 = "inch"
      val conversion = 2.54d
      val unit2 = "centimetre"
      f"$unit1%s is $conversion%2.1f $unit2%s" shouldBe "inch is 2.5 centimetre"
    }
  }

}
