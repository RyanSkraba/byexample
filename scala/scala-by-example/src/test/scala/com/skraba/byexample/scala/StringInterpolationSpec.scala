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
