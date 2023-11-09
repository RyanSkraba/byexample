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

  describe("Using a raw-string or raw interpolator") {
    it("behaves as an s-string without converting literals") {
      val x = (".*", 456)
      // No interpolation, but literals are converted
      "${x._1}\n${x._2}" shouldBe "${x._1}\n${x._2}"
      // Interpolation, and literals are converted
      s"${x._1}\n${x._2}" shouldBe ".*\n456"
      s"${x._1}\n${x._2}".exists(_.isWhitespace) shouldBe true
      // Interpolation, and literals are not converted
      raw"${x._1}\n${x._2}" shouldBe ".*\\n456"
      raw"${x._1}\n${x._2}".exists(_.isWhitespace) shouldBe false
      // This is especially useful for regex!
      raw"${x._1}\n${x._2}".r matches ("123\n456") shouldBe true
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

    it("should format dates") {
      // Saturday, February 14, 2015 3:14:15.926 Pm
      val time = 1423926855926L

      // Standard 2 digits 24 hour clock
      f"$time%tH:$time%tM:$time%tS" shouldBe "16:14:15"
      f"$time%tT" shouldBe "16:14:15"
      f"$time%tR" shouldBe "16:14"
      f"$time%tT.$time%tL" shouldBe "16:14:15.926"
      f"$time%tT.$time%tN" shouldBe "16:14:15.926000000"

      // Standard 12 hour clock
      f"$time%tI:$time%tM:$time%tS $time%Tp" shouldBe "04:14:15 PM"
      f"$time%tr" shouldBe "04:14:15 PM"

      // Weird non-standard MM/DD/YYYY date that everyone hates
      f"$time%tm/$time%td/$time%ty" shouldBe "02/14/15"
      f"$time%tD" shouldBe "02/14/15"

      // ISO-8601
      f"$time%tY-$time%tm-$time%td" shouldBe "2015-02-14"
      f"$time%tF" shouldBe "2015-02-14"

      // Git-like dates (the time zone is taken from the local)
      f"$time%ta $time%tb $time%td $time%tT $time%tZ $time%tY" should fullyMatch regex "Sat Feb 14 16:14:15 .* 2015"
      f"$time%tc" should fullyMatch regex "Sat Feb 14 16:14:15 .* 2015"
    }
  }

}
