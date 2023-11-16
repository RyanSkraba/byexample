package com.skraba.byexample.scala

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.matching.Regex

/** String formats and interpolation in scala.
  *
  * @see
  *   [[java.util.Formatter]]
  * @see
  *   https://docs.scala-lang.org/scala3/book/string-interpolation.html
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
      // No argument index
      "%s %s".format("one", "two") shouldBe "one two"
      // With argument indices (both formats)
      "%1s %2s".format("one", "two") shouldBe "one two"
      "%2$s %1$s".format("one", "two") shouldBe "two one"
      // Mixed
      "%2$s %s".format("one", "two") shouldBe "two one"
      "%2$s %s %s".format("one", "two") shouldBe "two one two"
      "%2$s %s %2$s %s %1$s".format("one", "two") shouldBe "two one two two one"
    }

    it("should align strings") {
      "%1$-10s".format("one") shouldBe "one       "
      "%1$10s".format("one") shouldBe "       one"
    }

    it("should resolve booleans") {
      "%b %b %b".format(None.orNull, true, false) shouldBe "false true false"
      "%b %b %b".format("false", "true", "") shouldBe "true true true"
      "%b %b %b".format(0, -1, math.Pi) shouldBe "true true true"
      "%B %B %B".format(None.orNull, true, false) shouldBe "FALSE TRUE FALSE"
    }

    it("should resolve hashcodes") {
      "%h %h %h".format(None.orNull, true, false) shouldBe "null 4cf 4d5"
      "%h %h %h".format("false", "true", "") shouldBe "5cb1923 36758e 0"
      "%h %h %h".format(0, -1, math.Pi) shouldBe "0 ffffffff 144d0ce3"
      "%H %H %H".format(None.orNull, true, false) shouldBe "NULL 4CF 4D5"
    }

    it("should resolve strings") {
      "%s %s %s".format(None.orNull, true, false) shouldBe "null true false"
      "%s %s %s".format("false", "true", "") shouldBe "false true "
      "%s %s %s".format(0, -1, math.Pi) shouldBe "0 -1 3.141592653589793"
      "%S %S %S".format(None.orNull, "hello", false) shouldBe "NULL HELLO FALSE"
    }

    it("should convert characters") {
      "%c %c %c".format(None.orNull, 66, 'a') shouldBe "null B a"
      "%c %c".format('\u0151', 'ő') shouldBe "ő ő"
      "%C %C %C".format(None.orNull, 66, 'a') shouldBe "NULL B A"
      "%C %C".format('\u0151', 'ő') shouldBe "Ő Ő"
    }

    it("should convert integers") {
      "%d %d %d".format(None.orNull, 3, -14) shouldBe "null 3 -14"
      "%o %o %o".format(None.orNull, 3, -14) shouldBe "null 3 37777777762"
      "%x %x %x".format(None.orNull, 3, -14) shouldBe "null 3 fffffff2"
      "%X %X %X".format(None.orNull, 3, -14) shouldBe "NULL 3 FFFFFFF2"
    }

    it("should convert floating points") {
      val pi = math.Pi
      val e = -math.E
      "%e %e".format(pi, e) shouldBe "3.141593e+00 -2.718282e+00"
      "%f %f".format(pi, e) shouldBe "3.141593 -2.718282"
      "%f %f".format(pi, e) shouldBe "3.141593 -2.718282"
      "%g %g".format(pi * 1e4, e * 1e5) shouldBe "31415.9 -271828"
      "%g %g".format(pi * 1e6, e * 1e7) shouldBe "3.14159e+06 -2.71828e+07"
      "%a %a".format(pi, e) shouldBe "0x1.921fb54442d18p1 -0x1.5bf0a8b145769p1"
      "%E %E".format(pi, e) shouldBe "3.141593E+00 -2.718282E+00"
      "%G %G".format(pi * 1e6, e * 1e7) shouldBe "3.14159E+06 -2.71828E+07"
      "%A %A".format(pi, e) shouldBe "0X1.921FB54442D18P1 -0X1.5BF0A8B145769P1"
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

  describe("Using a custom interpolator") {
    it("you can create a custom interpolator by providing an extension") {
      case class Issue(prj: String, num: Int)

      // This really should implement AnyVal
      implicit class IssueHelper(val sc: StringContext) {
        def i(args: Any*): Issue = {
          val xx = sc.parts.mkString.split("-")
          Issue(xx.head, xx(1).toInt)
        }
      }
      i"ABC-123".num shouldBe 123
      i"ABC-123".prj shouldBe "ABC"
    }
  }

}
