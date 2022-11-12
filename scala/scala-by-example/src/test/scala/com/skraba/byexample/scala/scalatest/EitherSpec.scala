package com.skraba.byexample.scala.scalatest

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Handling options with the [[Either]] class.
  *
  * @see
  *   [[http://www.scalatest.org/user_guide/using_matchers]]
  */
class EitherSpec extends AnyFunSpecLike with Matchers {

  describe("Eithers") {
    val left: Either[Int, String] = Left(123)
    val right: Either[Int, String] = Right("ABC")

    it("can be asserted as Left or Right") {
      left shouldBe Left(123)
      left shouldBe 'left
      left.left.get shouldBe 123
      right shouldBe Right("ABC")
      right shouldBe 'right
      right.right.get shouldBe "ABC"

      // An exception is thrown if we try to get the wrong side
      intercept[NoSuchElementException] { left.right.get }
      intercept[NoSuchElementException] { right.left.get }

    }

    it("can use EitherValues for more complex expressions") {
      import org.scalatest.EitherValues._

      // Tests that something is defined then applies the test to the value
      left.left.value should (be > 100 and be < 200)
      right.right.value should (be > "AAA" and be < "BBB")

      intercept[TestFailedException] { left.right.value }
      intercept[TestFailedException] { right.left.value }
    }
  }
}
