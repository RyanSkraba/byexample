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

    it("is right-biased") {
      // Note: by convention, right is success and left is the failure value.

      val rightInt: Either[Int, Int] = Right(123)
      val leftInt: Either[Int, Int] = Left(123)

      // Note that map and flatMap don't change the value of Lefts
      rightInt.map(_ + 1) shouldBe Right(124)
      leftInt.map(_ + 1) shouldBe Left(123)
      rightInt.flatMap(v => Right(v + 1)) shouldBe Right(124)
      rightInt.flatMap(v => Left(v + 1)) shouldBe Left(124)
      leftInt.flatMap(v => Right(v + 1)) shouldBe leftInt
      leftInt.flatMap(v => Left(v + 1)) shouldBe leftInt
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
