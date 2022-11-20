package com.skraba.byexample.scala.scalatest

import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success}

/** Handling options with the [[Either]] class.
  *
  * @see
  *   [[http://www.scalatest.org/user_guide/using_matchers]]
  */
class EitherSpec extends AnyFunSpecLike with Matchers {

  describe("Eithers") {
    val l123: Either[Int, String] = Left(123)
    val rAbc: Either[Int, String] = Right("ABC")

    it("can be asserted as Left or Right") {
      l123 shouldBe Left(123)
      l123 shouldBe 'left
      l123.left.get shouldBe 123
      rAbc shouldBe Right("ABC")
      rAbc shouldBe 'right
      rAbc.right.get shouldBe "ABC"

      // An exception is thrown if we try to get the wrong side
      intercept[NoSuchElementException] { l123.right.get }
      intercept[NoSuchElementException] { rAbc.left.get }
    }

    it("can use EitherValues for more complex expressions") {
      import org.scalatest.EitherValues._

      // Tests that something is defined then applies the test to the value
      l123.left.value should (be > 100 and be < 200)
      rAbc.right.value should (be > "AAA" and be < "BBB")

      intercept[TestFailedException] {
        l123.right.value
      }
      intercept[TestFailedException] {
        rAbc.left.value
      }
    }

    it("is right-biased in for comprehensions") {
      val rDef: Either[Int, String] = Right("DEF")
      val rGhi = Right("GHI"): Right[Int, String]
      val l456: Either[Int, String] = Left(456)

      // In a for comprehension, if all of the arguments are right, they can be used in the yield
      (for { x <- rAbc; y <- rDef; z <- rGhi } yield x + y + z) shouldBe Right(
        "ABCDEFGHI"
      )

      // If any turn out to be a left, the first one is the return value of the comprehension, no matter what
      (for { x <- l123; y <- rDef; z <- rGhi } yield x + y + z) shouldBe l123
      (for { x <- rAbc; y <- l123; z <- l456 } yield x + y + z) shouldBe l123
      (for { x <- rAbc; y <- rDef; z <- l123 } yield x + y + z) shouldBe l123

      // Guard expressions are not supported.
      """for { i <- rAbc if i > 0 } yield i""" shouldNot compile

      // Similarly, refutable patterns are not supported.
      """for (x: String <- rAbc) yield x""" shouldNot compile
    }

    it("is right-biased") {
      // Note: by convention, right is success and left is the failure value.
      l123.getOrElse("HEY") shouldBe "HEY"
      rAbc.getOrElse("HEY") shouldBe "ABC"

      val rightInt: Either[Int, Int] = Right(123)
      val leftInt: Either[Int, Int] = Left(123)

      // Note that map and flatMap don't change the value of Lefts
      rightInt.map(_ + 1) shouldBe Right(124)
      leftInt.map(_ + 1) shouldBe Left(123)
      rightInt.flatMap(v => Right(v + 1)) shouldBe Right(124)
      rightInt.flatMap(v => Left(v + 1)) shouldBe Left(124)
      leftInt.flatMap(v => Right(v + 1)) shouldBe leftInt
      leftInt.flatMap(v => Left(v + 1)) shouldBe leftInt

      // Lefts are ignored even for contains and filters
      rightInt.contains(123) shouldBe true
      leftInt.contains(123) shouldBe false
      rightInt.filterOrElse(_ == 123, -1) shouldBe Right(123)
      rightInt.filterOrElse(_ == 124, -1) shouldBe Left(-1)
      leftInt.filterOrElse(_ == 123, -1) shouldBe Left(123)

      // foreach only operates on rights
      val acc = new StringBuilder()
      rightInt.foreach(acc.append)
      acc.mkString shouldBe "123"
      acc.clear()
      leftInt.foreach(acc.append)
      acc.mkString shouldBe ""

      // Weirdly enough, forall is true for lefts
      rightInt.forall(_ == 123) shouldBe true
      rightInt.forall(_ == 124) shouldBe false
      leftInt.forall(_ == 123) shouldBe true
      leftInt.forall(_ == 124) shouldBe true
    }

    it("can use turn a left or right into a known type") {
      // Fold provides two methods that can either turn a left or a right value into a common type.
      // The first method acts on the left, and the second acts on the right.
      // They both have a Int return value.
      l123.fold(_ + 1, _.length) shouldBe 124
      rAbc.fold(_ + 1, _.length) shouldBe 3
    }

    it("can be an try") {
      val good: Either[Exception, Int] = Right(100)
      val bad: Either[Exception, Int] = Left(new IllegalArgumentException())
      good.toTry shouldBe Success(100)
      bad.toTry shouldBe Failure(bad.left.get)
    }
  }
}
