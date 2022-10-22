package com.skraba.byexample.scala.scalatest

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import funspec._
import org.scalatest.matchers.should._
import matchers.should.Matchers._

import scala.util.Try

/** Handling exceptions with the [[Try]] class.
  *
  * @see
  *   [[https://www.scalatest.org/user_guide/using_matchers#expectedExceptions]]
  * @see
  *   [[http://danielwestheide.com/blog/2012/12/26/the-neophytes-guide-to-scala-part-6-error-handling-with-try.html]]
  */
class TrySpec extends AnyFunSpecLike with Matchers {

  /** A bug id with a project and a number. */
  case class BugId(prj: String, num: Int = 1) {
    def next: BugId = if (num < Int.MaxValue) copy(num = num + 1)
    else throw InvalidBugIdException("Overflow")
    def prev: BugId = if (num > 1) copy(num = num - 1)
    else throw InvalidBugIdException("Underflow")
  }

  case class InvalidBugIdException(msg: String) extends Exception(msg)

  describe("Catching an exception in a test") {

    it("can be caught and wrapped") {
      // A classical way would be to write code wrapping the action under test with a try/catch block, and testing that the expected exception occurs
      val value =
        try {
          BugId("XYZ").prev.num
        } catch {
          case InvalidBugIdException(msg) =>
            msg shouldBe "Underflow"
            Int.MinValue
          case _: Throwable => fail()
        }
      value shouldBe Int.MinValue
    }

    it("can be expected") {
      // This doesn't see the exception but ensures that it was thrown
      assertThrows[InvalidBugIdException] {
        BugId("ABC").prev
      }

      an[InvalidBugIdException] should be thrownBy BugId("ABC").prev
      the[InvalidBugIdException] thrownBy {
        BugId("ABC").prev
      } should have message "Underflow"
      noException should be thrownBy BugId("ABC", 999).prev
    }

    it("can be intercepted") {
      // Or better yet, intercept and test the exception
      val ex = intercept[InvalidBugIdException] {
        BugId("ABC").prev
      }
      ex.msg shouldBe "Underflow"
    }
  }

  describe("Try type") {

    it("should be a failure") {
      val prevTry = Try(BugId("ABC").prev)
      prevTry.isFailure shouldBe true
      prevTry.isSuccess shouldBe false
    }

    it("should be recoverable") {
      val prevTry = Try(BugId("ABC").prev).recover { case _ => BugId("ABC", 999) }
      prevTry.isFailure shouldBe false
      prevTry.isSuccess shouldBe true
      prevTry.get.num shouldBe 999
    }

    it("recover can throw exception") {
      intercept[IllegalArgumentException] {
        Try(BugId("ABC").prev).recover {
          throw new IllegalArgumentException("Rewrite the exception")
        }
        fail()
      } should have message "Rewrite the exception"
    }
  }
}
