package com.skraba.byexample.scala.scalatest

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest._
import funspec._
import org.scalatest.matchers.should._
import matchers.should.Matchers._

import scala.util.{Failure, Success, Try}

/** Handling exceptions with the [[Try]] class.
  *
  * @see
  *   [[https://www.scalatest.org/user_guide/using_matchers#expectedExceptions]]
  * @see
  *   [[http://danielwestheide.com/blog/2012/12/26/the-neophytes-guide-to-scala-part-6-error-handling-with-try.html]]
  */
class TrySpec extends AnyFunSpecLike with Matchers {

  /** A bug id with a project and a number. */
  case class BugId(prj: String, num: Int) {
    def next: BugId = if (num < Int.MaxValue) copy(num = num + 1)
    else throw InvalidBugIdException("Overflow")

    def prev: BugId = if (num > 1) copy(num = num - 1)
    else throw InvalidBugIdException("Underflow")

    /** Equivalent to [[prev]] but wrapped in a Try */
    def prevTry: Try[BugId] = Try(prev)
  }

  case class InvalidBugIdException(msg: String) extends Exception(msg)

  describe("Catching an exception in a test") {

    it("can be caught and wrapped") {
      // A classical way would be to write code wrapping the action under test with a try/catch block, and testing that the expected exception occurs
      val value =
        try {
          BugId("XYZ", 1).prev.num
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
        BugId("ABC", 1).prev
      }

      an[InvalidBugIdException] should be thrownBy BugId("ABC", 1).prev
      the[InvalidBugIdException] thrownBy {
        BugId("ABC", 1).prev
      } should have message "Underflow"
      noException should be thrownBy BugId("ABC", 999).prev
    }

    it("can be intercepted") {
      // Or better yet, intercept and test the exception
      val ex = intercept[InvalidBugIdException] {
        BugId("ABC", 1).prev
      }
      ex.msg shouldBe "Underflow"
    }
  }

  describe("Try type") {
    it("can be a failure") {
      val prevTry = BugId("ABC", 1).prevTry
      prevTry.isFailure shouldBe true
      prevTry.isSuccess shouldBe false
    }

    it("can be recoverable") {
      val prevTry = BugId("ABC", 1).prevTry.recover { case _ =>
        BugId("ABC", 999)
      }
      prevTry.isFailure shouldBe false
      prevTry.isSuccess shouldBe true
      prevTry shouldBe Success(BugId("ABC", 999))
    }

    it("can recover by throwing another exception") {
      intercept[IllegalArgumentException] {
        BugId("ABC", 1).prevTry.recover {
          throw new IllegalArgumentException("Rewrite the exception")
        }
        fail()
      } should have message "Rewrite the exception"
    }

    it("can apply some collection methods") {
      val prevSuccess = BugId("ABC", 3).prevTry
      prevSuccess shouldBe 'success
      prevSuccess shouldBe Success(BugId("ABC", 2))

      val prevFailure = BugId("ABC", 1).prevTry
      prevFailure shouldBe 'failure
      prevFailure shouldBe Failure(InvalidBugIdException("Underflow"))

      // Mapping on the value if successful
      prevSuccess.map(_.prev) shouldBe Success(BugId("ABC", 1))
      prevSuccess.map(_.next.prev) shouldBe Success(BugId("ABC", 2))
      prevSuccess.map(_.prev.prev) shouldBe Failure(
        InvalidBugIdException("Underflow")
      )

      // Ignored entirely if it's already a failure
      prevFailure.map(_.next) shouldBe prevFailure

      // When chaining methods objects, map can be diffult, but flatMap works as expected
      val prev2TryMap: Try[Try[BugId]] = prevSuccess.map(b => b.prevTry)
      prev2TryMap shouldBe Success(Success(BugId("ABC", 1)))
      val prev2Try: Try[BugId] = prevSuccess.flatMap(b => b.prevTry)
      prev2Try shouldBe Success(BugId("ABC", 1))
    }

  }
}
