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
    else throw BadBugIdException("Overflow")

    def prev: BugId = if (num > 1) copy(num = num - 1)
    else throw BadBugIdException("Underflow")

    /** Equivalent to [[prev]] but wrapped in a Try */
    def prevTry: Try[BugId] = Try(prev)
  }

  case class BadBugIdException(msg: String) extends Exception(msg)

  describe("Catching an exception in a test") {

    it("can be caught and wrapped") {
      // A classical way would be to write code wrapping the action under test with a try/catch block, and testing that the expected exception occurs
      val value =
        try {
          BugId("XYZ", 1).prev.num
        } catch {
          case BadBugIdException(msg) =>
            msg shouldBe "Underflow"
            Int.MinValue
          case _: Throwable => fail()
        }
      value shouldBe Int.MinValue
    }

    it("can be expected") {
      // This doesn't see the exception but ensures that it was thrown
      assertThrows[BadBugIdException] {
        BugId("ABC", 1).prev
      }

      an[BadBugIdException] should be thrownBy BugId("ABC", 1).prev
      the[BadBugIdException] thrownBy {
        BugId("ABC", 1).prev
      } should have message "Underflow"
      noException should be thrownBy BugId("ABC", 999).prev
    }

    it("can be intercepted") {
      // Or better yet, intercept and test the exception
      val ex = intercept[BadBugIdException] {
        BugId("ABC", 1).prev
      }
      ex.msg shouldBe "Underflow"
    }
  }

  describe("Try type") {

    val good = BugId("ABC", 999).prevTry
    val bad = BugId("ABC", 1).prevTry

    it("can be a success") {
      good.isFailure shouldBe false
      good.isSuccess shouldBe true
      good shouldBe 'success
      good shouldBe Success(BugId("ABC", 998))

      // Getting the value on a success just returns it
      good.get shouldBe BugId("ABC", 998)
      good.getOrElse(BugId("DEF", 1)) shouldBe BugId("ABC", 998)
      good.orElse(Try(BugId("DEF", 1))) shouldBe good
    }

    it("can be a failure") {
      bad.isFailure shouldBe true
      bad.isSuccess shouldBe false
      bad shouldBe 'failure
      bad shouldBe Failure(BadBugIdException("Underflow"))

      // Getting the value should cause the exception to be thrown
      intercept[BadBugIdException] {
        bad.get
      } should have message "Underflow"
      bad.getOrElse(BugId("DEF", 1)) shouldBe BugId("DEF", 1)
      bad.orElse(Try(BugId("DEF", 1))) shouldBe Success(BugId("DEF", 1))
    }

    it("can be recoverable") {
      // recover returns a value that will be a success
      val recovered = bad.recover { case BadBugIdException(msg) =>
        BugId(msg, 999)
      }
      recovered.isFailure shouldBe false
      recovered.isSuccess shouldBe true
      recovered shouldBe Success(BugId("Underflow", 999))

      // recoverWith returns a Try that will replace the failure
      val recoverWith = bad.recoverWith { case BadBugIdException(msg) =>
        Try(BugId(msg, 999))
      }
      recoverWith.isFailure shouldBe false
      recoverWith.isSuccess shouldBe true
      recoverWith shouldBe Success(BugId("Underflow", 999))
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
      // Mapping on the value if successful
      good.map(_.prev) shouldBe Success(BugId("ABC", 997))
      good.map(_.next.prev) shouldBe Success(BugId("ABC", 998))
      good.map(_.copy(num = 1).prev) shouldBe Failure(
        BadBugIdException("Underflow")
      )

      // Ignored entirely if it's already a failure
      bad.map(_.next) shouldBe bad

      // When chaining methods objects, map can be difficult, but flatMap works as expected
      val goodNestMap: Try[Try[BugId]] = good.map(b => b.prevTry)
      goodNestMap shouldBe Success(Success(BugId("ABC", 997)))
      val goodNest: Try[BugId] = good.flatMap(b => b.prevTry)
      goodNest shouldBe Success(BugId("ABC", 997))

      goodNestMap.flatten shouldBe goodNest

      // Filter only works on success
      bad.filter(_.num % 2 == 0) shouldBe bad
      good.filter(_.num % 2 == 0) shouldBe good
      good.filter(_.num % 2 == 1) shouldBe 'failure
    }

    it("can invert the success/failure") {
      // Calling .failed causes a failure to be a Success (containing it's own exception)
      bad.failed shouldBe Success(BadBugIdException("Underflow"))
      // And the good to be a failure containing an UnsupportedOperationException
      good.failed shouldBe 'failure
      intercept[UnsupportedOperationException] {
        good.failed.get
      } should have message "Success.failed"
    }

    it("can use fold to turn a success or failure into a known type") {
      // First method is applied to a failure, second to a success.
      // They both have a String return value.
      good.fold(_.getMessage, _.toString) shouldBe "BugId(ABC,998)"
      bad.fold(_.getMessage, _.toString) shouldBe "Underflow"
    }
  }
}
