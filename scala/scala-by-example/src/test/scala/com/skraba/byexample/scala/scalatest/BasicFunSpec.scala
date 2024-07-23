package com.skraba.byexample.scala.scalatest

import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** Basic FunSpec style specification to organise unit tests.
  *
  * @see
  *   [[http://www.scalatest.org/getting_started_with_fun_spec]]
  */
class BasicFunSpec extends AnyFunSpecLike with Matchers {

  describe("FunSpec scenarios") {
    describe("when nesting in a describe") {
      val reused = true
      it("permits you to organise your unit tests") {
        assert(reused)
        assert(reused, "A custom error messages")
        assertResult(3)(1 + 2)
        assertThrows[StringIndexOutOfBoundsException]("hi" (10))
      }
      it("organises unit tests carefully") {
        withClue("using a clue") {
          reused should not be false
        }
      }
      for (x <- 1 to 10) {
        it(s"allows creating a scope for tests $x") {
          reused shouldBe true
        }
      }
    }

    ignore("can be ignored by changing the keyword") {
      it("so this failing test isn't used") {
        fail("I never get here")
      }
      it("or this one") {
        fail("I never get here")
      }
    }

    it("can be configured to be ignored with a tag", Slow) {
      // This is ignored by default in the maven build.

      // IntelliJ can be configured to ignore these tests by putting
      // -l org.scalatest.tags.Slow
      // in the Program arguments of the Run/Debug configuration
    }
  }

  describe("Controlling scenarios") {
    val condition = false

    it("can have pending tests") {
      pending
      fail("This test was not actually run: Test Pending")
    }

    it("can have pendingUntilFixed tests") {
      pendingUntilFixed {
        // This test is marked as PENDING if and only if it actually fails
        fail("This isn't run")
      }
    }

    it("can stop") {
      assume(condition)
      fail("This test was not actually run: Test Canceled: condition was false")
    }

    it("can have cancelled tests") {
      if (!condition) cancel("I don't really fail")
      fail("This test was not actually run: Test Canceled: I don't really fail")
    }

    describe("inside a pending describe block") {
      if (!condition) pending
      it("should fail") { fail("This isn't run but isn't seen in the console") }
      it("should also fail") { fail("This isn't run but isn't seen in the console") }
    }

    /** This is a technique for disabling all of the unit tests in this spec by rewriting the `it` word that is used to
      * run the tests.
      */
    class MaybeItWord(enabled: Boolean) extends ItWord {
      override def apply(specText: String, testTags: Tag*)(testFun: => Any)(implicit pos: Position): Unit = {
        // Since we override `it`, we fall back on the equivalent `they` if the tests are enabled.
        if (enabled) they(specText)(testFun)
        else ignore(specText)()
      }
    }

    describe("by overwriting the `it` method") {
      val it = new MaybeItWord(false)
      it("so this failing test isn't used") { fail("I never get here") }
    }

    describe("by writing different `it` equivalents") {
      val yesIt = new MaybeItWord(true)
      val noIt = new MaybeItWord(false)
      it("so this test is run") { succeed }
      yesIt("so this test is also run") { succeed }
      noIt("so this failing test isn't used") { fail("I never get here") }
    }
  }
}
