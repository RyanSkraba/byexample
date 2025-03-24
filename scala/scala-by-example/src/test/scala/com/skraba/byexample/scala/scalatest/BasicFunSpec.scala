package com.skraba.byexample.scala.scalatest

import org.scalactic.source.Position
import org.scalatest.Tag
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import org.scalatest.tagobjects.Slow

/** Basic FunSpec style specification to organise unit tests.
  *
  * @see
  *   [[https://www.scalatest.org/getting_started_with_fun_spec]]
  */
class BasicFunSpec extends AnyFunSpecLike with Matchers {

  describe("FunSpec scenarios") {
    describe("when nesting in a describe") {
      val reused = true
      it("permits you to organise your unit tests") {
        assert(reused)
        assert(reused, "A custom error message")
        assertResult(3)(1 + 2)
        assertThrows[StringIndexOutOfBoundsException]("hi" (10))
      }
      it("organises unit tests carefully") {
        withClue("using a clue") { reused should not be false }
      }
      for (x <- 1 to 10) {
        it(s"allows creating a scope for tests (#$x)") {
          reused shouldBe true
        }
      }
    }

    they("have some synonyms") {
      succeed
    }

    ignore("can be ignored by changing the keyword") {
      it("so this failing test isn't used") { fail("I never get here") }
      it("or this one") { fail("I never get here") }
    }
  }

  describe("Using tags to control execution") {
    it("can be configured to be ignored with a tag", Slow) {
      // This is ignored by default in the maven build, but can be configured to run:
      //   mvn package -Dplugin.maven.scalatest.exclude=

      // IntelliJ runs these tests by default, but can be configured to ignore them by adding Program arguments to
      // the Run/Debug configuration:
      //   -l org.scalatest.tags.Slow
    }
  }

  describe("In order to control which tests are run") {
    val condition = false

    ignore("ignore can replace it/they/describe") {
      it("so this failing test isn't used") {
        fail("I never get here")
      }
      it("or this one") {
        fail("I never get here")
      }
    }

    describe("pending tests are similar to ignored") {
      it("indicates that a test is not yet implemented") {
        pending
        fail("This test was not actually run: Test Pending")
      }

      it("pendingUntilFixed blocks are considered pending if broken, and failed if they pass") {
        pendingUntilFixed {
          // If this test were to succeed, the TestFailedException would indicate that the pendingUntilFixed is
          // obsolete.  It must faile to be ignored.
          fail("This isn't run")
        }
      }

      describe("when applied in a describe block applies to all the tests") {
        if (!condition) pending
        it("should fail") { fail("This isn't run but isn't seen in the console") }
        it("should also fail") { fail("This isn't run but isn't seen in the console") }
      }

    }

    describe("tests can be canceled (but not failed)") {
      it("using an assume condition") {
        assume(condition)
        fail("This test was not actually run: Test Canceled: condition was false")
      }

      it("or explicitly using cancel") {
        if (!condition) cancel("I don't really fail")
        fail("This test was not actually run: Test Canceled: I don't really fail")
      }
    }
  }

  /** This is a technique for disabling many unit tests in a spec by rewriting the `it` word used to run the tests. */
  class MaybeItWord(enabled: Boolean) extends ItWord {
    override def apply(specText: String, testTags: Tag*)(testFun: => Any)(implicit pos: Position): Unit = {
      // If we override `it`, we can fall back on the equivalent `they` for the tests that are enabled.
      if (enabled) they(specText, testTags: _*)(testFun)
      else ignore(specText)((): Unit)
    }
  }

  /** If these are top-level in the class, they get the "right" highlighting in IntelliJ. */
  val yesIt = new MaybeItWord(true)
  val noIt = new MaybeItWord(false)

  describe("Using a custom it to selectively ignore or run tests.") {
    describe("by overwriting the `it` method") {
      val it = new MaybeItWord(false)
      it("so this failing test isn't used") { fail("I never get here") }
      it("and neither is this one") { fail("I never get here") }
      they("but this one is run") { succeed }
    }

    describe("by writing different `it` equivalents") {
      it("so this test is run") { succeed }
      yesIt("so this test is also run") { succeed }
      noIt("so this failing test isn't used") { fail("I never get here") }
    }
  }
}
