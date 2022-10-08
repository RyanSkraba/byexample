package com.skraba.byexample.scala.scalatest

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
    it("can stop") {
      assume(condition)
      fail("This test was not actually run.")
    }
    it("can have cancelled tests") {
      if (!condition) cancel("I don't really fail")
    }
  }
}
