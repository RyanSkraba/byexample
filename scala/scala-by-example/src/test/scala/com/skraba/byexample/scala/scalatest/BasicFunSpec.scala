package com.skraba.byexample.scala.scalatest

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Basic FunSpec style specification to organise unit tests.
  *
  * @see http://www.scalatest.org/getting_started_with_fun_spec
  */
class BasicFunSpec extends AnyFunSpecLike with Matchers {

  describe("FunSpec scenarios") {
    describe("when nesting in a describe") {
      val reused = true
      it("permits you to organise your unit tests") {
        assert(reused)
      }
      it("organises unit tests carefully") {
        reused should not be false
      }
      for (x <- 1 to 10) {
        it(s"allows creating a scope for tests $x") {
          reused shouldBe true
        }
      }
    }

    ignore("can be ignored by changing the keyword") {
      val reused = false
      it("so this failing test isn't used") {
        assert(reused)
      }
      it("or this one") {
        reused should not be false
      }
    }
  }
}
