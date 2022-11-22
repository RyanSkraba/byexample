package com.skraba.byexample.scala.scalatest

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Matchers and assertions on collections.
  *
  * Remember, strings, [[Option]] and [[Array]] are also collections.
  *
  * @see
  *   [[http://www.scalatest.org/user_guide/using_matchers]]
  */
class OptionsSpec extends AnyFunSpecLike with Matchers {

  describe("Options") {

    it("can be asserted as None") {
      val nothing: Option[Int] = None

      // All of these can be used to show that the option is None
      nothing shouldBe None
      nothing shouldBe 'empty
      nothing shouldBe empty
      nothing should not be defined
    }

    it("can be asserted as Some") {
      val something: Option[Int] = Some(1)

      something shouldBe Some(1)

      // Collection type matchers
      something should contain(1)
      something should contain oneOf (1, 2, 3)
      something should contain noneOf (2, 3, 4)

      // Negative assertion on None
      something shouldNot be(None)
      something should not be 'empty
      something should not be empty
      something shouldBe defined
    }

    it("can use OptionValues for more complex expressions") {
      import org.scalatest.OptionValues._

      val something: Option[Int] = Some(1)

      // Tests that something is defined then applies the test to the value
      something.value should (be > 0 and be < 10)
    }

    it("has product methods") {
      val nothing: Option[Int] = None
      val something: Option[Int] = Some(1)

      nothing.productArity shouldBe 0
      nothing.productPrefix shouldBe "None"
      nothing.productIterator.toSeq shouldBe Nil
      something.productArity shouldBe 1
      something.productElement(0) shouldBe 1
      something.productPrefix shouldBe "Some"
      something.productIterator.toSeq shouldBe Seq(1)
    }
  }
}
