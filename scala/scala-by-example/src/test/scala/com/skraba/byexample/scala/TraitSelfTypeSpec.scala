package com.skraba.byexample.scala

import org.scalatest.BeforeAndAfterEach
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Self types and traits.
  *
  * Instead of extending a base, you can do this:
  *
  * {{{
  * trait MixinSelfType {
  *   // self is like a private[this] scoped member.
  *   self: Base =>
  *
  *   def mixinValue: String = "m" + self.value
  * }
  * }}}
  *
  * @see
  *   [[https://softwareengineering.stackexchange.com/questions/219038/what-is-the-difference-between-self-types-and-trait-inheritance-in-scala]]
  * @see
  *   [[https://alvinalexander.com/scala/how-to-define-scala-trait-subclassed-certain-self-types]]
  */
class TraitSelfTypeSpec extends AnyFunSpecLike with Matchers with BeforeAndAfterEach {

  /** A base trait */
  trait Base {
    def value: String = "--"
  }

  /** A subtrait using normal extension. */
  trait SubX extends Base {
    override def value = "X"
  }

  /** Another subtrait using normal extension. */
  trait SubY extends Base {
    override def value = "Y"
  }

  /** A self-type trait to be mixed in. */
  trait MixinSelfType {
    // self is like a private[this] scoped member.
    self: Base =>

    def mixinValue: String = "m" + self.value

  }

  /** A trait using extension to be mixed in. */
  trait MixinExtends extends Base {
    def mixinValue: String = "m" + value
  }

  describe("Traits that mix in with extend") {

    /** Actually is a Base, but via the extends not explicitly. */
    object ImNotABase extends MixinExtends

    object ImABase extends MixinExtends with Base

    object ImASubX extends MixinExtends with SubX

    object ImASubY extends MixinExtends with SubY

    object ImASubXY extends MixinExtends with SubX with SubY

    object ImASubYX extends MixinExtends with SubY with SubX

    it("return the mixed in behaviour") {
      ImNotABase.mixinValue shouldBe "m--"
      ImABase.mixinValue shouldBe "m--"
      ImASubX.mixinValue shouldBe "mX"
      ImASubY.mixinValue shouldBe "mY"
      ImASubXY.mixinValue shouldBe "mY"
      ImASubYX.mixinValue shouldBe "mX"
    }

    it("expose the behaviour of the extended trait") {
      val mixinB: MixinExtends = ImASubXY
      mixinB.mixinValue shouldBe "mY"
      mixinB.value shouldBe "Y" // See below
    }
  }

  describe("Traits that mix in with self-types") {
    // FAIL: Does not conform to Base.
    // object ImNotABase extends MixinSelfType

    object ImABase extends MixinSelfType with Base

    object ImASubX extends MixinSelfType with SubX

    object ImASubY extends MixinSelfType with SubY

    object ImASubXY extends MixinSelfType with SubX with SubY

    object ImASubYX extends MixinSelfType with SubY with SubX

    it("return the mixed in behaviour") {
      // Looks exactly the same as above.
      ImABase.mixinValue shouldBe "m--"
      ImASubX.mixinValue shouldBe "mX"
      ImASubY.mixinValue shouldBe "mY"
      ImASubXY.mixinValue shouldBe "mY"
      ImASubYX.mixinValue shouldBe "mX"
    }

    it("hides the behaviour of the extended trait") {
      // Cast so we can't see which Base it is.
      val mixinY: MixinSelfType = ImASubXY
      // This is different: we aren't allowed to access the info from the Base, just the Mixin!
      mixinY.mixinValue shouldBe "mY"
      "mixinY.mixinValue" should compile
      "mixinY.value" shouldNot compile
    }
  }
}
