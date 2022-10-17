package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour140CompoundTypesSelfTypesSpec extends AnyFunSpecLike with Matchers {

  describe("Compound types") {
    it("are simple") {
      // Two traits.
      trait Cloneable extends java.lang.Cloneable {
        override def clone(): Cloneable = {
          super.clone().asInstanceOf[Cloneable]
        }
      }
      trait Resetable {
        def reset(): Unit
      }
      // A method that requires both traits.
      def cloneAndReset(obj: Cloneable with Resetable): Cloneable = {
        val cloned = obj.clone()
        obj.reset()
        cloned
      }
    }
  }

  /** @see
    *   [[com.skraba.byexample.scala.TraitSelfTypeSpec]] for more detail.
    */
  describe("Self types") {
    it("is simple") {
      trait User {
        def username: String
      }

      // This is the self type.
      trait Tweeter {
        this: User => // reassign this
        def tweet(tweetText: String): String = s"$username: $tweetText"
      }

      class VerifiedTweeter(val username_ : String) extends Tweeter with User {
        // We mixin User because Tweeter required it
        def username = s"real $username_"
      }

      val realBeyoncé = new VerifiedTweeter("Beyoncé")
      realBeyoncé.tweet(
        "Just spilled my glass of lemonade"
      ) shouldBe "real Beyoncé: Just spilled my glass of lemonade"
    }
  }
}
