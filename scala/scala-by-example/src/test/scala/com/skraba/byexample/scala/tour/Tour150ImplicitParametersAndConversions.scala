package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour150ImplicitParametersAndConversions
    extends AnyFunSpecLike
    with Matchers {

  describe("Implicit/contextual parameters") {
    describe("are automatically injected into method calls") {

      // Note that his has changed significantly in Scala 3 for clarity and readability.
      trait Comparator[A] {
        def compare(x: A, y: A): Int
      }

      object Comparator {

        /** The implicit comparator can be an object */
        implicit object IntComparator extends Comparator[Int] {
          def compare(x: Int, y: Int): Int = Integer.compare(x, y)
        }

        /** The implicit comparator can be a method */
        implicit def stringComparator: Comparator[String] =
          (x: String, y: String) => x.compareTo(y)
      }

      def max[A](x: A, y: A)(implicit comparator: Comparator[A]): A =
        if (comparator.compare(x, y) >= 0) x
        else y

      it("from companion objects") {
        max(10, 6) shouldBe 10
        max("hello", "world") shouldBe "world"
        "max(true, false)" shouldNot compile
      }

      it("from implicit objects in scope") {
        implicit object BooleanComparator extends Comparator[Boolean] {
          override def compare(x: Boolean, y: Boolean): Int = x.compareTo(y)
        }
        implicit def ReverseIntComparator: Comparator[Int] =
          (x: Int, y: Int) => y.compareTo(x)
        max(10, 6) shouldBe 6
        max("hello", "world") shouldBe "world" // still from companion
        max(true, false) shouldBe true
      }

    }
  }

  describe("Implicit/contextual conversions") {

    // Using this feature is really discouraged. Warnings are shown unless this
    // import is explicitly added.
    import scala.language.implicitConversions

    case class Greeting(greet: String)

    case class Repeater(statement: String, number: Int) {
      def exclaim(): String = statement * number
    }

    object Augmenter {
      implicit def turnGreetingToRepeater(in: Greeting): Repeater =
        Repeater(in.greet, 1)
      implicit def turnRepeaterToGreeting(in: Repeater): Greeting =
        Greeting(in.exclaim())
    }

    import Augmenter._

    it("can turn an A into a B") {
      val g1 = Greeting("Hello world\n")
      val r1: Repeater = g1
      r1 shouldBe Repeater("Hello world\n", 1)

      val g2 = r1.copy(number = 2)
      g2.greet shouldBe "Hello world\nHello world\n"
    }

    it("can add B methods to A") {
      Greeting("Hello world").exclaim() shouldBe "Hello world"
    }
  }
}
