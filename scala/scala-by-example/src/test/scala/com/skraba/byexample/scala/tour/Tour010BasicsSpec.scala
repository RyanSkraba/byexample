package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.runtime.BoxedUnit

/** Examples from the scala tour. Each spec covers a page.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour010BasicsSpec extends AnyFunSpecLike with Matchers {

  describe("Basic scala syntax") {

    it("has expressions") {
      (1 + 1) shouldBe 2
      ("Hello " + "World") shouldBe "Hello World"
    }

    it("has mutable and immutable variables") {
      val x = 1 + 1
      x shouldBe 2
      x shouldBe a[Integer]
      val x2: Int = 1 + 1
      x2 shouldBe 2
      // Not permitted, values are immutable.
      """x = 3""" shouldNot compile

      var x3 = 1 + 1
      x3 shouldBe 2
      x3 = 3
      x3 shouldBe 3
    }

    it("has blocks that evaluate to their last statement") {
      val x = {
        val x = 1 + 1
        x + 1
      }
      x shouldBe a[Integer]
      x shouldBe 3
    }

    it("has functions") {
      ((x: Int) => x + 1)(2) shouldBe 3
      val incr = (x: Int) => x + 1
      incr(2) shouldBe 3
      incr shouldBe a[_ => _]
      "val check: Int => Int = incr" should compile

      val add = (x: Int, y: Int) => x + y
      add(2, 1) shouldBe 3
      add shouldBe a[(_, _) => _]
      "val check: (Int, Int) => Int = add" should compile

      val three = () => 3
      three() shouldBe 3
      three shouldBe a[() => _]
      "val check: () => Int = three" should compile
    }

    it("has methods") {
      def add(x: Int, y: Int): Int = x + y

      // Unlike functions, not permitted:
      // add shouldBe a[(Integer, Integer) => Integer]
      add(1, 2) shouldBe 3

      // Several parameter lists.
      def addThenMultiply(x: Int, y: Int)(multiplier: Int): Int =
        (x + y) * multiplier

      addThenMultiply(1, 2)(3) shouldBe 9

      def three: String = "three"

      three shouldBe "three"
    }

    it("has classes") {
      class Greeter(prefix: String, suffix: String) {
        def greet(name: String): Unit =
          println(prefix + name + suffix)

        def greeting(name: String): String =
          prefix + name + suffix
      }

      val greet = new Greeter("Hello, ", "!")

      greet.greeting("world") shouldBe "Hello, world!"

      // Has a side effect of printing the greeting.
      greet.greet("World") shouldBe ()
      greet.greet("World") shouldBe a[BoxedUnit]
    }

    it("has case classes") {
      // immutable and compared by value.
      case class Point(x: Int, y: Int)

      val x = Point(1, 2)
      x shouldBe Point(1, 2)
      x.x shouldBe 1
      x.y shouldBe 2

      val y = Point(1, 2)
      val z = Point(2, 2)
      x shouldBe y
      x == y shouldBe true
      x == z shouldBe false
    }

    it("has objects") {
      object IdFactory {
        private var counter = 0

        def create(): Int = {
          counter += 1
          counter
        }
      }

      IdFactory.create() shouldBe 1
      IdFactory.create() shouldBe 2
    }

    it("has traits") {

      trait Greeter {
        def greet(name: String): String
      }

      trait Greeter2 {
        def greet(name: String): String = "Hello, " + name + "!"
      }

      class DefaultGreeter extends Greeter2

      class CustomizableGreeter(prefix: String, postfix: String) extends Greeter {
        override def greet(name: String): String = {
          prefix + name + postfix
        }
      }

      new DefaultGreeter().greet(
        "Scala developer"
      ) shouldBe "Hello, Scala developer!"

      new CustomizableGreeter("How are you, ", "?").greet(
        "World"
      ) shouldBe "How are you, World?"
    }
  }
}
