package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

/** Examples from the scala tour.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour030ClassesSpec extends AnyFunSpecLike with Matchers {

  describe("Classes") {

    it("can be very simple") {
      class Thing
      val x = new Thing

      // Doesn't have equals() semantics
      x shouldBe a[Thing]
      x should not be new Thing
    }

    it("can have a constructor") {
      // Constructors are val and private by default.
      // If you add val or var, they are public with accessor(s).
      // If you add private, there are no accessors.

      // var x is public and modifiable
      // private var y is private and modifiable
      // z is private and unmodifiable
      class Point(var x: Int, private var y: Int, z: Int) {

        def move(dx: Int, dy: Int): Unit = {
          x = x + dx
          y = y + dy
        }

        override def toString: String = s"($x, $y, $z)"
      }

      val point1 = new Point(2, 3, 0)
      point1.toString shouldBe "(2, 3, 0)"

      // Because they are var, the fields can be used.
      point1.x shouldBe 2
      point1.x = 3
      point1.x shouldBe 3
      point1.toString shouldBe "(3, 3, 0)"

      // Because y is private we can't do the same.
      "print(point1.y)" shouldNot compile

      point1.move(1, 1)
      point1.toString shouldBe "(4, 4, 0)"
    }

    it("can have a constructor with optional parameters") {
      class Point(var x: Int = 0, var y: Int = 0)

      val origin = new Point // x and y are both set to 0
      origin.x shouldBe 0
      origin.y shouldBe 0
      val point1 = new Point(1)
      point1.x shouldBe 1
      point1.y shouldBe 0

      val point2 = new Point(y = 2)
      point2.x shouldBe 0
      point2.y shouldBe 2
    }

    it("can have private members / getters and setters") {
      class Point {
        // Access is assumed to be public in general.

        private var _x = 0
        private var _y = 0
        private val bound = 100

        def x: Int = _x

        def x_=(newValue: Int): Unit = {
          if (newValue < bound) _x = newValue else invalidate()
        }

        def y: Int = _y

        def y_=(newValue: Int): Unit = {
          if (newValue < bound) _y = newValue else invalidate()
        }

        private def invalidate(): Unit = {
          _x = -1
          _y = -1
        }
      }

      val point1 = new Point
      point1.x shouldBe a[Integer]
      point1.x shouldBe 0

      point1.x = 99
      point1.y = 99
      point1.x shouldBe 99
      point1.y shouldBe 99

      point1.x = 100
      point1.x shouldBe -1
      point1.y shouldBe -1
    }

    it("can have private constructor args") {
      // x, y and z are var, val and unmodified
      class Point(var x: Int, val y: Int, z: Int)
      val point = new Point(1, 2, 3)
      // var x is public and modifiable
      point.x shouldBe 1
      point.x = 99
      point.x shouldBe 99
      // val y is public and immutable
      point.y shouldBe 2
      "point.y = 99" shouldNot compile
      // z is private
      "print(point.z)" shouldNot compile
      "point.z = 99" shouldNot compile
    }
  }
}
