package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.annotation.tailrec

/** Examples from the scala tour.
  *
  * @see
  *   [[https://docs.scala-lang.org/tour/tour-of-scala.html]]
  */
class Tour060HigherOrderNestedCurryingSpec
    extends AnyFunSpecLike
    with Matchers {

  describe("Higher order functions") {

    it("are instances and types like anything else") {
      def apply(f: Int => String, v: Int) = f(v)

      // Methods are automatically coerced to functions
      val applyFunction: (Int => String, Int) => String = apply

      // applyFunction shouldBe a[(Int => String, Int) => String]
      applyFunction shouldBe a[(_ => _, _) => _]
      apply("Hello %d".format(_), 10) shouldBe "Hello 10"
      applyFunction("Hello %d".format(_), 10) shouldBe "Hello 10"

      // Another example, uses generic type A.
      class Decorator(left: String, right: String) {
        def layout[A](x: A): String = left + x.toString + right
      }

      val decorator = new Decorator("[", "]")
      apply(decorator.layout, 7) shouldBe "[7]"
    }
  }

  describe("Nested methods") {
    it("are possible") {
      def factorial(x: Int): Int = {
        @tailrec
        def fact(x: Int, accumulator: Int): Int = {
          if (x <= 1) accumulator
          else fact(x - 1, x * accumulator)
        }

        fact(x, 1)
      }

      factorial(4) shouldBe 24
      factorial(5) shouldBe 120
    }
  }

  describe("Currying") {
    it("can be applied to methods") {
      def filter(xs: List[Int], p: Int => Boolean): List[Int] =
        if (xs.isEmpty) xs
        else if (p(xs.head)) xs.head :: filter(xs.tail, p)
        else filter(xs.tail, p)

      def modN(n: Int)(x: Int) = (x % n) == 0

      filter((1 to 8).toList, modN(2)) shouldBe List(2, 4, 6, 8)
      filter((1 to 8).toList, modN(3)) shouldBe List(3, 6)
    }

    it("can be applied to functions") {
      // Must be lazy val because it refers to itself ?
      lazy val filter: (List[Int], Int => Boolean) => List[Int] = { (xs, p) =>
        {
          if (xs.isEmpty) xs
          else if (p(xs.head)) xs.head :: filter(xs.tail, p)
          else filter(xs.tail, p)
        }
      }

      // Note the difference in notation.
      val modN: Int => Int => Boolean = n => x => (x % n) == 0

      filter((1 to 8).toList, modN(2)) shouldBe List(2, 4, 6, 8)
      filter((1 to 8).toList, modN(3)) shouldBe List(3, 6)
    }
  }
}
