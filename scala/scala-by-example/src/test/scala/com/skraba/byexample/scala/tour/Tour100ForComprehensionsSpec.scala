package com.skraba.byexample.scala.tour

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ArrayBuffer

/** Examples from the scala tour.
  *
  * @see https://docs.scala-lang.org/tour/tour-of-scala.html
  */
class Tour100ForComprehensionsSpec extends AnyFunSpecLike with Matchers {

  describe("For comprehensions") {
    it("is simple") {

      // Create a sequence for testing.
      case class User(name: String, age: Int)
      val userBase = List(
        User("Travis", 28),
        User("Kelly", 33),
        User("Jennifer", 44),
        User("Dennis", 23)
      )

      // twentySomethings will be a List[User] since the first comprehension is a list.
      val twentySomethings =
        for (user <- userBase if user.age >= 20 && user.age < 30)
          yield user.name // i.e. add this to a list

      twentySomethings shouldBe List("Travis", "Dennis")

      // Desugared to withFilter and map.
      val twentySomethings2 = userBase
        .withFilter(user => user.age >= 20 && user.age < 30)
        .map(user => user.name) // i.e. add this to a list

      // Example with more than one enumeration.  The first loop(s) will be flatMap.
      def foo(n: Int, v: Int) =
        for (
          i <- 0 until n;
          j <- i until n if i + j == v
        )
          yield (i, j)

      foo(10, 10) shouldEqual List((1, 9), (2, 8), (3, 7), (4, 6), (5, 5))

      // You can omit yield if you don't want to return a List, but use side effects.
      val sideEffectCollector: ArrayBuffer[(Int, Int)] =
        ArrayBuffer.empty[(Int, Int)]

      def foox(n: Int, v: Int): Unit =
        for (
          i <- 0 until n;
          j <- i until n if i + j == v
        )
          sideEffectCollector += ((i, j))

      foox(10, 10)
      sideEffectCollector shouldEqual List(
        (1, 9),
        (2, 8),
        (3, 7),
        (4, 6),
        (5, 5)
      )
    }

    // For comprehensions can be applied to any object with map, flatMap and filter methods since
    // it is translated into these operations underneath.
    it("is translated into map, flatMap, filters.") {
      val xs = List(1, 2, 3)

      // Functions appropriate for a map, flatMap and filter arguments.
      def f(i: Int): Int = i * 2

      def g(i: Int): Iterable[Int] = List.tabulate(i)(_ => i)

      def p(i: Int): Boolean = i % 2 == 1

      // The arrow represents a map.
      (for (x <- xs) yield f(x)) shouldEqual List(2, 4, 6)
      (for (x <- xs) yield f(x)) shouldEqual xs.map(f)

      // The semicolon between predicates is a flatMap.
      (for (x <- xs; y <- g(x)) yield y) shouldEqual List(1, 2, 2, 3, 3, 3)
      (for (x <- xs; y <- g(x)) yield y) shouldEqual xs.flatMap(g)

      // The if predicate is a filter.
      (for (x <- xs if p(x)) yield x) shouldEqual List(1, 3)
      (for (x <- xs if p(x)) yield x) shouldEqual xs.filter(p)

      // In the other direction, the map/flatMap/filter implemented using the for comprehension.
      def mapUsingFor[IN, OUT](xs: List[IN], f: IN => OUT): List[OUT] =
        for (x <- xs) yield f(x)

      def flatMapUsingFor[IN, OUT](
          xs: List[IN],
          f: IN => Iterable[OUT]
      ): List[OUT] =
        for (x <- xs; y <- f(x)) yield y

      def filterUsingFor[IN](xs: List[IN], p: IN => Boolean): List[IN] =
        for (x <- xs if p(x)) yield x

      mapUsingFor(xs, f) shouldEqual List(2, 4, 6)
      flatMapUsingFor(xs, g) shouldEqual List(1, 2, 2, 3, 3, 3)
      filterUsingFor(xs, p) shouldEqual List(1, 3)
    }
  }
}
