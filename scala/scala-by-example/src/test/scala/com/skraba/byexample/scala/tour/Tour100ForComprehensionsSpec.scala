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

    it("are powerful and simple loops") {

      // Create a sequence for testing.
      case class Issue(project: String, number: Int)
      val issues = List(
        Issue("BYEX", 1234),
        Issue("BEAM", 1111),
        Issue("AVRO", 2222),
        Issue("BYEX", 2345)
      )

      // twentySomethings will be a List[User] since the first comprehension is a list.
      val byexIssues =
        for (i <- issues if i.project == "BYEX")
          yield i.number // i.e. add this to a list

      byexIssues shouldBe List(1234, 2345)

      // Desugared to withFilter and map.
      val byexIssues2 = issues
        .withFilter(i => i.project == "BYEX")
        .map(i => i.number) // i.e. add this to a list
      byexIssues shouldBe List(1234, 2345)

      // Example with more than one enumeration.  The first loop(s) will be flatMap.
      def foo(n: Int, v: Int) =
        for (
          i <- 0 until n;
          j <- i until n if i + j == v
        )
          yield (i, j)

      foo(10, 10) shouldBe List((1, 9), (2, 8), (3, 7), (4, 6), (5, 5))

      // The translation to map, flatMap and filter:
      foo(10, 10) shouldBe (0 until 10)
        .flatMap { i =>
          (i until 10).map(j => (i, j))
        }
        .filter(ij => ij._1 + ij._2 == 10)

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
      sideEffectCollector shouldBe List(
        (1, 9),
        (2, 8),
        (3, 7),
        (4, 6),
        (5, 5)
      )
    }

    // For comprehensions can be applied to any object with map, flatMap and filter methods since
    // it is translated into these operations underneath.
    it("are translated into map, flatMap, filters.") {
      val xs = List(1, 2, 3)

      // Functions appropriate for a map, flatMap and filter arguments.
      def f(i: Int): Int = i * 2

      def g(i: Int): Iterable[Int] = List.tabulate(i)(_ => i)

      def p(i: Int): Boolean = i % 2 == 1

      // The arrow represents a map.
      (for (x <- xs) yield f(x)) shouldBe List(2, 4, 6)
      (for (x <- xs) yield f(x)) shouldBe xs.map(f)

      // The semicolon between predicates is a flatMap.
      (for (x <- xs; y <- g(x)) yield y) shouldBe List(1, 2, 2, 3, 3, 3)
      (for (x <- xs; y <- g(x)) yield y) shouldBe xs.flatMap(g)

      // The if predicate is a filter.
      (for (x <- xs if p(x)) yield x) shouldBe List(1, 3)
      (for (x <- xs if p(x)) yield x) shouldBe xs.filter(p)

      // In the other direction, map, flatMap and filter can be implemented using
      // the for comprehensions
      def mapUsingFor[IN, OUT](xs: List[IN], f: IN => OUT): List[OUT] =
        for (x <- xs) yield f(x)

      def flatMapUsingFor[IN, OUT](
          xs: List[IN],
          f: IN => Iterable[OUT]
      ): List[OUT] =
        for (x <- xs; y <- f(x)) yield y

      def filterUsingFor[IN](xs: List[IN], p: IN => Boolean): List[IN] =
        for (x <- xs if p(x)) yield x

      mapUsingFor(xs, f) shouldBe List(2, 4, 6)
      flatMapUsingFor(xs, g) shouldBe List(1, 2, 2, 3, 3, 3)
      filterUsingFor(xs, p) shouldBe List(1, 3)
    }
  }
}
