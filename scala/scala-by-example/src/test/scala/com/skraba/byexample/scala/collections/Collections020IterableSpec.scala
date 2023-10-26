package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.View

/** Examples from the scala collections doc. Each spec covers a page.
  *
  * Iterable adds a hasNext/next methods and some functions that require this to
  * have high performance.
  *
  * It has three major subclasses:
  *
  * Seq (PartialFunction, apply returns the element at that index, isDefinedAt)
  * Map (PartialFunction, apply returns the value for that key, isDefinedAt) Set
  * (apply returns whether or not the element exists.)
  *
  * @see
  *   [[https://docs.scala-lang.org/overviews/collections/introduction.html]]
  */
class Collections020IterableSpec extends AnyFunSpecLike with Matchers {

  describe("Iterable") {

    // Instance for testing.
    val xs = Iterable(1, 2, 3)

    it("has a default implementation of immutable.List") {
      xs shouldBe a[List[_]]

      // It defines foreach in Traversable via iterator.
      val it = xs.iterator
      it shouldBe an[Iterator[_]]
      // An Iterator is a collection too -- with hasNext() and next()
      while (it.hasNext) it.next should (be > 0 and be <= 3)
    }

    it("supports grouped sub-iterators") {
      // Returns iterators taking the original collection in chunks.
      val it = xs.grouped(2)
      it.hasNext shouldBe true
      it.next shouldBe Iterable(1, 2)
      it.hasNext shouldBe true
      it.next shouldBe Iterable(3)
      it.hasNext shouldBe false
    }

    it("supports sliding sub-iterators") {
      // Returns iterators taking the original collection in chunks.
      val it = xs.sliding(2, 1)
      it.hasNext shouldBe true
      it.next shouldBe Iterable(1, 2)
      it.hasNext shouldBe true
      it.next shouldBe Iterable(2, 3)
      it.hasNext shouldBe false
    }

    it("supports additional subcollection") {
      // In addition to take/drop
      xs takeRight 2 shouldBe Iterable(2, 3)
      xs dropRight 2 shouldBe Iterable(1)
    }

    it("supports iterating together") {
      val ys1 = Iterable("a", "b")
      val ys2 = Iterable("a", "b", "c", "d")

      // Using the smallest iterator.
      xs zip ys1 shouldBe Iterable((1, "a"), (2, "b"))
      xs zip ys2 shouldBe Iterable((1, "a"), (2, "b"), (3, "c"))

      // lazyZip is the same, without evaluating (see LazyList)
      (xs lazyZip ys1).toSeq should contain allOf ((1, "a"), (2, "b"))

      // Using the longest and providing defaults.
      xs.zipAll(ys1, 99, "z") shouldBe Iterable((1, "a"), (2, "b"), (3, "z"))
      xs.zipAll(ys2, 99, "z") shouldBe Iterable(
        (1, "a"),
        (2, "b"),
        (3, "c"),
        (99, "d")
      )

      // Iterate with it's own index (the second value)
      xs.zipWithIndex shouldBe Iterable((1, 0), (2, 1), (3, 2))

    }

    it("supports comparison") {
      val ys1 = Iterable("a", "b")
      xs.iterator sameElements ys1.iterator shouldBe false
    }
  }
}
