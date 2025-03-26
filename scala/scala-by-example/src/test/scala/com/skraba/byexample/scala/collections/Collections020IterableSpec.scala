package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable

/** Examples from the scala collections doc. Each spec covers a page.
  *
  * All methods in the [[Iterable]] trait are implemented via `iterator`
  *
  * It has three major subclasses:
  *
  *   - Seq (PartialFunction, apply returns the element at that index, isDefinedAt)
  *   - Map (PartialFunction, apply returns the value for that key, isDefinedAt)
  *   - Set (apply returns whether or not the element exists.)
  *
  * @see
  *   [[https://docs.scala-lang.org/overviews/collections-2.13/introduction.html]]
  */
class Collections020IterableSpec extends AnyFunSpecLike with Matchers {

  // Instance for testing.
  val xs: Iterable[Int] = Iterable(1, 2, 3)

  describe("Iterator") {
    it("has high-performance next() and hasNext() methods") {
      val it: Iterator[Int] = xs.iterator
      it.hasNext shouldBe true
      it.next() shouldBe 1
      it.hasNext shouldBe true
      it.nextOption() shouldBe Some(2)
      it.hasNext shouldBe true
      it.next() shouldBe 3

      // nextOption is OK, but don't call next if hasNext is false.
      it.hasNext shouldBe false
      it.nextOption() shouldBe None
      val t = intercept[NoSuchElementException] { it.next() }
      t.getMessage shouldBe "head of empty list"
    }

    it("has aliases") {
      // The only alias for Iterator is concat
      val it: Iterator[Int] = xs.iterator ++ xs.iterator
      it.length shouldBe 6
    }

    it("has other methods that can be found in Iterable") {
      // The contains method works on Iterator, but should only be used *once*
      // on the instance, then discarded
      xs.iterator.contains(-1) shouldBe false

      // The return value is undefined after a contains() call!
      val it = xs.iterator
      it.contains(3) shouldBe true
      it.contains(1) shouldBe false

      // Many other methods that can efficiently be implemented through an
      // iterator, such as scanLeft, exist with the same semantics as Iterable
    }
  }

  describe("Iterable") {
    it("has a default implementation of immutable.List") {
      xs shouldBe a[List[_]]

      // All methods in this trait are implemented via .iterator
      val it: Iterator[Int] = xs.iterator
      // An Iterator is a collection too -- with hasNext() and next()
      while (it.hasNext) it.next() should (be > 0 and be <= 3)

      // For example
      xs.foreach(_ should (be > 0 and be <= 3))
    }

    it("has aliases") {
      // The only alias for Iterable is concat
      (xs ++ xs).size shouldBe 6
    }

    it("has factories") {
      // Nothing
      Iterable.empty shouldBe Iterable()

      // From something
      Iterable(1, 2, 3) shouldBe Iterable(1, 2, 3)

      // From repeatedly applying a funtion
      Iterable.iterate(1, 4)(_ * 2) shouldBe Iterable(1, 2, 4, 8)

      // Unfold takes a state (accumulator) and applies a function to it that
      // returns the next value of the Iterable -> newState, or the end.
      Iterable.unfold('d') { c => if (c <= 'f') Some(c - 'a', (c + 1).toChar) else None } shouldBe Iterable(3, 4, 5)

      // Ranges fill the iterable
      Iterable.range(0, 3) shouldBe Iterable(0, 1, 2)
      Iterable.range(0, 10, 3) shouldBe Iterable(0, 3, 6, 9)

      // Iterable dimensional
      Iterable.fill(5)("A") shouldBe Iterable("A", "A", "A", "A", "A")
      Iterable.tabulate(5)(x => s"A$x") shouldBe Iterable("A0", "A1", "A2", "A3", "A4")

      // Up to five dimensions are possible.
      Iterable.fill(5, 2)("A") shouldBe Iterable(
        Iterable("A", "A"),
        Iterable("A", "A"),
        Iterable("A", "A"),
        Iterable("A", "A"),
        Iterable("A", "A")
      )
      Iterable.tabulate(5, 2)((x, y) => s"A$x$y") shouldBe Iterable(
        Iterable("A00", "A01"),
        Iterable("A10", "A11"),
        Iterable("A20", "A21"),
        Iterable("A30", "A31"),
        Iterable("A40", "A41")
      )
    }

    it("supports addition") {
      Iterable(1, 2) ++ Iterable(3) shouldBe Iterable(1, 2, 3)
      Iterable(1, 2).concat(Iterable(3)) shouldBe Iterable(1, 2, 3)
    }

    it("supports map operations") {
      xs.map(_ + 1) shouldBe Iterable(2, 3, 4)
      xs.flatMap(n => List.fill(n)(10 + n)) shouldBe Iterable(11, 12, 12, 13, 13, 13)
      xs.collect { case x if x % 2 == 0 => -x } shouldBe Iterable(-2)
    }

    it("supports conversion") {
      xs.to(LazyList) shouldBe a[LazyList[_]]
      xs.toArray shouldBe an[Array[_]]
      xs.toBuffer shouldBe an[mutable.Buffer[_]]
      xs.toIndexedSeq shouldBe an[IndexedSeq[_]]
      xs.toList shouldBe a[List[_]]
      xs.toSeq shouldBe a[Seq[_]]
      xs.toSet shouldBe a[Set[_]]
      xs.toVector shouldBe a[Vector[_]]
      // Static error if the Iterable is not a tuple.
      xs.map(x => (x, x + 1)).toMap shouldBe Map(1 -> 2, 2 -> 3, 3 -> 4)
    }

    it("supports copying") {
      val arr = Array.ofDim[Int](3)
      xs.copyToArray(arr, 0, 3)
      arr shouldBe Array(1, 2, 3)
      // If you want to copy into a buffer, do it the other way around
      val buf = mutable.ListBuffer[Int]() ++= xs
      buf shouldBe mutable.ListBuffer(1, 2, 3)
    }

    it("supports size info") {
      xs.isEmpty shouldBe false
      xs.nonEmpty shouldBe true
      xs.size shouldBe 3
      xs.knownSize shouldBe -1
      xs.toIndexedSeq.knownSize shouldBe 3
      // A comparator that can be a more efficient when calculating the
      // actual size is prohibitive.
      xs.sizeCompare(LazyList.continually(0)) shouldBe -1
      xs.sizeCompare(2) shouldBe 1
      xs.sizeIs >= 3 shouldBe true
    }

    it("supports element retrieval") {
      xs.head shouldBe 1
      xs.headOption shouldBe Some(1)
      xs.last shouldBe 3
      xs.lastOption shouldBe Some(3)
      xs find (_ % 2 == 0) shouldBe Some(2)
    }

    it("supports subcollection") {
      xs.tail shouldBe Iterable(2, 3)
      xs.init shouldBe Iterable(1, 2)
      xs.slice(0, 2) shouldBe Iterable(1, 2)
      xs.take(2) shouldBe Iterable(1, 2)
      xs.takeRight(2) shouldBe Iterable(2, 3)
      xs.drop(2) shouldBe Iterable(3)
      xs.dropRight(2) shouldBe Iterable(1)
      xs.takeWhile(_ <= 2) shouldBe Iterable(1, 2)
      xs.dropWhile(_ <= 2) shouldBe Iterable(3)
      xs.filter(_ % 2 == 0) shouldBe Iterable(2)
      xs.filterNot(_ % 2 == 0) shouldBe Iterable(1, 3)
      // Used to optimize for comprehensions (non-strict), provides further map operations without
      // actually running yet.
      xs.withFilter(_ % 2 == 0).map(identity) shouldBe Iterable(2)

      // When creating subcollections out of bounds
      xs.slice(0, 999) shouldBe Iterable(1, 2, 3)
      xs.take(999) shouldBe Iterable(1, 2, 3)
      xs.takeRight(999) shouldBe Iterable(1, 2, 3)
      xs.drop(999) shouldBe empty
      xs.dropRight(999) shouldBe empty
    }

    it("supports subdivisions") {
      // (xs take n, xs drop n)
      xs splitAt 1 shouldBe (Iterable(1), Iterable(2, 3))
      // (xs takeWhile n, xs dropWhile n)
      xs span (_ <= 2) shouldBe (Iterable(1, 2), Iterable(3))
      // (xs filter p, xs.filterNot p)
      xs partition (_ % 2 == 0) shouldBe (Iterable(2), Iterable(1, 3))
      // Arbitrary discrimator function for the key.
      xs groupBy (_ <= 2) shouldBe Map(false -> Iterable(3), true -> Iterable(1, 2))
      // As well as arbitrary function for the value.
      xs.groupMap(_ % 2)(_ + 100) shouldBe Map(0 -> Iterable(102), 1 -> Iterable(101, 103))
      // Or even to accumulate the values.
      xs.groupMapReduce(_ % 2)(_ + 100)(_ * _) shouldBe Map(0 -> 102, 1 -> 10403)
    }

    it("supports element conditions") {
      xs forall (_ % 2 == 0) shouldBe false
      xs exists (_ % 2 == 0) shouldBe true
      xs count (_ % 2 == 0) shouldBe 1
    }

    it("supports folds") {
      case class Acc(name: String)

      // Note that /:  :\ are deprecated symbols for foldLeft and foldRight respectively
      // Going left to right.
      val opL: (Acc, Int) => Acc = (acc, n) => Acc(acc.name + n)
      xs.foldLeft(Acc("fL"))(opL) shouldBe Acc("fL123")

      // Going right to left.
      val opR: (Int, Acc) => Acc = (n, acc) => Acc(acc.name + n)
      xs.foldRight(Acc("fR"))(opR) shouldBe Acc("fR321")

      // Without an accumulator.
      val op: (Int, Int) => Int = (a, b) => a + b * 2
      xs reduceLeft op shouldBe 11 // 1 + 2 * 2 is 5, then 5 + 3 * 3 is 11
      xs reduceRight op shouldBe 17 // 2 + 3 * 2 is 8, then 1 + 8 * 2 is 17

      // Scan returns intermediate values.  There's always one more value in the output.
      // Note that the first argument in the lambda is the accumulator type.
      xs.scanLeft("z")(_ + _) shouldBe Iterable("z", "z1", "z12", "z123")

      // Note that the LAST argument in the lambda is the accumulator type.
      xs.scanRight("z")((a, b) => s"$a$b") shouldBe Iterable("123z", "23z", "3z", "z")

      // UnsupportedOperationException on empty lists.
      intercept[UnsupportedOperationException] { Iterable().reduceLeft(op) }
    }

    it("supports specific numeric folds") {
      xs.sum shouldBe 6
      xs.product shouldBe 6
      xs.min shouldBe 1
      xs.max shouldBe 3

      // On an empty iterable
      val xs0 = xs.drop(3)
      xs0.sum shouldBe 0
      xs0.product shouldBe 1
      intercept[UnsupportedOperationException] { xs0.min }.getMessage shouldBe "empty.min"
      intercept[UnsupportedOperationException] { xs0.max }.getMessage shouldBe "empty.max"

      xs.minOption shouldBe Some(1)
      xs.maxOption shouldBe Some(3)
      xs0.minOption shouldBe None
      xs0.maxOption shouldBe None
    }

    it("supports some string operations") {
      val b = new StringBuilder()
      xs.addString(b, "a", "b", "c")
      b shouldBe new StringBuilder("a1b2b3c")
      xs.mkString("x", "y", "z") shouldBe "x1y2y3z"
    }

    it("supports grouped sub-iterators") {
      // Returns iterators taking the original collection in chunks.
      val it = xs.grouped(2)
      it.hasNext shouldBe true
      it.next() shouldBe Iterable(1, 2)
      it.hasNext shouldBe true
      it.next() shouldBe Iterable(3)
      it.hasNext shouldBe false
    }

    it("supports sliding sub-iterators") {
      // Returns iterators taking the original collection in chunks.
      val it = xs.sliding(2, 1)
      it.hasNext shouldBe true
      it.next() shouldBe Iterable(1, 2)
      it.hasNext shouldBe true
      it.next() shouldBe Iterable(2, 3)
      it.hasNext shouldBe false
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
      xs.zipAll(ys2, 99, "z") shouldBe Iterable((1, "a"), (2, "b"), (3, "c"), (99, "d"))

      // Iterate with it's own index (the second value)
      xs.zipWithIndex shouldBe Iterable((1, 0), (2, 1), (3, 2))
    }

    it("supports comparison") {
      val ys1 = Iterable("a", "b")
      xs.iterator sameElements ys1.iterator shouldBe false
    }

    it("supports views") {
      // See spec on views in this package. TL;DR --> they're lazy.
      xs.view should contain theSameElementsAs List(1, 2, 3)
      xs.view.slice(1, 2) should contain theSameElementsAs List(2)
    }
  }
}
