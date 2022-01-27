package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.mutable.ListBuffer

/** Examples from the scala collections doc. Each spec covers a page.
  *
  * Traversable is the root trait for collections.
  *
  * @see
  *   https://docs.scala-lang.org/overviews/collections/introduction.html
  */
class Collections010TraversableSpec extends AnyFunSpecLike with Matchers {

  describe("Traversable") {

    // Instance for testing.
    val xs = Traversable(1, 2, 3)

    it("has a default implementation of immutable.List") {
      xs shouldBe a[List[_]]

      // If you want to define a new Traversable, all you need to implement is foreach.
      xs.foreach {
        _ should (be > 0 and be <= 3)
      }
    }

    it("supports addition") {
      Traversable(1, 2) ++ Traversable(3) shouldBe Traversable(1, 2, 3)
    }

    it("supports map operations") {
      xs.map(_ + 1) shouldBe Traversable(2, 3, 4)
      xs.flatMap(n => List.fill(n)(10 + n)) shouldBe Traversable(11, 12, 12, 13,
        13, 13)
      xs.collect { case x if x % 2 == 0 => -x } shouldBe Traversable(-2)
    }

    it("supports conversion") {
      xs.toArray shouldBe an[Array[_]]
      xs.toList shouldBe a[List[_]]
      xs.toIterable shouldBe an[Iterable[_]]
      xs.toSeq shouldBe a[Seq[_]]
      xs.toIndexedSeq shouldBe an[IndexedSeq[_]]
      xs.toStream shouldBe a[Stream[_]]
      xs.toSet shouldBe a[Set[_]]
      // Static error if the Traversable is not a tuple.
      xs.map(x => (x, x + 1)).toMap shouldBe Map(1 -> 2, 2 -> 3, 3 -> 4)
    }

    it("supports copying") {
      val buf = ListBuffer[Int]()
      xs.copyToBuffer(buf)
      buf shouldBe ListBuffer(1, 2, 3)
      val arr = Array.ofDim[Int](3)
      xs.copyToArray(arr, 0, 3)
      arr shouldBe Array(1, 2, 3)
    }

    it("supports size info") {
      xs.isEmpty shouldBe false
      xs.nonEmpty shouldBe true
      xs.size shouldBe 3
      xs.hasDefiniteSize shouldBe true
    }

    it("supports element retrieval") {
      xs.head shouldBe 1
      xs.headOption shouldBe Some(1)
      xs.last shouldBe 3
      xs.lastOption shouldBe Some(3)
      xs find (_ % 2 == 0) shouldBe Some(2)
    }

    it("supports subcollections") {
      xs.tail shouldBe Traversable(2, 3)
      xs.init shouldBe Traversable(1, 2)
      xs.slice(0, 2) shouldBe Traversable(1, 2)
      xs.take(2) shouldBe Traversable(1, 2)
      xs.drop(2) shouldBe Traversable(3)
      xs.takeWhile(_ <= 2) shouldBe Traversable(1, 2)
      xs.dropWhile(_ <= 2) shouldBe Traversable(3)
      xs.filter(_ % 2 == 0) shouldBe Traversable(2)
      // Used to optimize for comprehensions (non-strict), provides further map operations without
      // actually running yet.
      xs.withFilter(_ % 2 == 0).map(x => x) shouldBe Traversable(2)
      xs.filterNot(_ % 2 == 0) shouldBe Traversable(1, 3)
    }

    it("supports subdivisions") {
      // (xs take n, xs drop n)
      xs splitAt 1 shouldBe (Traversable(1), Traversable(2, 3))
      // (xs takeWhile n, xs dropWhile n)
      xs span (_ <= 2) shouldBe (Traversable(1, 2), Traversable(3))
      // (xs filter p, xs.filterNot p)
      xs partition (_ % 2 == 0) shouldBe (Traversable(2), Traversable(1, 3))
      // Arbitrary discrimator function.
      xs groupBy (_ <= 2) shouldBe Map(
        false -> Traversable(3),
        true -> Traversable(1, 2)
      )
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
      xs.scanLeft("z")(_ + _) shouldBe Traversable("z", "z1", "z12", "z123")

      // Note that the LAST argument in the lambda is the accumulator type.
      xs.scanRight("z")(_ + _) shouldBe Traversable("123z", "23z", "3z", "z")

      // UnsupportedOperationException on empty lists.
      intercept[UnsupportedOperationException] {
        Traversable().reduceLeft(op)
      }
    }

    it("supports specific folds") {
      xs.sum shouldBe 6
      xs.product shouldBe 6
      xs.min shouldBe 1
      xs.max shouldBe 3
    }

    it("supports some string operations") {
      val b = new StringBuilder()
      xs.addString(b, "a", "b", "c") shouldBe new StringBuilder("a1b2b3c")
      xs.mkString("x", "y", "z") shouldBe "x1y2y3z"
      xs.stringPrefix shouldBe "List"
    }

    it("supports views") {
      // See spec on views in this package. TL;DR --> they're lazy.
      xs.view shouldBe Traversable(1, 2, 3)
      xs view (1, 2) shouldBe Traversable(2)
    }

    it("supports creation") {
      // One dimensional
      Traversable.fill(5)("A") shouldBe Traversable("A", "A", "A", "A", "A")
      Traversable.tabulate(5)(x => s"A$x") shouldBe Traversable(
        "A0",
        "A1",
        "A2",
        "A3",
        "A4"
      )

      // Up to five dimensions are possible.
      Traversable.fill(5, 2)("A") shouldBe Traversable(
        Traversable("A", "A"),
        Traversable("A", "A"),
        Traversable("A", "A"),
        Traversable("A", "A"),
        Traversable("A", "A")
      )
      Traversable.tabulate(5, 2)((x, y) => s"A$x$y") shouldBe Traversable(
        Traversable("A00", "A01"),
        Traversable("A10", "A11"),
        Traversable("A20", "A21"),
        Traversable("A30", "A31"),
        Traversable("A40", "A41")
      )
    }
  }
}
