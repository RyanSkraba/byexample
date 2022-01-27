package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.{SortedSet, immutable, mutable}

/** Examples from the scala collections doc. Each spec covers a page.
  *
  * @see
  *   https://docs.scala-lang.org/overviews/collections/introduction.html
  */
class Collections040SetSpec extends AnyFunSpecLike with Matchers {

  describe("Set") {

    // Instance for testing.
    val xs = Set(1, 2, 3)

    it("has a default implementation of immutable.HashSet for larger sets") {
      // Any smaller sets may have a specialized implementation.
      Set(1, 2, 3, 4, 5) shouldBe a[immutable.HashSet[_]]

      // Of course they aren't necessarily in order... we don't know if the following is true.
      // xs.takeRight(1) shouldBe Set(1))

      // Because of the apply method, a set can be used as a predicate.
      List(2, 3, 4).filter(xs) shouldBe List(2, 3)

      // And a composable function (Is the input string size in the set?)
      val f: String => Boolean = xs.compose((n: String) => n.length)
      List("one", "two", "three").map(f) shouldBe List(true, true, false)
    }

    it("supports tests") {
      xs contains 3 shouldBe true
      xs(3) shouldBe true // alias
      Set(1, 2) subsetOf xs shouldBe true
    }

    it("supports additions") {
      xs + 3 shouldBe xs // Already exists.
      xs + 4 shouldBe Set(1, 2, 3, 4)
      xs + (3, 4) shouldBe Set(1, 2, 3, 4)
      xs ++ Set(4) shouldBe Set(1, 2, 3, 4)
    }

    it("supports removals") {
      xs - 3 shouldBe Set(1, 2)
      xs - (2, 3) shouldBe Set(1)
      xs -- Set(2, 3) shouldBe Set(1)
      // Not very useful, but at least the same type.
      xs.empty shouldBe Set[Int]()
    }

    it("supports binary operations") {
      val ys = Set(2, 3, 4)
      xs & ys shouldBe Set(2, 3)
      xs intersect ys shouldBe Set(2, 3) // alias
      xs | ys shouldBe Set(1, 2, 3, 4)
      xs union ys shouldBe Set(1, 2, 3, 4) // alias
      xs &~ ys shouldBe Set(1)
      xs diff ys shouldBe Set(1) // alias
    }
  }

  describe("Mutable sets") {

    it("support additions") {
      val xs = mutable.Set(1, 2, 3)
      (xs += 4) shouldBe Set(1, 2, 3, 4)
      (xs += (4, 5, 6)) shouldBe Set(1, 2, 3, 4, 5, 6)
      (xs ++= Set(6, 7, 8)) shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8)
      xs add 9 shouldBe true // whether it was actually added
      xs shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      xs add 9 shouldBe false
      xs shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
    }

    it("supports removals") {
      val xs = mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      (xs -= 9) shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8)
      (xs -= (7, 8, 9)) shouldBe Set(1, 2, 3, 4, 5, 6)
      (xs --= Set(5, 6, 7)) shouldBe Set(1, 2, 3, 4)
      (xs remove 4) shouldBe true // Whether it was actually removed.
      xs shouldBe Set(1, 2, 3)
      (xs remove 4) shouldBe false // Whether it was actually removed.
      xs shouldBe Set(1, 2, 3)
      xs retain (_ % 2 == 0)
      xs shouldBe Set(2)
      xs.clear()
      xs shouldBe Set()
    }

    it("supports updates") {
      val xs = mutable.Set(1, 2, 3)
      xs(4) = true
      xs shouldBe Set(1, 2, 3, 4)
      xs(1) = false
      xs shouldBe Set(2, 3, 4)
      xs.update(5, included = true)
      xs shouldBe Set(2, 3, 4, 5)

      (xs.clone() + 1) shouldBe Set(1, 2, 3, 4, 5)
      xs shouldBe Set(2, 3, 4, 5)
    }
  }

  describe("Sorted sets") {
    it("has a default implementation of immutable.TreeSet") {
      // Any smaller sets may have a specialized implementation.
      val xs = SortedSet(1, 2, 3, 4, 5)
      xs shouldBe a[immutable.TreeSet[_]]
      // And it has an order
      xs.toSeq shouldBe Seq(1, 2, 3, 4, 5)
    }

    it("supports ordering") {
      // Use the greater than operation for less than reverses the order.
      val myOrdering = Ordering.fromLessThan[Int](_ > _)
      val xs = SortedSet.empty(myOrdering)
      (xs + (2, 1, 3)).toSeq shouldBe Seq(3, 2, 1)
    }

    it("supports ranges") {
      val xs = SortedSet(2, 4, 5, 3, 1)
      xs.toSeq shouldBe Seq(1, 2, 3, 4, 5)
      // inclusive start, exclusive end
      xs.range(2, 4).toSeq shouldBe Seq(2, 3)
      // inclusive
      xs.from(3).toSeq shouldBe Seq(3, 4, 5)
      xs.to(4).toSeq shouldBe Seq(1, 2, 3, 4)
    }
  }

  describe("Bit sets") {
    it("has a default implementation of immutable.BitSet") {
      // Can only contain ints, and is stored internally as sequences of Long
      val xs = immutable.BitSet(1, 2, 5, 100, 200)
      xs shouldBe a[immutable.BitSet]
      // But supports all SortedSet operations.
      xs(1) shouldBe true
      xs(3) shouldBe false
      xs.toSeq shouldBe Seq(1, 2, 5, 100, 200)
      xs.range(2, 200).toSeq shouldBe Seq(2, 5, 100)
    }
  }
}
