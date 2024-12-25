package com.skraba.byexample.scala.collections

import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers

import scala.collection.{SortedSet, immutable, mutable}

/** Examples from the scala collections doc. Each spec covers a page.
  *
  * @see
  *   [[https://docs.scala-lang.org/overviews/collections-2.13/introduction.html]]
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

    it("has aliases") {
      xs + 4 shouldBe Set(1, 2, 3, 4)
      xs ++ Iterable(3, 4) shouldBe Set(1, 2, 3, 4)
      xs - 3 shouldBe Set(1, 2)
      xs -- Iterable(3, 4) shouldBe Set(1, 2)
      xs | Set(3, 4) shouldBe Set(1, 2, 3, 4) // union
      xs & Set(3, 4) shouldBe Set(3) // intersect
      xs &~ Set(3, 4) shouldBe Set(1, 2) // diff
    }

    it("supports tests") {
      xs contains 3 shouldBe true
      xs(3) shouldBe true // alias for apply
      Seq(1, 2, 5, 6, 1, 4, 3, 99, 1).count(xs) shouldBe 5
      Set(1, 2) subsetOf xs shouldBe true
    }

    it("supports additions") {
      xs + 3 shouldBe xs // Already exists.
      xs + 4 shouldBe Set(1, 2, 3, 4)
      xs incl 4 shouldBe Set(1, 2, 3, 4)
      xs ++ Set(4) shouldBe Set(1, 2, 3, 4)
      xs ++ Seq(3, 6) shouldBe Set(1, 2, 3, 6)
      xs concat Iterable(3, 4) shouldBe Set(1, 2, 3, 4)
      xs union Set(3, 4) shouldBe Set(1, 2, 3, 4)
    }

    it("supports removals") {
      xs - 3 shouldBe Set(1, 2)
      xs excl 3 shouldBe Set(1, 2)
      xs -- Set(2, 3) shouldBe Set(1)
      xs removedAll Set(2, 3) shouldBe Set(1)
      xs removedAll Iterable(2, 3) shouldBe Set(1)
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

    it("supports subset enumeration") {
      xs subsetOf Set(1, 2, 3, 4) shouldBe true
      xs subsetOf Set(2, 3, 4) shouldBe false
      xs.subsets(2).toSet shouldBe Set(Set(1, 2), Set(1, 3), Set(2, 3))
      xs.subsets().toSet shouldBe Set(Set(), Set(1), Set(2), Set(3), Set(1, 2), Set(1, 3), Set(2, 3), Set(1, 2, 3))
    }
  }

  describe("Mutable sets") {

    it("has aliases") {
      val xs = mutable.Set(1, 2, 3)
      // These operations are the same as the immutable set and don't modify the original.
      xs ++ Iterable(3, 4) shouldBe Set(1, 2, 3, 4)
      xs | Set(3, 4) shouldBe Set(1, 2, 3, 4) // union
      xs & Set(3, 4) shouldBe Set(3) // intersect
      xs &~ Set(3, 4) shouldBe Set(1, 2) // diff
      xs shouldBe Set(1, 2, 3)
      // These aliases modify the original
      (xs += 4) shouldBe Set(1, 2, 3, 4)
      (xs ++= Set(4, 5, 6)) shouldBe Set(1, 2, 3, 4, 5, 6)
      (xs -= 6) shouldBe Set(1, 2, 3, 4, 5)
      (xs --= Iterable(0, 1, 2)) shouldBe Set(3, 4, 5)
    }

    it("support additions") {
      val xs = mutable.Set(1, 2, 3)
      (xs += 4) shouldBe Set(1, 2, 3, 4)
      (xs addOne 5) shouldBe Set(1, 2, 3, 4, 5)
      (xs ++= Set(4, 5, 6)) shouldBe Set(1, 2, 3, 4, 5, 6)
      (xs addAll Iterable(0, 1, 2)) shouldBe Set(0, 1, 2, 3, 4, 5, 6)
      xs add 9 shouldBe true // whether it was actually added
      xs shouldBe Set(0, 1, 2, 3, 4, 5, 6, 9)
      xs add 9 shouldBe false
      xs shouldBe Set(0, 1, 2, 3, 4, 5, 6, 9)
    }

    it("supports removals") {
      val xs = mutable.Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      (xs -= 9) shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8)
      (xs subtractOne 9) shouldBe Set(1, 2, 3, 4, 5, 6, 7, 8)
      (xs --= Iterable(7, 8, 9)) shouldBe Set(1, 2, 3, 4, 5, 6)
      (xs subtractAll Set(5, 6, 7)) shouldBe Set(1, 2, 3, 4)
      (xs remove 4) shouldBe true // Whether it was actually removed.
      xs shouldBe Set(1, 2, 3)
      (xs remove 4) shouldBe false // Whether it was actually removed.
      xs shouldBe Set(1, 2, 3)
      xs filterInPlace (_ % 2 == 0) shouldBe Set(2)
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
      xs.update(3, included = false)
      xs shouldBe Set(2, 4, 5)

      (xs.clone() addOne 1) shouldBe Set(1, 2, 4, 5)
      xs shouldBe Set(2, 4, 5)
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

    it("has aliases") {
      val xs = SortedSet(1, 2, 3)
      xs ++ Iterable(3, 4) shouldBe Set(1, 2, 3, 4)
      xs | Set(3, 4) shouldBe Set(1, 2, 3, 4) // union
      xs & Set(3, 4) shouldBe Set(3) // intersect
      xs &~ Set(3, 4) shouldBe Set(1, 2) // diff
    }

    it("can be created from an existing set") {
      val xs = Set('z', 'a', 'b', '@', 'A')
      val sortedXs = SortedSet.empty[Char] ++ xs
      sortedXs shouldBe a[immutable.TreeSet[_]]
      sortedXs.toSeq shouldBe Seq('@', 'A', 'a', 'b', 'z')
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
      xs.rangeFrom(3).toSeq shouldBe Seq(3, 4, 5)
      xs.rangeTo(4).toSeq shouldBe Seq(1, 2, 3, 4)
      xs.rangeUntil(4).toSeq shouldBe Seq(1, 2, 3)
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

    it("has aliases") {
      val xs = immutable.BitSet(1, 2, 3)
      xs + 4 shouldBe Set(1, 2, 3, 4)
      xs ++ Iterable(3, 4) shouldBe Set(1, 2, 3, 4)
      xs - 3 shouldBe Set(1, 2)
      xs -- Iterable(3, 4) shouldBe Set(1, 2)
      xs | Set(3, 4) shouldBe Set(1, 2, 3, 4) // union
      xs & Set(3, 4) shouldBe Set(3) // intersect
      xs &~ Set(3, 4) shouldBe Set(1, 2) // diff
    }
  }
}
